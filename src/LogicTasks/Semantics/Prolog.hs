{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}

module LogicTasks.Semantics.Prolog where


import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  english,
  german,
  translate,
  localise,
  translations,
  )
import Data.Maybe (fromJust)
import Data.Set (difference, member, toList, union)
import Data.Tuple (swap)
import Test.QuickCheck (Gen, suchThat)

import Config (PrologConfig(..), PrologInst(..))
import Formula.Types (Clause, Literal(..), PrologLiteral(..), PrologClause(..), literals, opposite, ClauseShape (HornClause), HornShape (Fact, Query), terms)
import Formula.Util (flipPol, isEmptyClause, isPositive, mkPrologClause, transformProlog)
import Formula.Resolution (resolvable, resolve)
import LogicTasks.Semantics.Step (genResStepClause)
import Util(prevent, preventWithHint)
import Control.Monad (when)
import LogicTasks.Helpers (example, extra)
import Formula.Helpers (hasTheClauseShape)
import Formula.Parsing.Delayed (Delayed, withDelayed, withDelayedSucceeding, complainAboutWrongNotation)
import Formula.Parsing (Parse(..), prologClauseSetParser)
import Data.List (intercalate)
import ParsingHelpers (tokenSymbol)
import Text.ParserCombinators.Parsec (Parser)

genPrologInst :: PrologConfig -> Gen PrologInst
genPrologInst PrologConfig{..} = (do
    (clause, resolveLit, literals1) <- genResStepClause minClauseLength maxClauseLength usedAtoms
    let
      termAddedClause1 = mkPrologClause $ map remap (resolveLit : literals1)
      termAddedClause2 = mkPrologClause $ map remap (opposite resolveLit : literals clause)
      resultClause = literals1 ++ filter (`notElem` literals1) (literals clause)

    pure $ PrologInst {
      literals1 = termAddedClause1
    , literals2 = termAddedClause2
    , solution = (remap resolveLit, mkPrologClause (map remap resultClause))
    , usesSetNotation = useSetNotation
    , showSolution = printSolution
    , addText = extraText
    })
  `suchThat` \(PrologInst clause1 clause2 _ _ _ _) -> hasTheClauseShape firstClauseShape clause1 && hasTheClauseShape secondClauseShape clause2
  where
    mapping = zip usedPredicates ['A'..'Z']
    usedAtoms = map snd mapping
    reverseMapping = map swap mapping
    remap l = if isPositive l then predicate else flipPol predicate
      where
        predicate = fromJust (lookup (letter l) reverseMapping)

showPrologClauseAsSet :: PrologClause -> String
showPrologClauseAsSet pc
  | null lits = "{ }"
  | otherwise = "{" ++ intercalate "," (map show lits) ++ "}"
  where lits = terms pc

description :: OutputCapable m => PrologInst -> LangM m
description PrologInst{..} = do
  paragraph $ do
    translate $ do
      german "Betrachten Sie die zwei folgenden Klauseln:"
      english "Consider the two following clauses:"
    indent $ code $ displayClause literals1
    indent $ code $ displayClause literals2
    pure ()
  paragraph $ translate $ do
    german "Resolvieren Sie die Klauseln und geben Sie die Resolvente an."
    english "Resolve the clauses and give the resulting resolvent."

  paragraph $ translate $ do
    german "Geben Sie das in dem Resolutionsschritt genutzte Literal (in positiver oder negativer Form) und das Ergebnis in der folgenden Tupelform an: (Literal, Resolvente)."
    english "Provide the literal (in positive or negative form) used for the step and the resolvent in the following tuple form: (literal, resolvent)."

  paragraph $ if usesSetNotation
    then do
      translate $ do
        german "Nutzen Sie zur Angabe der Klauseln die Mengenschreibweise. Ein Lösungsversuch mit den Klauseln {a(x), b(x)} und {-a(x), b(x), c(x)} könnte beispielsweise so aussehen:"
        english "Specify the clauses using set notation. A valid solution with the clauses {a(x), b(x)} and {-a(x), b(x), c(x)} could look like this:"
      indent $ code "(a(x), { b(x), c(x) })"
      pure ()
    else do
      translate $ do
        german "Nutzen Sie zur Angabe der Klauseln eine Formel. Ein Lösungsversuch mit den Klauseln 'a(x) oder b(x)' und '-a(x) oder b(x) oder c(x)' könnte beispielsweise so aussehen:"
        english "Specify the clauses using a formula. A valid solution with the clauses 'a(x) or b(x)' and '-a(x) or b(x) or c(x)' could look like this:"
      indent $ translatedCode $ flip localise $ translations $ do
        english "(a(x), b(x) or c(x))"
        german "(a(x), b(x) oder c(x))"
      pure ()

  paragraph $ translate $ do
    german "Die leere Klausel kann durch geschweifte Klammern '{ }' dargestellt werden."
    english "The empty clause can be denoted by curly braces '{ }'."

  extra addText
  pure ()
  where displayClause = if usesSetNotation then showPrologClauseAsSet else show

verifyStatic :: OutputCapable m => PrologInst -> LangM m
verifyStatic PrologInst{..}
    | any isEmptyClause [clause1, clause2] =
        refuse $ indent $ translate $ do
          german "Mindestens eine der Klauseln ist leer."
          english "At least one of the clauses is empty."

    | not $ resolvable clause1 clause2 =
        refuse $ indent $ translate $ do
          german "Die Klauseln sind nicht resolvierbar."
          english "The clauses are not resolvable."

    | otherwise = pure()
  where
    (clause1, clause2, _) = transform (literals1, literals2)



verifyQuiz :: OutputCapable m => PrologConfig -> LangM m
verifyQuiz PrologConfig{..}
    | any (<1) [minClauseLength, maxClauseLength] =
        refuse $ indent $ translate $ do
          german "Mindestens eines der 'length'-Parameter ist negativ."
          english "At least one length parameter is negative."

    | minClauseLength > maxClauseLength =
        refuse $ indent $ translate $ do
          german "Die untere Grenze der Klausellänge ist höher als die obere."
          english "The minimum clause length is greater than the maximum clause length."

    | length usedPredicates < minClauseLength =
        refuse $ indent $ translate $ do
          german "Zu wenige Literale für diese Klausellänge."
          english "There are not enough literals available for this clause length."

    | null usedPredicates =
        refuse $ indent $ translate $ do
          german "Es wurden keine Literale angegeben."
          english "You did not specify which literals should be used."

    | (firstClauseShape `elem` [HornClause Fact, HornClause Query]) && firstClauseShape == secondClauseShape =
        refuse $ indent $ translate $ do
          german "Mit diesen Klauselformen ist keine Resolution möglich."
          english "No resolution is possible with these clause forms."

    | otherwise = pure()



start :: (PrologLiteral, PrologClause)
start = (PrologLiteral True "a" ["x"], mkPrologClause [])

parser' :: Parser PrologClause -> Parser (PrologLiteral, PrologClause)
parser' clauseParser = do
  tokenSymbol "("
  lit <- parser
  tokenSymbol ","
  clause <- clauseParser
  tokenSymbol ")"
  pure (lit, clause)

partialGrade :: OutputCapable m => PrologInst -> Delayed (PrologLiteral, PrologClause) -> LangM m
partialGrade inst = (partialGrade' inst `withDelayed` parser' clauseParser) (const complainAboutWrongNotation)
  where clauseParser | usesSetNotation inst = prologClauseSetParser
                     | otherwise            = parser


partialGrade' :: OutputCapable m => PrologInst -> (PrologLiteral, PrologClause) -> LangM m
partialGrade' PrologInst{..} sol = do
  prevent (not (fst sol `member` availLits)) $
    translate $ do
      german "Gewähltes Literal kommt in den Klauseln vor?"
      english "Chosen literal is contained in any of the clauses?"

  preventWithHint (not $ null extraLiterals)
    (translate $ do
       german "Resolvente besteht aus bekannten Literalen?"
       english "Resolvent contains only known literals?"
    )
    (paragraph $ do
      translate $ do
        german "In der Resolvente sind unbekannte Literale enthalten. Diese Literale sind falsch: "
        english "The resolvent contains unknown literals. These are incorrect:"
      itemizeM $ map (text . show) extraLiterals
      pure ()
    )
  pure ()
  where
     availLits = pLiterals literals1 `union` pLiterals literals2
     solLits = pLiterals $ snd sol
     extraLiterals = toList $ solLits `difference` availLits

completeGrade :: OutputCapable m => PrologInst -> Delayed (PrologLiteral, PrologClause) -> LangM m
completeGrade inst = completeGrade' inst `withDelayedSucceeding` parser' clauseParser
  where clauseParser | usesSetNotation inst = prologClauseSetParser
                     | otherwise            = parser

completeGrade' :: OutputCapable m => PrologInst -> (PrologLiteral, PrologClause) -> LangM m
completeGrade' PrologInst{..} sol =
    case resolveResult of
        Nothing -> refuse $ indent $  do
          translate $ do
            german "Mit diesem Literal kann kein Schritt durchgeführt werden!"
            english "This literal cannot be used for a resolution step!"

          displaySolution

          pure ()

        Just solClause -> if solClause == transSol2
                            then pure ()
                            else refuse $ indent $ do
                                    translate $ do
                                      german "Resolvente ist nicht korrekt."
                                      english "Resolvent is not correct."

                                    displaySolution

                                    pure ()
  where
    (clause1, clause2, mapping) = transform (literals1, literals2)
    transSol1 = fromJust $ lookup (fst sol) mapping
    transSol2 = transformProlog (snd sol) mapping
    resolveResult = resolve clause1 clause2 transSol1
    displayClause = if usesSetNotation then showPrologClauseAsSet else show
    displaySolution = when showSolution $ do
          example ("(" ++ show (fst solution) ++ ", " ++ displayClause (snd solution) ++ ")") $ do
            english "A possible solution for this task is:"
            german "Eine mögliche Lösung für die Aufgabe ist:"
          pure ()



transform :: (PrologClause,PrologClause) -> (Clause,Clause,[(PrologLiteral,Literal)])
transform (pc1,pc2) = (clause1, clause2, applyPol)
  where
    allPredicates = toList (pLiterals pc1 `union` pLiterals pc2)
    noDups = map (\(PrologLiteral _ n f) -> PrologLiteral True n f) allPredicates
    mapping = zip noDups ['A'..'Z']

    polLookup p = (p,lit)
      where lit = case lookup p mapping of
                    Just l1  -> Positive l1
                    Nothing  -> case lookup (flipPol p) mapping of
                                  Just l2 -> Negative l2
                                  Nothing -> error "each literal should have a mapping."

    applyPol = map polLookup allPredicates
    clause1 = transformProlog pc1 applyPol
    clause2 = transformProlog pc2 applyPol



revertMapping :: [Literal] -> [(PrologLiteral,Literal)] -> [PrologLiteral]
revertMapping ls mapping = map fromJust getPredicates
  where
    reverseM = map swap mapping
    getPredicates = map (`lookup` reverseM) ls
