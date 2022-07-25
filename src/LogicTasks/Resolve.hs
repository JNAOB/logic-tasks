{-# language RecordWildCards #-}

module LogicTasks.Resolve where




import Config (ResolutionConfig(..), ResolutionInst(..), BaseConfig(..))
import Printing
import Types
import Formula
import Util
import Resolution

import qualified Data.Set as Set
import Data.List (sort)
import Data.Maybe (fromMaybe, fromJust, isJust)


import Control.Monad.Output (
  LangM,
  OutputMonad (..),
  english,
  german,
  translate
  )




fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b


thrd3 :: (a,b,c) -> c
thrd3 (_,_,c) = c



description :: OutputMonad m => ResolutionInst -> LangM m
description ResolutionInst{..} = do
  paragraph $ translate $ do
    german "Betrachten Sie die folgende Formel in KNF:"
    english "Consider the following formula in cnf:"

  -- PDoc $ nest 4 $ pretty $ mkCnf clauses

  paragraph $ translate $ do
    german "Führen Sie das Resolutionsverfahren an dieser Formel durch, um die leere Klausel abzuleiten."
    english "Use the resolution technique on this formula to derive the empty clause."

  paragraph $ translate $ do
    german "Geben Sie die Lösung als eine Liste von Tripeln an, wobei diese folgendermaßen aufgebaut sind: (Erste Klausel, Zweite Klausel, Resolvente)"
    english "Provide the solution as a list of triples with this structure: (first clause, second clause, resolvent)."

  paragraph $ translate $ do
    german "Beachten Sie dabei für die ASCII-Formel diese Legende:"
    english "Consider this key for the ASCII based formula:"

  paragraph $ do
    translate $ do
      german "Negation"
      english "negation"
    text ": ~"

  paragraph $ do
    translate $ do
      german "oder"
      english "or"
    text ": \\/"

  paragraph $ do
    translate $ do
      german "leere Klausel"
      english "empty clause"
    text ": { }"

  paragraph $ translate $ do
    german "Optional können Sie Klauseln auch durch Nummern substituieren."
    english "You can optionally substitute clauses with numbers."

  paragraph $ translate $ do
    german "Klauseln aus der Formel sind bereits ihrer Reihenfolge nach nummeriert. (erste Klausel = 1, zweite Klausel = 2, ...)"
    english "Clauses in the starting formula are already numbered by their order. (first clause = 1, second clause = 2, ...)"

  paragraph $ translate $ do
    german "neu resolvierte Klauseln können mit einer Nummer versehen werden, indem Sie '= NUMMER' an diese anfügen."
    english "Newly resolved clauses can be associated with a number by attaching '= NUMBER' behind them."

  paragraph $ do
    translate $ do
      german "Ein Lösungsversuch könnte beispielsweise so aussehen: "
      english "A valid solution could look like this: "
    text "[(1, 2, {A, ~B} = 5), (4, 5, { })]"

  paragraph $ text (fromMaybe "" addText)



verifyStatic :: OutputMonad m => ResolutionInst -> Maybe (LangM m)
verifyStatic ResolutionInst{..}
    | any isEmptyClause clauses =
        Just $ translate $ do
          german "Mindestens eine der Klauseln ist leer."
          english "At least one of the clauses is empty."

    | sat $ mkCnf clauses =
        Just $ translate $ do
          german "Die Formel ist erfüllbar."
          english "This formula is satisfiable."

    | otherwise = Nothing



verifyQuiz :: OutputMonad m => ResolutionConfig -> Maybe (LangM m)
verifyQuiz ResolutionConfig{..}
    | minSteps < 1 =
        Just $ translate $ do
          german "Die Mindestschritte müssen größer als 0 sein."
          english "The minimal amount of steps must be greater than 0."

    | maxClauseLength baseConf == 1 && minSteps > 1 =
        Just $ translate $ do
          german "Mit Klauseln der Länge 1 kann nicht mehr als ein Schritt durchgeführt werden."
          english "More than one step using only length 1 clauses is not possible."

    | minSteps > 2 * length (usedLiterals baseConf) =
        Just $ translate $ do
          german "Diese minimale Schrittzahl kann mit den gegebenen Literalen nicht durchgeführt werden."
          english "This amount of steps is impossible with the given amount of literals."

    | otherwise = checkBaseConf baseConf



start :: [ResStep]
start = []



partialGrade :: OutputMonad m => ResolutionInst -> [ResStep] -> Maybe (LangM m)
partialGrade ResolutionInst{..} sol
    | isJust checkMapping  = checkMapping

    | not (null wrongLitsSteps) =
        Just $ paragraph $ do
          translate $ do
            german "Mindestens ein Schritt beinhaltet Literale, die in der Formel nicht vorkommen. "
            english "At least one step contains literals not found in the original formula. "
          itemizeM $ map (text . show) wrongLitsSteps

    | not (null noResolveSteps) =
        Just $ paragraph $ do
          translate $ do
            german "Mindestens ein Schritt ist kein gültiger Resolutionsschritt. "
            english "At least one step is not a valid resolution step. "
          itemizeM $ map (text . show) noResolveSteps

    | checkEmptyClause =
        Just $ translate $ do
          german "Im letzten Schritt muss die leere Klausel abgeleitet werden."
          english "The last step must derive the empty clause."

    | otherwise = Nothing

  where
    checkMapping = correctMapping sol $ baseMapping clauses
    steps =  replaceAll sol $ baseMapping clauses
    checkEmptyClause = null steps || not (isEmptyClause $ thrd3 $ last steps)
    availLits = Set.unions (map (Set.fromList . literals) clauses)
    stepLits (c1,c2,r) = Set.toList $ Set.unions $ map (Set.fromList . literals) [c1,c2,r]
    wrongLitsSteps = filter (not . all (`Set.member` availLits) . stepLits) steps
    noResolveSteps = filter (\(c1,c2,r) -> maybe True (\x -> fromJust (resolve c1 c2 x) /= r) (resolvableWith c1 c2)) steps



completeGrade :: OutputMonad m => ResolutionInst -> [ResStep] -> Maybe (LangM m)
completeGrade ResolutionInst{..} sol =
    case applySteps clauses steps of
        Nothing -> Just $ translate $ do
                     german "In mindestens einem Schritt werden Klauseln resolviert, die nicht in der Formel sind oder noch nicht abgeleitet wurden."
                     english "In at least one step clauses are used, that are not part of the original formula and are not derived from previous steps."

        Just solClauses -> if (any isEmptyClause solClauses)
                            then Nothing
                            else Just $ translate $ do
                                   german "Die Leere Klausel wurde nicht korrekt abgeleitet."
                                   english "The Empty clause was not derived correctly."

      where
        steps = replaceAll sol $ baseMapping clauses



baseMapping :: [Clause] -> [(Int,Clause)]
baseMapping xs = zip [1..] $ sort xs


correctMapping :: OutputMonad m => [ResStep] -> [(Int,Clause)] -> Maybe (LangM m)
correctMapping [] _ = Nothing
correctMapping (Res (c1,c2,(c3,i)): rest) mapping
    | checkIndices = Just $ translate $ do
                       german "Mindestens ein Schritt verwendet einen nicht vergebenen Index. "
                       english "At least one step is using an unknown index."

    | alreadyUsed i = Just $ translate $ do
                        german "Mindestens ein Schritt vergibt einen Index, welcher bereits verwendet wird. "
                        english "At least one step assigns an index, which is already in use. "

    | otherwise = correctMapping rest newMapping


  where
    newMapping = case i of Nothing      -> mapping
                           (Just index) -> (index,c3) : mapping


    unknown (Left _) = False
    unknown (Right n) = n `notElem` (map fst mapping)

    checkIndices = unknown c1 || unknown c2

    alreadyUsed Nothing = False
    alreadyUsed (Just n) = n `elem` (map fst mapping)



replaceAll :: [ResStep] -> [(Int,Clause)] -> [(Clause,Clause,Clause)]
replaceAll [] _ = []
replaceAll (Res (c1,c2,(c3,i)) : rest) mapping = (replaceNum c1, replaceNum c2, c3) : replaceAll rest newMapping
  where
    newMapping = case i of Nothing      -> mapping
                           (Just index) -> (index,c3) : mapping

    replaceNum (Left c) = c
    replaceNum (Right n) = case lookup n mapping of Nothing  -> error "no mapping"
                                                    (Just c) -> c
