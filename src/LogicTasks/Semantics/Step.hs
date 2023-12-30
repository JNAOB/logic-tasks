{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}

module LogicTasks.Semantics.Step where


import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  OutputMonad,
  english,
  german,
  translate,
  )
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.List (delete)
import Data.Set (difference, fromList, member, toList, union)
import Test.QuickCheck (Gen, elements)

import Config (StepAnswer(..), StepConfig(..), StepInst(..), BaseConfig(..))
import Formula.Util (isEmptyClause, mkClause)
import Formula.Types (Clause, Literal(..), genClause, literals, opposite)
import Formula.Resolution (resolvable, resolve)
import LogicTasks.Helpers (clauseKey)
import Util (checkBaseConf, prevent, preventWithHint, tryGen)




genStepInst :: StepConfig -> Gen StepInst
genStepInst StepConfig{ baseConf = BaseConfig{..}, ..} = do
    (clause2, resolveLit, lits1) <- genResStepClause minClauseLength maxClauseLength usedLiterals
    let
      litAddedClause1 = mkClause $ resolveLit : lits1
      litAddedClause2 = mkClause $ opposite resolveLit : literals clause2
    pure $ StepInst litAddedClause1 litAddedClause2 extraText



description :: OutputMonad m => StepInst -> LangM m
description StepInst{..} = do
  paragraph $ do
    translate $ do
      german "Betrachten Sie die zwei folgenden Klauseln:"
      english "Consider the two following clauses:"
    indent $ code $ show clause1
    indent $ code $ show clause2
    pure ()
  paragraph $ translate $ do
    german "Resolvieren Sie die Klauseln und geben Sie die Resolvente an."
    english "Resolve the clauses and give the resulting resolvent."

  paragraph $ translate $ do
    german "Geben Sie das in dem Resolutionsschritt genutzte Literal (in positiver oder negativer Form) und das Ergebnis in der folgenden Tupelform an: (Literal, Resolvente)."
    english "Provide the literal (in positive or negative form) used for the step and the resolvent in the following tuple form: (literal, resolvent)."

  clauseKey

  paragraph $ indent $ do
    translate $ do
      german "Ein Lösungsversuch könnte beispielsweise so aussehen: "
      english "A valid solution could look like this: "
    code "(A, not B or C)"
    pure ()
  paragraph $ text (fromMaybe "" addText)
  pure ()


verifyStatic :: OutputMonad m => StepInst -> LangM m
verifyStatic StepInst{..}
    | any isEmptyClause [clause1, clause2] =
        refuse $ indent $ translate $ do
          german "Mindestens eine der Klauseln ist leer."
          english "At least one of the clauses is empty."

    | not $ resolvable clause1 clause2 =
        refuse $ indent $ translate $ do
          german "Die Klauseln sind nicht resolvierbar."
          english "The clauses are not resolvable."

    | otherwise = pure()



verifyQuiz :: OutputMonad m => StepConfig -> LangM m
verifyQuiz StepConfig{..} = checkBaseConf baseConf



start :: StepAnswer
start = StepAnswer Nothing



partialGrade :: OutputMonad m => StepInst -> StepAnswer -> LangM m
partialGrade StepInst{..} sol = do

  prevent (isNothing $ step sol) $
    translate $ do
      german "Lösung ist nicht leer?"
      english "The solution is not empty?"

  prevent (not (fst mSol `member` availLits)) $
    translate $ do
      german "Das gewählte Literal kommt in einer der Klauseln vor?"
      english "The chosen literal is contained in any of the clauses?"

  preventWithHint (not $ null extra)
    (translate $ do
      german "Resolvente besteht aus bekannten Literalen?"
      english "Resolvent contains only known literals?"
    )
    (paragraph $ do
      translate $ do
        german "In der Resolvente sind unbekannte Literale enthalten. Diese Literale sind falsch: "
        english "The resolvent contains unknown literals. These literals are incorrect:"
      itemizeM $ map (text . show) extra
      pure ()
    )
  pure ()
  where
     mSol = fromJust $ step sol
     availLits = fromList (literals clause1) `union` fromList (literals clause2)
     solLits = fromList $ literals $ snd mSol
     extra = toList (solLits `difference` availLits)



completeGrade :: OutputMonad m => StepInst -> StepAnswer -> LangM m
completeGrade StepInst{..} sol =
    case resolve clause1 clause2 (fst mSol) of
        Nothing -> refuse $ indent $ translate $ do
                     german "Mit diesem Literal kann kein Schritt durchgeführt werden!"
                     english "This literal can not be used for a resolution step!"

        Just solClause -> if solClause == snd mSol
                            then pure()
                            else refuse $ indent $ translate $ do
                                   german "Resolvente ist nicht korrekt."
                                   english "Resolvent is not correct."
  where
    mSol = fromJust $ step sol


genResStepClause :: Int -> Int -> [Char] -> Gen (Clause, Literal, [Literal])
genResStepClause minClauseLength maxClauseLength usedLiterals = do
    rChar <- elements usedLiterals
    resolveLit <- elements [Literal rChar, Not rChar]
    let
      restLits = delete rChar usedLiterals
    minLen1 <- elements [minClauseLength-1..maxClauseLength-1]
    minLen2 <- elements [minClauseLength-1..maxClauseLength-1]
    clause1 <- genClause (minLen1,maxClauseLength-1) restLits
    let
      lits1 = literals clause1
    clause2 <- tryGen (genClause (minLen2,maxClauseLength-1) restLits) 100
        (all (\lit -> opposite lit `notElem` lits1) .  literals)
    pure (clause2, resolveLit, lits1)
