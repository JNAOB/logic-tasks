{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module ResolutionSpec where

import Test.Hspec
import Formula.Resolution (applySteps)
import Data.Maybe (isJust, fromJust, isNothing)
import Formula.Types (Clause(Clause), Literal (..), Formula (literals))
import qualified Data.Set
import Config (ResolutionConfig (..), BaseConfig (..), dResConf, ResolutionInst(solution, clauses))
import Test.QuickCheck (Gen, chooseInt, forAll)
import LogicTasks.Semantics.Resolve (verifyQuiz, genResInst, completeGrade', partialGrade', description, verifyStatic)
import Control.OutputCapable.Blocks (LangM)
import FillSpec (validBoundsBaseConfig)
import TestHelpers (doesNotRefuse)
import Test.QuickCheck.Property (within)

justA :: Clause
justA = Clause (Data.Set.fromList [Positive 'A'])

notAnotB :: Clause
notAnotB = Clause (Data.Set.fromList [Negative 'A', Negative 'B'])

notAjustB :: Clause
notAjustB = Clause (Data.Set.fromList [Negative 'A', Positive 'B'])

notB :: Clause
notB = Clause (Data.Set.fromList [Negative 'B'])

justB :: Clause
justB = Clause (Data.Set.fromList [Positive 'B'])

emptyClause :: Clause
emptyClause = Clause Data.Set.empty

containsNoTautologies :: [Clause] -> Bool
containsNoTautologies = all ((\theList -> length theList == length (Data.Set.fromList theList)) . list)
  where
    list = map (\case
      Positive y -> y
      Negative y -> y)
      . literals

validBoundsResolutionConfig :: Gen ResolutionConfig
validBoundsResolutionConfig = do
  baseConf <- validBoundsBaseConfig
  minSteps <- chooseInt (1, min (if maxClauseLength baseConf > 1 then 10 else 1) (2 * length (usedAtoms baseConf) - 1))
  pure $ ResolutionConfig {
    baseConf
  , minSteps
  , printFeedbackImmediately = False
  , useSetNotation = True
  , printSolution = False
  , extraText = Nothing
  , offerUnicodeInput = False
  }


spec :: Spec
spec = do
  describe "applySteps" $ do
    it "should return a Just value if there are no clauses" $
      isJust $ applySteps [] []
    it "should return the original list of clauses if there are no steps to apply" $ do
      let clauses = [Clause (Data.Set.fromList [Positive 'A'])]
      fromJust (applySteps clauses []) == clauses
    it "should return the correct list of clauses if the steps are able to be applied" $ do
      let clauses = [justA, notAnotB, notAjustB]
      let steps = [(justA, notAnotB, notB)
                 , (justA, notAjustB, justB)
                 , (notB, justB, emptyClause)]
      let result = fromJust (applySteps clauses steps)
      notB `elem` result && justB `elem` result && emptyClause `elem` result
    it "should return Nothing if some step is invalid" $ do
      let clauses = [justA, notAnotB, notAjustB]
      let steps = [(justA, notAnotB, justB)]
      isNothing $ applySteps clauses steps
    it "should return Nothing if some steps use unavailable clauses" $ do
      let clauses = [justB, notAnotB]
      let steps = [(justB, notB, emptyClause)]
      isNothing $ applySteps clauses steps
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (verifyQuiz dResConf :: LangM Maybe)
    it "validBoundsResolutionConfig should generate a valid config" $
      forAll validBoundsResolutionConfig $ \resConfig ->
        doesNotRefuse (verifyQuiz resConfig :: LangM Maybe)
  describe "description" $ do
    it "should not reject" $
      forAll validBoundsResolutionConfig $ \resConfig ->
        within (30 * 100000) $ forAll (genResInst resConfig) $ \resInst ->
          doesNotRefuse (description resInst :: LangM Maybe)
  describe "genResInst" $ do
    it "should required at least minSteps amount of steps" $
      forAll validBoundsResolutionConfig $ \resConfig ->
        within (30 * 100000) $ forAll (genResInst resConfig) $ \resInst ->
          minSteps resConfig <= length (solution resInst)
    it "should contain no clause with a literal that appears both positively and negatively" $
      forAll validBoundsResolutionConfig $ \resConfig ->
        within (30 * 100000) $ forAll (genResInst resConfig) $ \resInst ->
          containsNoTautologies (clauses resInst)
    it "should pass verifyStatic" $
      forAll validBoundsResolutionConfig $ \resConfig ->
        within (30 * 100000) $ forAll (genResInst resConfig) $ \resInst ->
          doesNotRefuse (verifyStatic resInst :: LangM Maybe)
    it "should generate the correct solution" $
      forAll validBoundsResolutionConfig $ \resConfig ->
        within (30 * 100000) $ forAll (genResInst resConfig) $ \resInst ->
          doesNotRefuse (partialGrade' resInst (solution resInst) :: LangM Maybe) &&
          doesNotRefuse (completeGrade' resInst (solution resInst) :: LangM Maybe)

