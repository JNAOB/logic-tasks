module StepSpec where

import Test.Hspec (Spec, describe, it)
import Control.OutputCapable.Blocks (LangM)
import TestHelpers (doesNotRefuse)
import Test.QuickCheck (forAll)
import Config (StepInst(solution), dStepConf, StepAnswer (StepAnswer))
import LogicTasks.Semantics.Step (verifyQuiz, genStepInst, description, verifyStatic, partialGrade', completeGrade')



spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (verifyQuiz dStepConf :: LangM Maybe)
  describe "description" $ do
    it "should not reject" $
      forAll (genStepInst dStepConf) $ \inst ->
        doesNotRefuse (description True inst :: LangM Maybe)
  describe "generateStepInst" $ do
    it "should pass verifyStatic" $
      forAll (genStepInst dStepConf) $ \inst ->
        doesNotRefuse
          (verifyStatic inst :: LangM Maybe)
    it "possible solution passes partialGrade" $
      forAll (genStepInst dStepConf) $ \inst ->
        doesNotRefuse (partialGrade' inst $ StepAnswer $ Just $ solution inst :: LangM Maybe)
    it "possible solution passes completeGrade" $
      forAll (genStepInst dStepConf) $ \inst ->
        doesNotRefuse (completeGrade' inst $ StepAnswer $ Just $ solution inst :: LangM Maybe)
