{-# LANGUAGE RecordWildCards #-}
module PrologSpec where
import Test.Hspec
import LogicTasks.Semantics.Prolog (genPrologInst, verifyQuiz, description, verifyStatic, partialGrade', completeGrade')
import Config (dPrologConf, PrologInst (..), PrologConfig (..))
import Formula.Helpers (hasTheClauseShape)
import Test.QuickCheck
import Control.OutputCapable.Blocks (LangM)
import TestHelpers (doesNotRefuse)



spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (verifyQuiz dPrologConf :: LangM Maybe)
  describe "description" $ do
    it "should not reject" $
      forAll (genPrologInst dPrologConf) $ \inst ->
        doesNotRefuse (description inst :: LangM Maybe)
  describe "genPrologInst" $ do
    it "should pass verifyStatic" $
      forAll (genPrologInst dPrologConf) $ \inst ->
        doesNotRefuse (verifyStatic inst :: LangM Maybe)
    it "should pass grading with correct answer" $
      forAll (genPrologInst dPrologConf) $ \inst ->
        doesNotRefuse (partialGrade' inst (solution inst) :: LangM Maybe) &&
        doesNotRefuse (completeGrade' inst (solution inst) :: LangM Maybe)
    it "should only generate PrologInst with horn clauses by default" $
      forAll (genPrologInst dPrologConf) $ \PrologInst {..} ->
        hasTheClauseShape (firstClauseShape dPrologConf) literals1
          && hasTheClauseShape (secondClauseShape dPrologConf) literals2
