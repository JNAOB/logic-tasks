{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module DecideSpec where

-- jscpd:ignore-start
import Test.Hspec
import Test.QuickCheck (forAll, Gen, choose, suchThat)
import Control.OutputCapable.Blocks (LangM, Rated)
import Config (dDecideConf, DecideConfig (..), DecideInst (..), FormulaConfig(..), DecideChoice (..))
import LogicTasks.Semantics.Decide (verifyQuiz, genDecideInst, verifyStatic, description, partialGrade, completeGrade)
-- import Data.Maybe (fromMaybe)
import SynTreeSpec (validBoundsSynTreeConfig)
import Formula.Types (Table(getEntries), getTable)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Util (withRatio, validBoundsPercentPosLiteral)
import FillSpec (validBoundsNormalFormConfig)
import LogicTasks.Util (formulaDependsOnAllAtoms)
import TestHelpers (doesNotRefuse)
-- jscpd:ignore-end

validBoundsDecideConfig :: Gen DecideConfig
validBoundsDecideConfig = do
  -- formulaType <- elements ["Cnf", "Dnf", "Arbitrary"]
  let formulaType = "Arbitrary"
  formulaConfig <- case formulaType of
    "Cnf" -> FormulaCnf <$> validBoundsNormalFormConfig
    "Dnf" -> FormulaDnf <$> validBoundsNormalFormConfig
    _ -> FormulaArbitrary <$> validBoundsSynTreeConfig `suchThat` \SynTreeConfig{..} ->
            maxNodes < 30 &&
            minAmountOfUniqueAtoms == fromIntegral (length availableAtoms)

  percentageOfChanged <- choose (1, 100)
  -- percentTrueEntriesLow' <- choose (1, 90)
  -- percentTrueEntriesHigh' <- choose (percentTrueEntriesLow', 99) `suchThat` (/= percentTrueEntriesLow')
  -- percentTrueEntries <- elements [Just (percentTrueEntriesLow', percentTrueEntriesHigh'), Nothing]
  percentTrueEntries <- case formulaConfig of FormulaArbitrary x -> validBoundsPercentPosLiteral (2 ^ length (availableAtoms x))

  pure $ DecideConfig {
      formulaConfig
    , percentageOfChanged
    , percentTrueEntries
    , printSolution = False
    , extraText = Nothing
    }

spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (verifyQuiz dDecideConf :: LangM Maybe)
    it "validBoundsDecideConfig should generate a valid config" $
      forAll validBoundsDecideConfig $ \decideConfig ->
        doesNotRefuse (verifyQuiz decideConfig :: LangM Maybe)
  describe "description" $ do
    it "should not reject" $
      forAll validBoundsDecideConfig $ \decideConfig@DecideConfig{..} -> do
        forAll (genDecideInst decideConfig) $ \inst ->
          doesNotRefuse (description False inst :: LangM Maybe)
  describe "genDecideInst" $ do
    it "should pass verifyStatic" $
      forAll validBoundsDecideConfig $ \decideConfig@DecideConfig{..} -> do
        forAll (genDecideInst decideConfig) $ \inst ->
          doesNotRefuse (verifyStatic inst :: LangM Maybe)
    it "should pass grading with correct answer" $
      forAll validBoundsDecideConfig $ \decideConfig@DecideConfig{..} -> do
        forAll (genDecideInst decideConfig) $ \inst ->
          doesNotRefuse
            (partialGrade
              inst
                [ if i `elem` changed inst then Wrong else Correct
                | i <- [1.. length $ getEntries $ getTable $ formula inst]] :: LangM Maybe) &&
          doesNotRefuse
            (completeGrade
              inst
                [ if i `elem` changed inst then Wrong else Correct
                | i <- [1.. length $ getEntries $ getTable $ formula inst]] :: Rated Maybe)
    it "should generate an instance with the right amount of changed entries" $
      forAll validBoundsDecideConfig $ \decideConfig@DecideConfig{..} -> do
        forAll (genDecideInst decideConfig) $ \DecideInst{..} ->
          let tableLen = length (getEntries (getTable formula))
              mistakeCount = max (tableLen * percentageOfChanged `div` 100) 1 in
          length changed == mistakeCount
    it "generated formula should depend on all atomics" $
      forAll validBoundsDecideConfig $ \decideConfig@DecideConfig{..} -> do
        forAll (genDecideInst decideConfig) $ \DecideInst{..} ->
          formulaDependsOnAllAtoms formula
    it "the generated instance should pass verifyStatic" $
      forAll validBoundsDecideConfig $ \decideConfig -> do
        forAll (genDecideInst decideConfig) $ \decideInst ->
          doesNotRefuse (verifyStatic decideInst :: LangM Maybe)
    it "should respect percentTrueEntries" $
      forAll validBoundsDecideConfig $ \decideConfig@DecideConfig{..} -> do
        forAll (genDecideInst decideConfig) $ \DecideInst{..} ->
          withRatio (percentTrueEntries) formula

