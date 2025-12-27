{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module PickSpec where
import Control.OutputCapable.Blocks (LangM)
import Test.Hspec (Spec, describe, it)
import Config (dPickConf, PickConfig (..), PickInst (..), FormulaConfig(..), Number (Number))
import LogicTasks.Semantics.Pick (verifyQuiz, genPickInst, verifyStatic, description, partialGrade, completeGrade)
import Data.Maybe (fromMaybe)
import Test.QuickCheck (Gen, chooseInt, forAll, suchThat)
import SynTreeSpec (validBoundsSynTreeConfig')
import Tasks.SynTree.Config (SynTreeConfig(..))
import Formula.Util (isSemanticEqual)
import Data.List.Extra (nubOrd, nubSort, nubBy)
import Util (withRatio)
import Formula.Types(atomics)
import FillSpec (validBoundsNormalFormConfig, validBoundsPercentTrueEntries)
import LogicTasks.Util (formulaDependsOnAllAtoms)
import TestHelpers (doesNotRefuse)
import Test.QuickCheck.Property (within)

validBoundsPickConfig :: Gen PickConfig
validBoundsPickConfig = do
  amountOfOptions <- chooseInt (2, 5)
  -- formulaType <- elements ["Cnf", "Dnf", "Arbitrary"]
  let formulaType = "Arbitrary"
  formulaConfig <- case formulaType of
    "Cnf" -> FormulaCnf <$> validBoundsNormalFormConfig
    "Dnf" -> FormulaDnf <$> validBoundsNormalFormConfig
    _ -> FormulaArbitrary <$> validBoundsSynTreeConfig' False `suchThat` \SynTreeConfig{..} ->
            amountOfOptions <= 4*2^ length availableAtoms &&
            maxNodes <= 40 &&
            length availableAtoms >= 3

  percentTrueEntries' <- (do
    percentTrueEntriesLow' <- chooseInt (1, 90)
    percentTrueEntriesHigh' <- chooseInt (percentTrueEntriesLow' + 1, 99)
    return (percentTrueEntriesLow', percentTrueEntriesHigh')
    ) `suchThat` \(a,b) -> b - a >= 30

  percentTrueEntries''@(l,h) <- validBoundsPercentTrueEntries formulaConfig

  let percentTrueEntries = if h - l < 30 then Just percentTrueEntries' else Just percentTrueEntries''

  pure $ PickConfig {
      formulaConfig
    , amountOfOptions
    , percentTrueEntries
    , printSolution = False
    , extraText = Nothing
    }

spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (verifyQuiz dPickConf :: LangM Maybe)
    it "validBoundsPickConfig should generate a valid config" $
      forAll validBoundsPickConfig $ \pickConfig ->
        doesNotRefuse (verifyQuiz pickConfig :: LangM Maybe)
  describe "description" $ do
    it "should not reject" $
      forAll validBoundsPickConfig $ \pickConfig@PickConfig{..} ->
        within (30 * 100000) $ forAll (genPickInst pickConfig) $ \inst ->
          doesNotRefuse (description False inst :: LangM Maybe)
  describe "genPickInst" $ do
    it "generated formulas should not be semantically equivalent" $
      forAll validBoundsPickConfig $ \pickConfig@PickConfig{..} ->
        within (30 * 100000) $ forAll (genPickInst pickConfig) $ \PickInst{..} ->
          length (nubBy isSemanticEqual formulas) == amountOfOptions
    it "generated formulas should only consist of the same atomics" $
      forAll validBoundsPickConfig $ \pickConfig ->
        within (30 * 100000) $ forAll (genPickInst pickConfig) $ \PickInst{..} ->
          length (nubOrd (map (nubSort . atomics) formulas)) == 1
    it "generated formulas should depend on all atomics" $
      forAll validBoundsPickConfig $ \pickConfig ->
        within (30 * 100000) $ forAll (genPickInst pickConfig) $ \PickInst{..} ->
          all formulaDependsOnAllAtoms formulas
    it "the generated instance should pass verifyStatic" $
      forAll validBoundsPickConfig $ \pickConfig -> do
        within (30 * 100000) $ forAll (genPickInst pickConfig) $ \pickInst ->
          doesNotRefuse (verifyStatic pickInst :: LangM Maybe)
    it "should respect percentTrueEntries" $
      forAll validBoundsPickConfig $ \pickConfig@PickConfig{..} ->
        within (30 * 100000) $ forAll (genPickInst pickConfig) $ \PickInst{..} ->
          all (withRatio (fromMaybe (0, 100) percentTrueEntries)) formulas
    it "the generated solution should pass grading" $
      forAll validBoundsPickConfig $ \pickConfig@PickConfig{..} ->
        within (30 * 100000) $ forAll (genPickInst pickConfig) $ \inst ->
          doesNotRefuse (partialGrade inst (Number $ Just $ correct inst)  :: LangM Maybe) &&
          doesNotRefuse (completeGrade inst (Number $ Just $ correct inst)  :: LangM Maybe)

