{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module FillSpec where

-- jscpd:ignore-start
import Test.Hspec
import Test.QuickCheck (forAll, Gen, chooseInt, elements, suchThat)
import Control.OutputCapable.Blocks (LangM, Rated)
import Config (
  dFillConf,
  FillConfig (..),
  FillInst (..),
  FormulaConfig(..),
  BaseConfig(..),
  dBaseConf,
  NormalFormConfig(..),
  dNormalFormConf
 )
import LogicTasks.Semantics.Fill (verifyQuiz, genFillInst, verifyStatic, partialGrade, completeGrade, description)
import Data.Maybe (fromMaybe)
import SynTreeSpec (validBoundsSynTreeConfig)
import Formula.Types (Table(getEntries), getTable, lengthBound, TruthValue (TruthValue))
import Tasks.SynTree.Config (SynTreeConfig(..))
import Util (withRatio, checkBaseConf, checkNormalFormConfig)
import LogicTasks.Util (formulaDependsOnAllAtoms)
import TestHelpers (doesNotRefuse, genSublistOf)
-- jscpd:ignore-end

validBoundsBaseConfig :: Gen BaseConfig
validBoundsBaseConfig = do
  minClauseLength <- chooseInt (1, 5)
  maxClauseLength <- chooseInt (max 2 minClauseLength, 10)
  usedAtoms <- genSublistOf (maxClauseLength, 26) ['A' .. 'Z']
  pure $ BaseConfig {
    minClauseLength
  , maxClauseLength
  , usedAtoms
  }

validBoundsNormalFormConfig :: Gen NormalFormConfig
validBoundsNormalFormConfig = do
  minClauseAmount <- chooseInt (1, 5)
  maxClauseAmount <- chooseInt (max 2 minClauseAmount, 10)
  baseConf <- validBoundsBaseConfig `suchThat` \bc ->
    minClauseAmount * minClauseLength bc >= length (usedAtoms bc) &&
    minClauseAmount <= 2 ^ length (usedAtoms bc) &&
    minClauseAmount <= lengthBound (length (usedAtoms bc)) (maxClauseLength bc)
  pure $ NormalFormConfig {
    baseConf
  , minClauseAmount
  , maxClauseAmount
  }

validBoundsFillConfig :: Gen FillConfig
validBoundsFillConfig = do
  -- formulaType <- elements ["Cnf", "Dnf", "Arbitrary"]
  let formulaType = "Arbitrary"
  formulaConfig <- case formulaType of
    "Cnf" -> FormulaCnf <$> validBoundsNormalFormConfig
    "Dnf" -> FormulaDnf <$> validBoundsNormalFormConfig
    _ -> FormulaArbitrary <$> validBoundsSynTreeConfig `suchThat` \SynTreeConfig{..} ->
            maxNodes < 30 &&
            minAmountOfUniqueAtoms == fromIntegral (length availableAtoms)

  percentageOfGaps <- chooseInt (1, 100)
  percentTrueEntriesLow' <- chooseInt (0, 90)
  percentTrueEntriesHigh' <- chooseInt (percentTrueEntriesLow' + 1, 100)
  percentTrueEntries <- elements [Just (percentTrueEntriesLow', percentTrueEntriesHigh'), Nothing]

  pure $ FillConfig {
      formulaConfig
    , percentageOfGaps
    , percentTrueEntries
    , printSolution = False
    , extraText = Nothing
    }

spec :: Spec
spec = do
  describe "BaseConfig" $ do
    it "default base config should pass config check" $
      doesNotRefuse (checkBaseConf dBaseConf :: LangM Maybe)
    it "validBoundsBaseConfig should generate a valid config" $
      forAll validBoundsBaseConfig $ \baseConfig ->
        doesNotRefuse (checkBaseConf baseConfig :: LangM Maybe)
  describe "NormalFormConfig" $ do
    it "default cnf config should pass config check" $
      doesNotRefuse (checkNormalFormConfig dNormalFormConf :: LangM Maybe)
    it "validBoundsNormalFormConfig should generate a valid config" $
      forAll validBoundsNormalFormConfig $ \normalFormConfig ->
        doesNotRefuse (checkNormalFormConfig normalFormConfig :: LangM Maybe)
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (verifyQuiz dFillConf :: LangM Maybe)
    it "validBoundsFillConfig should generate a valid config" $
      forAll validBoundsFillConfig $ \fillConfig ->
        doesNotRefuse (verifyQuiz fillConfig :: LangM Maybe)
  describe "description" $ do
    it "should not reject" $
      forAll validBoundsFillConfig $ \fillConfig@FillConfig{..} -> do
        forAll (genFillInst fillConfig) $ \inst ->
          doesNotRefuse (description False inst :: LangM Maybe)
  describe "genFillInst" $ do
    it "should generate an instance with the right amount of gaps" $
      forAll validBoundsFillConfig $ \fillConfig@FillConfig{..} -> do
        forAll (genFillInst fillConfig) $ \FillInst{..} ->
          let tableLen = length (getEntries (getTable formula))
              gapCount = max (tableLen * percentageOfGaps `div` 100) 1 in
          length missing == gapCount
    it "generated formula should depend on all atomics" $
     forAll validBoundsFillConfig $ \fillConfig@FillConfig{..} -> do
        forAll (genFillInst fillConfig) $ \FillInst{..} ->
          formulaDependsOnAllAtoms formula
    it "should respect percentTrueEntries" $
      forAll validBoundsFillConfig $ \fillConfig@FillConfig{..} ->
        forAll (genFillInst fillConfig) $ \FillInst{..} ->
          withRatio (fromMaybe (0, 100) percentTrueEntries) formula
    it "the generated instance should pass verifyStatic" $
      forAll validBoundsFillConfig $ \fillConfig -> do
        forAll (genFillInst fillConfig) $ \fillInst ->
          doesNotRefuse (verifyStatic fillInst :: LangM Maybe)
    it "the generated solution should pass grading" $
      forAll validBoundsFillConfig $ \fillConfig -> do
        forAll (genFillInst fillConfig) $ \fillInst ->
          doesNotRefuse (partialGrade fillInst (map TruthValue (missingValues fillInst))  :: LangM Maybe) &&
          doesNotRefuse (completeGrade fillInst (map TruthValue (missingValues fillInst))  :: Rated Maybe)

