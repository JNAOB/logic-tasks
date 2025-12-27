{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module ComposeFormulaSpec where

import Capabilities.Cache.IO ()
import Capabilities.LatexSvg.IO ()
import Test.Hspec
import Tasks.ComposeFormula.Config (
  ComposeFormulaConfig(..),
  TreeDisplayMode (FormulaDisplay),
  checkComposeFormulaConfig,
  defaultComposeFormulaConfig,
  ComposeFormulaInst(..))
import Test.QuickCheck
import SynTreeSpec (validBoundsSynTreeConfig)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Control.OutputCapable.Blocks (LangM)
import Data.Maybe (isJust)
import Tasks.ComposeFormula.Quiz (generateComposeFormulaInst)
import Trees.Types (SynTree(Binary), TreeFormulaAnswer (TreeFormulaAnswer))
import LogicTasks.Syntax.ComposeFormula (partialGrade', description, completeGrade', verifyInst)
import TestHelpers (doesNotRefuse, doesNotRefuseIO)
import System.IO.Temp (withSystemTempDirectory)

validBoundsComposeFormulaConfig :: Gen ComposeFormulaConfig
validBoundsComposeFormulaConfig = do
  syntaxTreeConfig <- validBoundsSynTreeConfig `suchThat` \SynTreeConfig{..} ->
    minUniqueBinOperators >= 1 && minNodes > 6
  displayModeL <- elements [minBound..maxBound :: TreeDisplayMode]
  displayModeR <- elements [minBound..maxBound :: TreeDisplayMode]
  return ComposeFormulaConfig {
    syntaxTreeConfig,
    treeDisplayModes = (displayModeL, displayModeR),
    extraText = Nothing,
    printSolution = False,
    offerUnicodeInput = False
  }

spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (checkComposeFormulaConfig defaultComposeFormulaConfig :: LangM Maybe)
    it "validBoundsComposeFormulaConfig should generate a valid config" $
      forAll validBoundsComposeFormulaConfig $ \composeFormulaConfig ->
        doesNotRefuse (checkComposeFormulaConfig composeFormulaConfig :: LangM Maybe)
  describe "description" $ do
    it "should not reject" $
      forAll validBoundsComposeFormulaConfig $ \config ->
        within (30 * 100000) $ forAll (generateComposeFormulaInst config) $ \inst -> ioProperty $
          withSystemTempDirectory "logic-tasks" $ \path ->
            doesNotRefuseIO (description False path inst)
  describe "generateComposeFormulaInst" $ do
    it "should pass verifyInst" $
      forAll validBoundsComposeFormulaConfig $ \composeFormulaConfig ->
        within (30 * 100000) $ forAll (generateComposeFormulaInst composeFormulaConfig) $ \inst ->
          doesNotRefuse
            (verifyInst inst :: LangM Maybe)
    it "possible solution passes partialGrade" $
      forAll validBoundsComposeFormulaConfig $ \composeFormulaConfig ->
        within (30 * 100000) $ forAll (generateComposeFormulaInst composeFormulaConfig) $ \inst@ComposeFormulaInst{..} ->
          let lrTree = Binary operator leftTree rightTree
              rlTree = Binary operator rightTree leftTree
          in doesNotRefuse
            (partialGrade' inst [TreeFormulaAnswer (Just lrTree), TreeFormulaAnswer (Just rlTree)] :: LangM Maybe)
    it "possible solution passes completeGrade" $
      forAll validBoundsComposeFormulaConfig $ \composeFormulaConfig ->
        within (30 * 100000) $ forAll (generateComposeFormulaInst composeFormulaConfig) $ \inst@ComposeFormulaInst{..} ->
          let lrTree = Binary operator leftTree rightTree
              rlTree = Binary operator rightTree leftTree
          in ioProperty $
            withSystemTempDirectory "logic-tasks" $ \path ->
              doesNotRefuseIO (completeGrade' path inst [TreeFormulaAnswer (Just lrTree), TreeFormulaAnswer (Just rlTree)])
    it "leftTreeImage and rightTreeImage has the right value" $
      forAll validBoundsComposeFormulaConfig $ \composeFormulaConfig@ComposeFormulaConfig{..} ->
        within (30 * 100000) $ forAll (generateComposeFormulaInst composeFormulaConfig) $ \ComposeFormulaInst{..} ->
          fst treeDisplayModes == FormulaDisplay || isJust leftTreeImage &&
          snd treeDisplayModes == FormulaDisplay || isJust rightTreeImage
