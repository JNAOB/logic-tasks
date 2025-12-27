{-# LANGUAGE RecordWildCards #-}
module DecomposeFormulaSpec where

import Capabilities.Cache.IO ()
import Capabilities.LatexSvg.IO ()
import Test.Hspec
import Tasks.DecomposeFormula.Config (
  DecomposeFormulaConfig(..),
  checkDecomposeFormulaConfig,
  defaultDecomposeFormulaConfig,
  DecomposeFormulaInst(..))
import Test.QuickCheck
import SynTreeSpec (validBoundsSynTreeConfig)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Control.OutputCapable.Blocks (LangM)
import Data.Maybe (fromJust)
import Tasks.DecomposeFormula.Quiz (generateDecomposeFormulaInst)
import Trees.Helpers (bothKids, binOp, swapKids)
import Trees.Types (SynTree(..), BinOp(..), TreeFormulaAnswer (TreeFormulaAnswer))
import Trees.Print (display)
import qualified Data.Map as Map (fromList)
import LogicTasks.Syntax.DecomposeFormula (verifyInst, description, partialGrade', completeGrade')
import TestHelpers (doesNotRefuse, doesNotRefuseIO)
import System.IO.Temp (withSystemTempDirectory)

validBoundsDecomposeFormulaConfig :: Gen DecomposeFormulaConfig
validBoundsDecomposeFormulaConfig = do
  syntaxTreeConfig <- validBoundsSynTreeConfig `suchThat` \SynTreeConfig{..} ->
    minUniqueBinOperators >= 1 && minUniqueBinOperators < 4 && minNodes > 6
  return DecomposeFormulaConfig {
    syntaxTreeConfig = syntaxTreeConfig {
      binOpFrequencies = Map.fromList
        [ (And, 1)
        , (Or, 1)
        , (Impl, 0)
        , (BackImpl, 0)
        , (Equi, 1)
        ]
    },
    extraText = Nothing,
    printSolution = False,
    offerUnicodeInput = False
  }

spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (checkDecomposeFormulaConfig defaultDecomposeFormulaConfig :: LangM Maybe)
    it "validBoundsDecomposeFormulaConfig should generate a valid config" $
      forAll validBoundsDecomposeFormulaConfig $ \decomposeFormulaConfig ->
        doesNotRefuse (checkDecomposeFormulaConfig decomposeFormulaConfig :: LangM Maybe)
  describe "description" $ do
    it "should not reject" $
      forAll validBoundsDecomposeFormulaConfig $ \config -> do
        within (30 * 100000) $ forAll (generateDecomposeFormulaInst config) $ \inst ->
          doesNotRefuse (description inst :: LangM Maybe)
  describe "generateDecomposeFormulaInst" $ do
    it "the generated instance should pass verifyInst" $
      forAll validBoundsDecomposeFormulaConfig $ \config -> do
        within (30 * 100000) $ forAll (generateDecomposeFormulaInst config) $ \inst ->
          doesNotRefuse (verifyInst inst :: LangM Maybe)
    it "should pass partialGrade with correct answer" $
      forAll validBoundsDecomposeFormulaConfig $ \config@DecomposeFormulaConfig{..} -> do
        within (30 * 100000) $ forAll (generateDecomposeFormulaInst config) $ \inst ->
          doesNotRefuse (partialGrade' inst (TreeFormulaAnswer $ Just $ swapKids $ tree inst) :: LangM Maybe)
    it "should pass completeGrade with correct answer" $
      forAll validBoundsDecomposeFormulaConfig $ \config@DecomposeFormulaConfig{..} -> do
        within (30 * 100000) $ forAll (generateDecomposeFormulaInst config) $ \inst ->
          ioProperty $
            withSystemTempDirectory "logic-tasks" $ \path ->
              doesNotRefuseIO (completeGrade' path inst (TreeFormulaAnswer $ Just $ swapKids $ tree inst))
    it "should generate an instance with different subtrees" $
      forAll validBoundsDecomposeFormulaConfig $ \decomposeFormulaConfig ->
        within (30 * 100000) $ forAll (generateDecomposeFormulaInst decomposeFormulaConfig) $ \DecomposeFormulaInst{..} ->
          let (lk,rk) = bothKids tree
              rootOp = fromJust $ binOp tree
          in notElem (display (Binary rootOp lk rk)) [display (Binary rootOp rk lk), reverse (display (Binary rootOp lk rk))]
