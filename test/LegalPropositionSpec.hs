{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module LegalPropositionSpec (spec) where

import Capabilities.Cache.IO ()
import Capabilities.LatexSvg.IO ()
import Data.Either (isLeft, isRight)
import Data.List ((\\))
import Data.Char (isLetter)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, chooseInteger, forAll, suchThat, within, ioProperty)
import Data.Tuple.Extra (thd3)

import Tasks.LegalProposition.Config (
  LegalPropositionConfig (..),
  LegalPropositionInst(..),
  checkLegalPropositionConfig,
  defaultLegalPropositionConfig,
  propFormulaIsErroneous)
import Tasks.LegalProposition.PrintIllegal (illegalDisplay)
import Tasks.LegalProposition.PrintBracket (bracketDisplay,)
import Tasks.LegalProposition.Quiz (generateLegalPropositionInst)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Trees.Parsing (formulaParse)
import Trees.Generate (genSynTree)
import SynTreeSpec (validBoundsSynTreeConfig)
import Trees.Print (display)
import TestHelpers (deleteBrackets, deleteSpaces, doesNotRefuse, doesNotRefuseIO)
import Control.OutputCapable.Blocks (LangM)
import Tasks.LegalProposition.Helpers (formulaAmount)
import LogicTasks.Syntax.IllegalFormulas (description, partialGrade, verifyInst, completeGrade)
import System.IO.Temp (withSystemTempDirectory)

validBoundsLegalPropositionConfig :: Gen LegalPropositionConfig
validBoundsLegalPropositionConfig = do
    formulas <- chooseInteger (1, 15)
    syntaxTreeConfig@SynTreeConfig {..}  <- validBoundsSynTreeConfig
      `suchThat` \cfg -> formulaAmount cfg >= formulas
    illegals <- chooseInteger (0, formulas)
    bracketFormulas <- chooseInteger (0, formulas - illegals)
    return $ LegalPropositionConfig
        {
            syntaxTreeConfig
            , formulas
            , illegals
            , bracketFormulas
            , extraText = Nothing
            , printDetailedSolution = Nothing
        }

timeout :: Int
timeout = 30000000 -- 30 seconds

spec :: Spec
spec = do
    describe "config" $ do
      it "default config should pass config check" $
        doesNotRefuse (checkLegalPropositionConfig defaultLegalPropositionConfig :: LangM Maybe)
      it "validBoundsLegalPropositionConfig should generate a valid config" $
        forAll validBoundsLegalPropositionConfig $ \legalPropConfig ->
          doesNotRefuse (checkLegalPropositionConfig legalPropConfig :: LangM Maybe)
    describe "description" $ do
      it "should not reject" $
        forAll validBoundsLegalPropositionConfig $ \config ->
          within timeout $ forAll (generateLegalPropositionInst config) $ \inst ->
            doesNotRefuse (description False inst :: LangM Maybe)
    describe "illegalDisplay" $ do
        it "at least creates actual formula symbols" $
            forAll validBoundsSynTreeConfig $ \synTreeConfig ->
                within timeout $ forAll
                  (genSynTree synTreeConfig) $ \synTree ->
                      forAll (deleteSpaces . fst <$> illegalDisplay synTree) $
                      all (\c -> c `elem` "()∧∨¬<=>" || isLetter c)
        it "the string after illegalDisplay cannot be parsed" $
            forAll validBoundsSynTreeConfig $ \synTreeConfig ->
                within timeout $ forAll
                  (genSynTree synTreeConfig) $ \synTree ->
                      forAll (fst <$> illegalDisplay synTree) $ \str -> isLeft (formulaParse str)
    describe "bracket display" $ do
        it "the String after bracketDisplay just add a bracket " $
            forAll validBoundsSynTreeConfig $ \synTreeConfig ->
                within timeout $ forAll
                  (genSynTree synTreeConfig) $ \synTree ->
                      forAll (bracketDisplay synTree) $ \str -> length str == length (display synTree) + 2
        it "the String can be parsed by formulaParse" $
            forAll validBoundsSynTreeConfig $ \synTreeConfig ->
                within timeout $ forAll
                  (genSynTree synTreeConfig) $ \synTree ->
                      forAll (bracketDisplay synTree) $ \str -> formulaParse str == Right synTree
        it "the String remove all brackets should same with display remove all brackets" $
            forAll validBoundsSynTreeConfig $ \synTreeConfig ->
                within timeout $ forAll
                  (genSynTree synTreeConfig) $ \synTree ->
                      forAll (bracketDisplay synTree) $ \str -> deleteBrackets str == deleteBrackets (display synTree)
    describe "generateLegalPropositionInst" $ do
        it "the generateLegalPropositionInst should generate expected illegal number" $
            forAll validBoundsLegalPropositionConfig $ \config ->
                within timeout $ forAll (generateLegalPropositionInst config) $ \LegalPropositionInst{..} ->
                  let serialsOfWrong = [i | (i,info,_) <- formulaInfos, propFormulaIsErroneous info] in
                    all (\x -> isLeft (formulaParse (thd3 (formulaInfos !! (x - 1))))) serialsOfWrong
        it "the generateLegalPropositionInst should generate expected legal number" $
            forAll validBoundsLegalPropositionConfig $ \config@LegalPropositionConfig{..} ->
                within timeout $ forAll (generateLegalPropositionInst config) $ \LegalPropositionInst{..} ->
                  let serialsOfWrong = [i | (i,info,_) <- formulaInfos, propFormulaIsErroneous info] in
                    all
                    (\x -> isRight (formulaParse (thd3 (formulaInfos !! (x - 1)))))
                    ([1 .. fromIntegral formulas] \\ serialsOfWrong)
        it "the generateLegalPropositionInst should pass verifyStatic" $
            forAll validBoundsLegalPropositionConfig $ \config@LegalPropositionConfig{..} ->
                within timeout $ forAll (generateLegalPropositionInst config) $ \inst ->
                   doesNotRefuse (verifyInst inst :: LangM Maybe)
        it "the generateLegalPropositionInst should pass grading" $
            forAll validBoundsLegalPropositionConfig $ \config@LegalPropositionConfig{..} ->
                within timeout $ forAll (generateLegalPropositionInst config) $ \inst ->
                   doesNotRefuse (partialGrade inst [i | (i,info,_) <- formulaInfos inst, not $ propFormulaIsErroneous info] :: LangM Maybe)
        it "the generateLegalPropositionInst should pass grading" $
          forAll validBoundsLegalPropositionConfig $ \config@LegalPropositionConfig{..} ->
            within timeout $ forAll (generateLegalPropositionInst config) $ \inst -> ioProperty $
              withSystemTempDirectory "logic-tasks" $ \path ->
                doesNotRefuseIO (completeGrade path inst [i | (i,info,_) <- formulaInfos inst, not $ propFormulaIsErroneous info])
