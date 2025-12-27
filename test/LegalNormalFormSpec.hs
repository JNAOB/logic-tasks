{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module LegalNormalFormSpec (spec) where

import Data.Either(isLeft, isRight)
import Test.Hspec (Spec, describe, it, xit)
import Test.QuickCheck (Gen, chooseInt, forAll, elements, ioProperty, withMaxSuccess, within)
import Data.List((\\))
import Data.Tuple.Extra (thd3)

import ParsingHelpers (fully)
import Formula.Types (Cnf, genCnf, genDnf, Dnf)
import Formula.Parsing (parser)
import Text.ParserCombinators.Parsec (ParseError, parse)
import Config (NormalFormConfig(..), BaseConfig(..))
import Trees.Types (SynTree(..), BinOp(..))
import Trees.Helpers (cnfToSynTree, dnfToSynTree)
import Tasks.LegalNormalForm.Config (
  LegalNormalFormConfig(..),
  LegalNormalFormInst(..),
  checkLegalNormalFormConfig,
  defaultLegalNormalFormConfig,
  treeIsErroneous
  )
import Tasks.LegalNormalForm.GenerateIllegal (genIllegalCnfSynTree, genIllegalDnfSynTree, )
import Tasks.LegalNormalForm.Quiz (generateLegalCNFInst, generateLegalDNFInst)
import Control.OutputCapable.Blocks (Language(German), LangM, Rated)
import Control.OutputCapable.Blocks.Debug(checkConfigWith)

import FormulaSpec (validBoundsNormalFormParams)
import qualified LogicTasks.Syntax.IllegalCnfs as IllegalCnfs (description, verifyInst, partialGrade, completeGrade)
import qualified LogicTasks.Syntax.IllegalDnfs as IllegalDnfs (description, verifyInst, partialGrade, completeGrade)
import TestHelpers (doesNotRefuse, genSubsetOf)

validBoundsLegalNormalFormConfig :: Gen LegalNormalFormConfig
validBoundsLegalNormalFormConfig = do
    ((minClauseAmount,maxClauseAmount),(minClauseLength,maxClauseLength),usedAtoms) <- validBoundsNormalFormParams

    let
      maxFormulas = (maxClauseLength - minClauseLength + 1) ^ (maxClauseAmount - minClauseAmount + 1) `div` 2 + 1
      formulaUpperBound
        | maxFormulas < 0 = 15 -- Int overflow
        | otherwise = min 15 maxFormulas

    formulas <- chooseInt
      (1, formulaUpperBound)
    illegals <- chooseInt (0, formulas)
    let includeFormWithJustOneClause = minClauseAmount == 1 && formulas - illegals > 0
        includeFormWithJustOneLiteralPerClause = minClauseLength == 1 && formulas - illegals > 1
    allowArrowOperators <- elements [True, False]
    return $ LegalNormalFormConfig
        {
          normalFormConfig = NormalFormConfig{
            minClauseAmount,
            maxClauseAmount,
            baseConf = BaseConfig{
                minClauseLength,
                maxClauseLength,
                usedAtoms
            }
          },
          formulas,
          illegals,
          includeFormWithJustOneClause,
          includeFormWithJustOneLiteralPerClause,
          maxStringSize =  maxClauseAmount * (maxClauseLength * 6 + 5),
          minStringSize = minClauseAmount * ((minClauseLength - 1) * 4 + 1),
          allowArrowOperators,
          printDetailedSolution = Nothing,
          extraText = Nothing
        }

invalidBoundsLegalCNF :: Gen LegalNormalFormConfig
invalidBoundsLegalCNF = do
    usedAtoms <- genSubsetOf (1, 10) ['A' .. 'Z']
    maxClauseLength <- chooseInt (1, 2 * length usedAtoms)
    minClauseLength <- chooseInt (maxClauseLength, 100)
    let clauses = product (take maxClauseLength (reverse [1 .. (2 * length usedAtoms)]))
    maxClauseAmount <- chooseInt (if maxClauseLength > 1 then 1 else 2, max 15 clauses)
    minClauseAmount <- chooseInt (1, maxClauseAmount + 20)
    formulas <- chooseInt (-10, max 15 (maxClauseLength - minClauseLength + 1) ^ (maxClauseAmount - minClauseAmount + 1))
    illegals <- chooseInt (-5, -1)
    minStringSize <- chooseInt (minClauseLength * (2 + minClauseAmount), 300)
    maxStringSize <- chooseInt (1, minStringSize)
    return $ LegalNormalFormConfig
        {
          normalFormConfig = NormalFormConfig{
            minClauseAmount,
            maxClauseAmount,
            baseConf = BaseConfig{
                minClauseLength,
                maxClauseLength,
                usedAtoms
            }
          },
          formulas,
          illegals,
          includeFormWithJustOneClause = True,
          includeFormWithJustOneLiteralPerClause = False,
          maxStringSize,
          minStringSize,
          allowArrowOperators = False,
          printDetailedSolution = Nothing,
          extraText = Nothing
        }

timeout :: Int
timeout = 30000000 -- 30 seconds

spec :: Spec
spec = do
    describe "config" $ do
      it "default config should pass config check" $
        doesNotRefuse (checkLegalNormalFormConfig defaultLegalNormalFormConfig :: LangM Maybe)

    describe "validBoundsLegalNormalFormConfig" $
        it "produces a valid config" $
          withMaxSuccess 1000 $ forAll validBoundsLegalNormalFormConfig $ \conf ->
            ioProperty $ checkConfigWith German conf checkLegalNormalFormConfig

    describe "invalidBoundsLegalCNF" $
        xit "produces a valid config" $
          forAll invalidBoundsLegalCNF $ \conf ->
            ioProperty (not <$> checkConfigWith German conf checkLegalNormalFormConfig)

    describe "description" $ do
      it "should not reject - CNF" $
        forAll validBoundsLegalNormalFormConfig $ \config ->
          within timeout $ forAll (generateLegalCNFInst config) $ \inst ->
            doesNotRefuse (IllegalCnfs.description False inst :: LangM Maybe)
      it "should not reject - DNF" $
        forAll validBoundsLegalNormalFormConfig $ \config ->
          within timeout $ forAll (generateLegalDNFInst config) $ \inst ->
            doesNotRefuse (IllegalDnfs.description False inst :: LangM Maybe)

    describe "genIllegalCnfSynTree" $
        it "the syntax Tree are not CNF syntax tree" $
            forAll validBoundsLegalNormalFormConfig $
              \LegalNormalFormConfig {normalFormConfig = NormalFormConfig{baseConf = BaseConfig{..}, ..}, ..} ->
                forAll
                  (genIllegalCnfSynTree
                    (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength)
                     usedAtoms
                     allowArrowOperators
                  )
                  (not . judgeCnfSynTree . fst)
    describe "genIllegalDnfSynTree" $
        it "the syntax Tree are not DNF syntax tree" $
            forAll validBoundsLegalNormalFormConfig $
              \LegalNormalFormConfig {normalFormConfig = NormalFormConfig{baseConf = BaseConfig{..}, ..}, ..} ->
                forAll
                  (genIllegalDnfSynTree
                    (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength)
                     usedAtoms
                     allowArrowOperators
                  )
                  (not . judgeDnfSynTree . fst)
    describe "judgeCnfSynTree" $
        it "is reasonably implemented" $
            forAll validBoundsLegalNormalFormConfig $
              \LegalNormalFormConfig {normalFormConfig = NormalFormConfig{baseConf = BaseConfig{..}, ..}} ->
                forAll
                  (genCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedAtoms False)
                  (judgeCnfSynTree . cnfToSynTree)
    describe "judgeDnfSynTree" $
        it "is reasonably implemented" $
            forAll validBoundsLegalNormalFormConfig $
              \LegalNormalFormConfig {normalFormConfig = NormalFormConfig{baseConf = BaseConfig{..}, ..}} ->
                forAll
                  (genDnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedAtoms False)
                  (judgeDnfSynTree . dnfToSynTree)
    describe "generateLegalCNFInst" $ do
        it "all of the formulas in the wrong serial should not be Cnf" $
            forAll validBoundsLegalNormalFormConfig $ \config ->
                within timeout $ forAll (generateLegalCNFInst config) $ \LegalNormalFormInst{..} ->
                  let formulaStrings = map thd3 formulaInfos in
                  all (\x -> isLeft (cnfParse (formulaStrings !! (x - 1)))) [i | (i, info,_) <- formulaInfos, treeIsErroneous info]
        it "all of the formulas not in the wrong serial should be Cnf" $
            forAll validBoundsLegalNormalFormConfig $ \config@LegalNormalFormConfig{..} ->
                within timeout $ forAll (generateLegalCNFInst config) $ \LegalNormalFormInst{..} ->
                  let formulaStrings = map thd3 formulaInfos in
                  all
                    (\x -> isRight (cnfParse (formulaStrings !! (x - 1))))
                    ([1..formulas] \\ [i | (i, info,_) <- formulaInfos, treeIsErroneous info])
        it "should pass verifyInst" $
            forAll validBoundsLegalNormalFormConfig $ \config@LegalNormalFormConfig{..} ->
                within timeout $ forAll (generateLegalCNFInst config) $ \inst ->
                  doesNotRefuse (IllegalCnfs.verifyInst inst :: LangM Maybe)
        it "should pass grading with correct answer" $
          forAll validBoundsLegalNormalFormConfig $ \config@LegalNormalFormConfig{..} ->
            within timeout $ forAll (generateLegalCNFInst config) $ \inst@LegalNormalFormInst{..} ->
              doesNotRefuse (
                IllegalCnfs.partialGrade
                  inst
                  ([i | (i, info,_) <- formulaInfos, not $ treeIsErroneous info])
                :: LangM Maybe) &&
              doesNotRefuse (
                IllegalCnfs.completeGrade
                  inst ([i | (i, info,_) <- formulaInfos, not $ treeIsErroneous info])
                :: Rated Maybe)
    describe "generateLegalDNFInst" $ do
        it "all of the formulas in the wrong serial should not be Dnf" $
            forAll validBoundsLegalNormalFormConfig $ \config ->
                within timeout $ forAll (generateLegalDNFInst config) $ \LegalNormalFormInst{..} ->
                  let formulaStrings = map thd3 formulaInfos in
                  all (\x -> isLeft (dnfParse (formulaStrings !! (x - 1)))) [i | (i, info,_) <- formulaInfos, treeIsErroneous info]
        it "all of the formulas not in the wrong serial should be Dnf" $
            forAll validBoundsLegalNormalFormConfig $ \config@LegalNormalFormConfig{..} ->
                within timeout $ forAll (generateLegalDNFInst config) $ \LegalNormalFormInst{..} ->
                  let formulaStrings = map thd3 formulaInfos in
                  all
                    (\x -> isRight (dnfParse (formulaStrings !! (x - 1))))
                    ([1..formulas] \\ [i | (i, info,_) <- formulaInfos, treeIsErroneous info])
        it "should pass verifyInst" $
            forAll validBoundsLegalNormalFormConfig $ \config@LegalNormalFormConfig{..} ->
                within timeout $ forAll (generateLegalDNFInst config) $ \inst ->
                  doesNotRefuse (IllegalDnfs.verifyInst inst :: LangM Maybe)
        it "should pass grading with correct answer" $
          forAll validBoundsLegalNormalFormConfig $ \config@LegalNormalFormConfig{..} ->
            within timeout $ forAll (generateLegalDNFInst config) $ \inst@LegalNormalFormInst{..} ->
              doesNotRefuse (
                IllegalDnfs.partialGrade
                  inst
                  ([i | (i, info,_) <- formulaInfos, not $ treeIsErroneous info])
                :: LangM Maybe) &&
              doesNotRefuse (
                IllegalDnfs.completeGrade
                  inst
                  ([i | (i, info,_) <- formulaInfos, not $ treeIsErroneous info])
                :: Rated Maybe)

judgeCnfSynTree :: SynTree BinOp a -> Bool
judgeCnfSynTree (Binary And a b) = judgeCnfSynTree a && judgeCnfSynTree b
judgeCnfSynTree (Binary Or a b) =  judgeCnfOr a && judgeCnfOr b
judgeCnfSynTree (Not a) = judgeLeaf a
judgeCnfSynTree (Leaf _) = True
judgeCnfSynTree _ = False

judgeDnfSynTree :: SynTree BinOp a -> Bool
judgeDnfSynTree (Binary Or a b) = judgeDnfSynTree a && judgeDnfSynTree b
judgeDnfSynTree (Binary And a b) =  judgeDnfAnd a && judgeDnfAnd b
judgeDnfSynTree (Not a) = judgeLeaf a
judgeDnfSynTree (Leaf _) = True
judgeDnfSynTree _ = False

judgeCnfOr :: SynTree BinOp a -> Bool
judgeCnfOr (Binary Or a b) =  judgeCnfOr a && judgeCnfOr b
judgeCnfOr (Not a) = judgeLeaf a
judgeCnfOr (Leaf _) = True
judgeCnfOr _ = False

judgeDnfAnd :: SynTree BinOp a -> Bool
judgeDnfAnd (Binary And a b) =  judgeDnfAnd a && judgeDnfAnd b
judgeDnfAnd (Not a) = judgeLeaf a
judgeDnfAnd (Leaf _) = True
judgeDnfAnd _ = False

judgeLeaf :: SynTree o a -> Bool
judgeLeaf (Leaf _) = True
judgeLeaf _ = False

cnfParse :: String -> Either ParseError Cnf
cnfParse = parse (fully parser) ""

dnfParse :: String -> Either ParseError Dnf
dnfParse = parse (fully parser) ""
