{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module SuperfluousBracketsSpec (spec) where

import Test.QuickCheck (Gen, forAll, choose, suchThat, (==>))
import Data.Either (fromRight)
import Data.List.Extra (notNull)
import Test.Hspec (Spec, describe, it)
import Text.Parsec (parse)

import Tasks.SuperfluousBrackets.Quiz (generateSuperfluousBracketsInst, feedback)
import Tasks.SuperfluousBrackets.Config(SuperfluousBracketsConfig(..), SuperfluousBracketsInst(..))
import Tasks.SynTree.Config (SynTreeConfig(..))
import SynTreeSpec (validBoundsSynTree)
import Trees.Types (SynTree(..), BinOp(..), PropFormula(..))
import Trees.Helpers (numberAllBinaryNodes, sameAssociativeOperatorAdjacent, treeNodes)
import Trees.Print (display, simplestDisplay)
import Tasks.SuperfluousBrackets.PrintSuperfluousBrackets (
  superfluousBracketsDisplay,
  sameAssociativeOperatorAdjacentSerial
  )
import Trees.Parsing(formulaParse, parsePropForm)
import TestHelpers (deleteBrackets)
import Trees.Generate (genSynTree)

validBoundsSuperfluousBrackets :: Gen SuperfluousBracketsConfig
validBoundsSuperfluousBrackets = do
    syntaxTreeConfig@SynTreeConfig {..} <- validBoundsSynTree `suchThat` ((5<=) . minNodes)
    superfluousBracketPairs <- choose (1, minNodes `div` 2)
    return $ SuperfluousBracketsConfig
        {
          syntaxTreeConfig
        , superfluousBracketPairs
        }

invalidBoundsSuperfluousBrackets :: Gen SuperfluousBracketsConfig
invalidBoundsSuperfluousBrackets = do
    syntaxTreeConfig@SynTreeConfig {..} <- validBoundsSynTree
    superfluousBracketPairs <- choose (minNodes + 1, 26)
    return $ SuperfluousBracketsConfig
        {
          syntaxTreeConfig
        , superfluousBracketPairs
        }

spec :: Spec
spec = do
    describe "sameAssociativeOperatorAdjacent" $ do
        it "should return false if there are no two \\/s or two /\\s as neighbors" $
            not $ sameAssociativeOperatorAdjacent (Binary Or (Leaf 'a') (Not (Binary Or (Leaf 'a') (Leaf 'c'))))
        it "should return true if two \\/s or two /\\s are Neighboring " $
            sameAssociativeOperatorAdjacent $
              Not $ Binary And (Binary Equi (Leaf 'a') (Leaf 'b')) (Binary And (Leaf 'a') (Leaf 'c'))
    describe "sameAssociativeOperatorAdjacent... functions" $
        it "is a consistent pair of functions" $
            forAll validBoundsSuperfluousBrackets $
              \SuperfluousBracketsConfig {syntaxTreeConfig = SynTreeConfig {..}} ->
                forAll
                  (genSynTree
                    (minNodes, maxNodes)
                    maxDepth
                    usedLiterals
                    atLeastOccurring
                    allowArrowOperators
                    maxConsecutiveNegations
                  ) $ \synTree ->
                    sameAssociativeOperatorAdjacent synTree ==>
                      notNull (sameAssociativeOperatorAdjacentSerial (numberAllBinaryNodes synTree) Nothing)
    describe "simplestDisplay and superfluousBracketsDisplay" $ do
        it "simplestDisplay should have less brackets than or equal to normal formula" $
            forAll validBoundsSuperfluousBrackets $
              \SuperfluousBracketsConfig {syntaxTreeConfig = SynTreeConfig {..}} ->
                forAll
                  (genSynTree
                    (minNodes, maxNodes)
                    maxDepth
                    usedLiterals
                    atLeastOccurring
                    allowArrowOperators
                    maxConsecutiveNegations
                  ) $ \synTree ->
                    length (sameAssociativeOperatorAdjacentSerial (numberAllBinaryNodes synTree) Nothing) *2
                      == length (display synTree) - length (simplestDisplay synTree)
        it
          ( "the number of brackets generated by simplestDisplay and display should be equal " ++
            "if 'sameAssociativeOperatorAdjacent' is not satisfied."
          ) $
            forAll validBoundsSuperfluousBrackets $
              \SuperfluousBracketsConfig {syntaxTreeConfig = SynTreeConfig {..}} ->
                forAll
                (genSynTree
                  (minNodes, maxNodes)
                  maxDepth
                  usedLiterals
                  atLeastOccurring
                  allowArrowOperators
                  maxConsecutiveNegations
                ) $
                  \synTree -> not (sameAssociativeOperatorAdjacent synTree) ==>
                    display synTree == simplestDisplay synTree
        it "after remove all bracket two strings should be same" $
            forAll validBoundsSuperfluousBrackets $ \config ->
                forAll (generateSuperfluousBracketsInst config) $ \SuperfluousBracketsInst{..} ->
                  deleteBrackets stringWithSuperfluousBrackets == deleteBrackets simplestString
    describe "valid formula" $
        it "the formula Parser can accept when brackets is max number" $
            forAll validBoundsSuperfluousBrackets $
              \SuperfluousBracketsConfig {syntaxTreeConfig = SynTreeConfig {..}} ->
                forAll
                    (genSynTree (minNodes, maxNodes)
                        maxDepth
                        usedLiterals
                        atLeastOccurring
                        allowArrowOperators
                        maxConsecutiveNegations
                      `suchThat` sameAssociativeOperatorAdjacent
                    ) $
                      \synTree -> forAll (superfluousBracketsDisplay synTree (treeNodes synTree + 1)) $
                        \stringWithSuperfluousBrackets ->
                          formulaParse stringWithSuperfluousBrackets == Right synTree
    describe "generateSuperfluousBracketsInst" $ do
        it "the correct store in Inst should be accept by feedback" $
            forAll validBoundsSuperfluousBrackets $ \superfluousBracketsConfig ->
                forAll (generateSuperfluousBracketsInst superfluousBracketsConfig) $
                  \superfluousBracketsInst@SuperfluousBracketsInst{..} ->
                    feedback superfluousBracketsInst (fromRight (Atomic ' ') (parse parsePropForm "" simplestString))
        it "the stringWithSuperfluousBrackets should have right number of SuperfluousBrackets" $
            forAll validBoundsSuperfluousBrackets $ \config@SuperfluousBracketsConfig {..} ->
                forAll (generateSuperfluousBracketsInst config) $ \SuperfluousBracketsInst{..} ->
                  fromIntegral (length stringWithSuperfluousBrackets - length simplestString)
                    == superfluousBracketPairs * 2
