{-# OPTIONS_GHC -Wwarn=x-partial #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module SynTreeSpec (spec, validBoundsSynTreeConfig) where

import Test.Hspec (Spec, describe, it, xit)
import Test.QuickCheck (Gen, chooseInteger, elements, forAll, suchThat, sublistOf)
import Data.List.Extra (nubOrd, isInfixOf)

import TestHelpers (deleteSpaces, doesNotRefuse)
import Trees.Print (display)
import Trees.Parsing (formulaParse)
import Tasks.SynTree.Config (
  SynTreeConfig (..),
  checkSynTreeConfig,
  defaultSynTreeConfig)
import Trees.Helpers (
  collectLeaves,
  treeDepth,
  treeNodes,
  maxNodesForDepth,
  numOfUniqueBinOpsInSynTree,
  maxDepthForNodes,
  collectUniqueBinOpsInSynTree)
import SAT.MiniSat hiding (Formula(Not))
import qualified SAT.MiniSat as Sat (Formula(Not))
import Trees.Types (SynTree(..), BinOp(..))
import LogicTasks.Formula (ToSAT(convert), isSemanticEqual)
import Trees.Generate (genSynTree)
import Control.OutputCapable.Blocks (LangM)
import Data.Map (Map)
import qualified Data.Map as Map (fromList, filter)

opFrequencies :: Map BinOp Int
opFrequencies = Map.fromList
  [ (And, 1)
  , (Or, 1)
  , (Impl, 1)
  , (BackImpl, 1)
  , (Equi, 1)
  ]

opFrequenciesNoArrows :: Map BinOp Int
opFrequenciesNoArrows = Map.fromList
  [ (And, 1)
  , (Or, 1)
  , (Impl, 0)
  , (BackImpl, 0)
  , (Equi, 0)
  ]

validBoundsSynTreeConfig :: Gen SynTreeConfig
validBoundsSynTreeConfig = do
  binOpFrequencies <- elements [opFrequencies, opFrequenciesNoArrows]
  maxConsecutiveNegations <- chooseInteger (0, 3)
  availableAtoms <- sublistOf ['A' .. 'Z'] `suchThat` (not . null)
  minAmountOfUniqueAtoms <- chooseInteger (1, fromIntegral $ length availableAtoms)
  minNodes <- chooseInteger (max 3 (minAmountOfUniqueAtoms * 2), 60) `suchThat` \minNodes' -> maxConsecutiveNegations /= 0 || odd minNodes'
  let minDepth = 1 + floor (logBase (2 :: Double) $ fromIntegral minNodes)
  let minMaxDepth = max (maxConsecutiveNegations + 1) minDepth
  maxDepth <- chooseInteger (minMaxDepth,max (minMaxDepth+ 3) 10)
  maxNodes <- chooseInteger (minNodes, maxNodesForDepth maxDepth) `suchThat`
    \maxNodes' -> (maxConsecutiveNegations /= 0 || odd maxNodes')
      && maxDepth <= maxDepthForNodes maxConsecutiveNegations maxNodes'
  let availableBinOpsCount = fromIntegral $ length $ Map.filter (>0) binOpFrequencies
  minUniqueBinOperators <- chooseInteger (1, min availableBinOpsCount ((minNodes - 1) `div` 2))
  return $ SynTreeConfig {
    maxNodes,
    minNodes,
    minDepth,
    maxDepth,
    availableAtoms,
    minAmountOfUniqueAtoms,
    binOpFrequencies,
    negOpFrequency = min (fromIntegral maxConsecutiveNegations) 1,
    maxConsecutiveNegations,
    minUniqueBinOperators
  }


spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (checkSynTreeConfig defaultSynTreeConfig :: LangM Maybe)
    it "validBoundsSynTreeConfig should generate a valid config" $
      forAll validBoundsSynTreeConfig $ \synTreeConfig ->
        doesNotRefuse (checkSynTreeConfig synTreeConfig :: LangM Maybe)
  describe "feedback" $
    it "rejects nonsense" $
      forAll validBoundsSynTreeConfig $ \synTreeConfig@SynTreeConfig {..} ->
        forAll (genSynTree synTreeConfig) $ \tree -> formulaParse (tail (display tree)) /= Right tree
  describe "numOfUniqueBinOpsInSynTree" $ do
        it "should return 0 if there is only a leaf" $
            numOfUniqueBinOpsInSynTree @BinOp (Leaf 'a') == 0
        it "should return 1 if there is only one operator" $
            numOfUniqueBinOpsInSynTree (Binary Or (Leaf 'a') (Leaf 'b')) == 1
        it "should return 1 if there are two operators of same kind" $
            numOfUniqueBinOpsInSynTree (Binary Or (Leaf 'a') (Not (Binary Or (Leaf 'a') (Leaf 'c')))) == 1
        it "should return 2 if there are two unique operators" $
            let subtree = Binary And (Leaf 'a') in
            numOfUniqueBinOpsInSynTree (Binary Or (Leaf 'a') (Not (subtree (subtree (Leaf 'c'))))) == 2
  describe "genSynTree" $ do
    it "should generate a random SyntaxTree that satisfies the required amount of unique binary operators" $
      forAll validBoundsSynTreeConfig $ \synTreeConfig@SynTreeConfig {..} ->
        forAll (genSynTree synTreeConfig) $ \synTree -> numOfUniqueBinOpsInSynTree synTree >= minUniqueBinOperators
  describe "genSyntaxTree" $ do
    it "should generate a random SyntaxTree from the given parament and can be parsed by formulaParse" $
      forAll validBoundsSynTreeConfig $ \synTreeConfig@SynTreeConfig {..} ->
        forAll (genSynTree synTreeConfig) $ \tree -> formulaParse (display tree) == Right tree
    xit ("should generate a random SyntaxTree from the given parament and can be parsed by formulaParse, " ++
        "even without spaces") $
      forAll validBoundsSynTreeConfig $ \synTreeConfig@SynTreeConfig {..} ->
        forAll (genSynTree synTreeConfig) $ \tree -> formulaParse (deleteSpaces (display tree)) == Right tree
    it "should generate a random SyntaxTree from the given parament and in the node area" $
      forAll validBoundsSynTreeConfig $ \synTreeConfig@SynTreeConfig {..} ->
        forAll (genSynTree synTreeConfig) $ \tree -> treeNodes tree >= minNodes && treeNodes tree <= maxNodes
    it "should generate a random SyntaxTree from the given parament and not deeper than the maxDepth" $
      forAll validBoundsSynTreeConfig $ \synTreeConfig@SynTreeConfig {..} ->
        forAll (genSynTree synTreeConfig) $ \tree -> treeDepth tree <= maxDepth
    it "should generate a random SyntaxTree from the given parament and use as many chars as it must use" $
      forAll validBoundsSynTreeConfig $ \synTreeConfig@SynTreeConfig {..} ->
        forAll (genSynTree synTreeConfig) $ \tree -> fromIntegral (length (nubOrd (collectLeaves tree))) >= minAmountOfUniqueAtoms
    it "should generate a random SyntaxTree with limited ConsecutiveNegations" $
      forAll validBoundsSynTreeConfig $ \synTreeConfig@SynTreeConfig {..} ->
        forAll (genSynTree synTreeConfig) $ \tree -> not (replicate (fromIntegral maxConsecutiveNegations + 1) '~'
                    `isInfixOf` deleteSpaces (display tree))
    it "should generate a random SyntaxTree with fixed nodes and depth" $
      forAll (validBoundsSynTreeConfig `suchThat` \cfg -> minNodes cfg == maxNodes cfg && minDepth cfg == maxDepth cfg) $
        \synTreeConfig@SynTreeConfig {..} -> forAll (genSynTree synTreeConfig) $ \synTree ->
            treeDepth synTree == maxDepth && treeNodes synTree == maxNodes
    it "should respect operator frequencies" $
       forAll (validBoundsSynTreeConfig `suchThat` ((== opFrequenciesNoArrows) . binOpFrequencies)) $ \synTreeConfig ->
        forAll (genSynTree synTreeConfig) $ \tree ->
          any  (`notElem` collectUniqueBinOpsInSynTree tree) [Impl, BackImpl, Equi]

  describe "ToSAT instance" $ do
    it "should correctly convert Leaf" $
      convert @(SynTree BinOp Char) (Leaf 'A') == Var 'A'
    it "should correctly convert Not" $ do
      convert @(SynTree BinOp Char) (Not (Leaf 'A')) == Sat.Not (Var 'A') &&
        convert (Not (Binary And (Binary Impl (Leaf 'A') (Not (Leaf 'B'))) (Leaf 'C')))
          == Sat.Not((Var 'A' :->: Sat.Not (Var 'B')) :&&: Var 'C')
    it "should correctly convert Binary" $
      let orTree = Binary Or (Leaf 'C') (Leaf 'D') in
      convert (Binary And (Leaf 'A') (Leaf 'B')) == (Var 'A' :&&: Var 'B') &&
      convert orTree == (Var 'C' :||: Var 'D') &&
      convert (Binary Impl (Leaf 'A') (Leaf 'B')) == (Var 'A' :->: Var 'B') &&
      convert (Binary Equi (Leaf 'A') (Leaf 'B')) == (Var 'A' :<->: Var 'B') &&
      convert (Binary And (Binary Impl (Leaf 'A') (Not (Leaf 'B'))) (Binary Equi orTree (Leaf 'E')))
        == (Var 'A' :->: Sat.Not (Var 'B')) :&&: ((Var 'C' :||: Var 'D') :<->: Var 'E')

  describe "semantic equivalence of syntax trees (isSemanticEqual)" $  do
    it "a syntax tree's formula is semantically equivalent to itself" $
      forAll validBoundsSynTreeConfig $ \synTreeConfig@SynTreeConfig {..} ->
        forAll (genSynTree synTreeConfig) $ \tree -> isSemanticEqual tree tree
    it "a syntax tree's formula is semantically equivalent to itself with associativity applied" $ do
      isSemanticEqual
        ((Leaf 'A' `treeAnd` Leaf 'B') `treeAnd` Leaf 'C')
        (Leaf 'A' `treeAnd` (Leaf 'B' `treeAnd` Leaf 'C')) &&
        isSemanticEqual
          ((Leaf 'A' `treeOr` Leaf 'B') `treeOr` Leaf 'C')
          (Leaf 'A' `treeOr` (Leaf 'B' `treeOr` Leaf 'C'))
    it "a syntax tree's formula is semantically equivalent to itself with commutativity applied" $ do
      isSemanticEqual (Leaf 'A' `treeAnd` Leaf 'B') (Leaf 'B' `treeAnd` Leaf 'A') &&
        isSemanticEqual (Leaf 'A' `treeOr` Leaf 'B') (Leaf 'B' `treeOr` Leaf 'A') &&
          isSemanticEqual (Leaf 'A' `treeBiImpl` Leaf 'B') (Leaf 'B' `treeBiImpl` Leaf 'A')
    it "a syntax tree's formula is semantically equivalent to itself with distributivity applied" $ do
      isSemanticEqual
        ((Leaf 'A' `treeAnd` Leaf 'B') `treeOr` Leaf 'C')
        ((Leaf 'A' `treeOr` Leaf 'C') `treeAnd` (Leaf 'B' `treeOr` Leaf 'C')) &&
        isSemanticEqual
          ((Leaf 'A' `treeOr` Leaf 'B') `treeAnd` Leaf 'C')
          ((Leaf 'A' `treeAnd` Leaf 'C') `treeOr` (Leaf 'B' `treeAnd` Leaf 'C'))

-- shorthands
treeAnd, treeOr, treeBiImpl :: SynTree BinOp a -> SynTree BinOp a -> SynTree BinOp a
treeAnd = Binary And
treeOr = Binary Or
treeBiImpl = Binary Equi
