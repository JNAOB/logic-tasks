module Generate(
 minDepthForNodes,
 genSynTree,
 maxLeavesForNodes,
 maxNodesForDepth,
 genSynTreeSubtreeExc,
 noSameSubTree,
) where

import Test.QuickCheck (choose, Gen, oneof, shuffle, suchThat, elements)
import Data.List.Extra (nubOrd)
import Data.Set (size)
import Test.QuickCheck.Gen (vectorOf)

import Types (SynTree(..), collectLeaves, relabelShape, allSubtree)

minDepthForNodes :: Integer -> Integer
minDepthForNodes nodes = ceiling (logBase 2 (fromIntegral (nodes + 1) :: Float))

maxNodesForDepth :: Integer -> Integer
maxNodesForDepth depth = 2 ^ depth - 1

maxLeavesForNodes :: Integer -> Integer
maxLeavesForNodes nodes = (nodes + 1) `div` 2

chooseList :: Bool -> [SynTree c -> SynTree c -> SynTree c]
chooseList useImplEqui = if useImplEqui
        then [And, Or, Impl, Equi]
        else [And, Or]

randomList :: [c] -> [c] -> Integer -> Gen [c]
randomList availableLetters atLeastOccurring len = let
    restLength = fromIntegral len - length atLeastOccurring
    in do
        randomRest <- vectorOf restLength (elements availableLetters)
        shuffle (atLeastOccurring ++ randomRest)

genSynTree :: (Integer, Integer) -> Integer -> String -> Integer -> Bool -> Gen (SynTree Char)
genSynTree (minNodes, maxNodes) maxDepth availableLetters atLeastOccurring useImplEqui = do
    nodesNum <- choose (minNodes, maxNodes)
    sample <- syntaxShape nodesNum maxDepth useImplEqui `suchThat` \synTree -> fromIntegral (length (collectLeaves synTree)) >= atLeastOccurring
    usedList <- randomList availableLetters (take (fromIntegral atLeastOccurring) availableLetters) $ fromIntegral $ length $ collectLeaves sample
    return (relabelShape sample usedList )

syntaxShape :: Integer -> Integer -> Bool -> Gen (SynTree ())
syntaxShape nodesNum maxDepth useImplEqui
    | nodesNum == 1 = positiveLiteral
    | nodesNum == 2 = negativeLiteral
    | maxNodesForDepth (maxDepth - 1) < nodesNum - 1 = oneof binaryOper
    | otherwise = oneof $ negativeForm : binaryOper
    where
        binaryOper = map (binaryOperator nodesNum maxDepth useImplEqui) $ chooseList useImplEqui
        negativeForm = negativeFormula nodesNum maxDepth useImplEqui

binaryOperator :: Integer -> Integer -> Bool -> (SynTree () -> SynTree () -> SynTree ()) -> Gen (SynTree ())
binaryOperator nodesNum maxDepth useImplEqui operator =
    let minNodesPerSide = max 1 (restNodes - maxNodesForDepth newMaxDepth)
        restNodes = nodesNum - 1
        newMaxDepth = maxDepth - 1
    in  do
        leftNodesNum <- choose (minNodesPerSide , restNodes - minNodesPerSide)
        leftTree <- syntaxShape leftNodesNum newMaxDepth useImplEqui
        rightTree <- syntaxShape (restNodes - leftNodesNum ) newMaxDepth useImplEqui
        return (operator leftTree rightTree)

negativeFormula :: Integer -> Integer -> Bool -> Gen (SynTree ())
negativeFormula nodesNum maxDepth useImplEqui =
    let restNodes = nodesNum - 1
        newMaxDepth = maxDepth - 1
    in  do
        e <- syntaxShape restNodes newMaxDepth useImplEqui
        return (Not e)

negativeLiteral ::  Gen (SynTree ())
negativeLiteral = Not <$> positiveLiteral

positiveLiteral :: Gen (SynTree ())
positiveLiteral = return (Leaf ())

--------------------------------------------------------------------------------------------------------------
noSameSubTree :: Ord c => SynTree c -> Bool
noSameSubTree synTree = let treeList = collectLeaves synTree
    in
        treeList == nubOrd treeList
-- generate subtree exercise
genSynTreeSubtreeExc :: (Integer, Integer) -> Integer -> String -> Integer -> Bool -> Bool -> Integer -> Gen (SynTree Char)
genSynTreeSubtreeExc (minNodes, maxNodes) maxDepth availableLetters atLeastOccurring useImplEqui useDupelTree minSubtreeNum =
    let
        syntaxTree = if not useDupelTree && minSubtreeNum > minNodes
            then  genSynTree (minSubtreeNum, maxNodes) maxDepth availableLetters atLeastOccurring useImplEqui
            else  genSynTree (minNodes, maxNodes) maxDepth availableLetters atLeastOccurring useImplEqui
    in
        syntaxTree `suchThat` \synTree -> (noSameSubTree synTree || useDupelTree) && size (allSubtree synTree) >= fromIntegral minSubtreeNum
