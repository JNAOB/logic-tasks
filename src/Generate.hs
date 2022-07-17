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
minDepthForNodes nodes = head [ depth | depth <- [1..], maxNodesForDepth depth >= nodes ]

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
genSynTree (minNode, maxNode) maxDepth availableLetters atLeastOccurring useImplEqui = do
    nodeNum <- choose (minNode, maxNode)
    sample <- syntaxShape nodeNum maxDepth useImplEqui `suchThat` \synTree -> fromIntegral (length (collectLeaves synTree)) >= atLeastOccurring
    usedList <- randomList availableLetters (take (fromIntegral atLeastOccurring) availableLetters) $ fromIntegral $ length $ collectLeaves sample
    return (relabelShape sample usedList )

syntaxShape :: Integer -> Integer -> Bool -> Gen (SynTree ())
syntaxShape nodeNum maxDepth useImplEqui
    | nodeNum == 1 = positiveLiteral
    | nodeNum == 2 = negativeLiteral
    | maxNodesForDepth (maxDepth - 1) < nodeNum - 1 = oneof binaryOper
    | otherwise = oneof $ negativeForm : binaryOper
    where
        binaryOper = map (binaryOperator nodeNum maxDepth useImplEqui) $ chooseList useImplEqui
        negativeForm = negativeFormula nodeNum maxDepth useImplEqui

binaryOperator :: Integer -> Integer -> Bool -> (SynTree () -> SynTree () -> SynTree ()) -> Gen (SynTree ())
binaryOperator nodeNum maxDepth useImplEqui operator =
    let minNodesPerSide = max 1 (restNodes - maxNodesForDepth newMaxDepth)
        restNodes = nodeNum - 1
        newMaxDepth = maxDepth - 1
    in  do
        leftNodeNum <- choose (minNodesPerSide , restNodes - minNodesPerSide)
        leftTree <- syntaxShape leftNodeNum newMaxDepth useImplEqui
        rightTree <- syntaxShape (restNodes - leftNodeNum ) newMaxDepth useImplEqui
        return (operator leftTree rightTree)

negativeFormula :: Integer -> Integer -> Bool -> Gen (SynTree ())
negativeFormula nodeNum maxDepth useImplEqui =
    let restNodes = nodeNum - 1
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
genSynTreeSubtreeExc (minNode, maxNode) maxDepth availableLetters atLeastOccurring useImplEqui useDupelTree minSubtreeNum =
    let
        syntaxTree = if not useDupelTree && minSubtreeNum > minNode
            then  genSynTree (minSubtreeNum, maxNode) maxDepth availableLetters atLeastOccurring useImplEqui
            else  genSynTree (minNode, maxNode) maxDepth availableLetters atLeastOccurring useImplEqui
    in
        syntaxTree `suchThat` \synTree -> (noSameSubTree synTree || useDupelTree) && size (allSubtree synTree) >= fromIntegral minSubtreeNum
