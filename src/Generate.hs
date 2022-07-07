module Generate(
 rangeDepthForNodes,
 genSynTree,
 maxLeavesForNodes,
) where

import Types (SynTree(..))
import Test.QuickCheck (choose, elements, oneof, Gen, frequency)
import Data.List (isSubsequenceOf, (\\))
import Data.Maybe (isJust, fromJust)

rangeDepthForNodes :: Integer -> (Integer, Integer)
rangeDepthForNodes nodes = (minDepth, maxDepth)
  where
    minDepth = head [ depth | depth <- [1..], maxNodesForDepth depth >= nodes ]
    maxDepth = nodes

maxNodesForDepth :: Integer -> Integer
maxNodesForDepth depth = 2 ^ depth - 1

maxLeavesForNodes :: Integer -> Integer
maxLeavesForNodes nodes = (nodes + 1) `div` 2

genSynTree :: (Integer , Integer) -> Integer ->String->String ->Bool-> Maybe (Gen SynTree)
genSynTree (minnode, maxnode) maxdepth lits minuse addoper
    | maxdepth <= 0 || maxnode<=0 || null lits || maxnode < minnode || fst (rangeDepthForNodes minnode) > maxdepth || not (isSubsequenceOf minuse lits) || maxLeavesForNodes maxnode < toInteger (length minuse) = Nothing
    | otherwise =  Just $ syntaxTree (a,maxnode) maxdepth lits minuse addoper
      where
        a=maximum [0,minnode]

syntaxTree :: (Integer , Integer) -> Integer -> String -> String -> Bool -> Gen SynTree
syntaxTree (minnode, maxnode) maxdepth lits minuse addoper
    | maxdepth == 1 || maxnode == 1 = leafNd
    | minnode == 1 && maxnode == 2 = oneof [ leafNd , negativeLiteral lits minuse]
    | minnode == 2 && maxnode < 3 = negativeLiteral lits minuse
    | minnode <= 2 && maxLeavesForNodes maxnode == toInteger (length minuse) = oneof binaryOper
    | maxLeavesForNodes maxnode == toInteger (length minuse) = oneof binaryOper
    | minnode == 2 && maxnode >= 3 = frequency [(1, negativeForm), (fromInteger (maxnode - minnode - 1), oneof binaryOper)]
    | minnode == 1 && maxnode >= 3 && length minuse <= 1 = frequency [(1,leafNd), (fromInteger (maxnode - minnode - 1), oneof ( negativeForm :  binaryOper))]
    | minnode == 1 && maxnode >= 3 && length minuse > 1= oneof ( negativeForm :  binaryOper)
    | minnode - 1 > maxNodesForDepth (maxdepth - 1) = oneof binaryOper
    | otherwise = oneof ( negativeForm : binaryOper )
      where
        binaryOper = map (binaryOperator (minnode, maxnode) maxdepth lits minuse addoper ) $ chooseList addoper
        negativeForm = negativeFormula (minnode, maxnode) maxdepth lits minuse addoper
        leafNd = leafnode lits $ judgminuse minuse

chooseList :: Bool ->[SynTree -> SynTree -> SynTree]
chooseList addoper = if addoper
    then [And, Or, Impl, Equi]
    else [And, Or]

binaryOperator::(Integer , Integer) -> Integer -> String -> String ->  Bool -> (SynTree -> SynTree -> SynTree) -> Gen SynTree
binaryOperator(minnode, maxnode) maxdepth lits minuse addoper  oper =
    let avoidoveralloc = maximum [0, minnode - 2 - maxNodesForDepth (maxdepth - 1)]
        correctminnode = maximum [3,minnode]
        chooseodd = maxLeavesForNodes maxnode == toInteger (length minuse)
    in do
      radmin <- if chooseodd
        then elements (filter odd [1+avoidoveralloc..correctminnode - 2 - avoidoveralloc])
        else choose (1 + avoidoveralloc,correctminnode - 2 - avoidoveralloc)
      radmax <- if chooseodd
        then elements (filter odd [radmin..maxnode - correctminnode + radmin])
        else choose (radmin,maxnode - correctminnode + radmin)
      left <- syntaxTree (radmin, radmax) (maxdepth - 1) lits (take (fromInteger $ maxLeavesForNodes radmax) minuse) addoper
      right <- syntaxTree (correctminnode - radmin - 1, maxnode - radmax - 1) (maxdepth-1) lits (minuse \\ (take $ fromInteger (maxLeavesForNodes radmax)) minuse) addoper
      return $ oper left right

negativeFormula::(Integer , Integer) -> Integer -> String -> String -> Bool -> Gen SynTree
negativeFormula (minnode, maxnode) maxdepth lits minus addoper =
    let correctminnode = maximum [2,minnode]
    in do
      e <- syntaxTree (correctminnode - 1, maxnode - 1) (maxdepth - 1) lits minus addoper
      return (Not e)

negativeLiteral :: String -> String -> Gen SynTree
negativeLiteral lits minus = do
    e <- leafnode lits $ judgminuse minus
    return (Not e)

leafnode::String -> Maybe Char -> Gen SynTree
leafnode lits minus =
    if isJust minus then return (Leaf (fromJust minus))
    else do
        e <- elements lits
        return (Leaf e)

judgminuse :: String -> Maybe Char
judgminuse char
    |length char == 1 = Just $ head char
    |otherwise = Nothing