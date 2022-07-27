{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SubTree.Quiz(
    feedback,
    genSubTreeInst,
) where

import Test.QuickCheck (generate)
import Generate (genSynTreeSubTreeExc)
import Data.Set (Set, isSubsetOf, size, map)

import Parsing (subFormulasStringParse, subTreeStringParse)
import Tasks.SubTree.Config (SubTreeConfig(..), SubTreeInst(..), SubTreeInst)
import Print (display)
import Types (allNotLeafSubTrees, SynTree)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Text.Parsec (ParseError)


genSubTreeInst :: SubTreeConfig -> IO SubTreeInst
genSubTreeInst SubTreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..} = do
    tree <- generate (genSynTreeSubTreeExc (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui allowDupelTree minSubTrees)
    return $ SubTreeInst
      { minInputTrees = minSubTrees
      , formula = display tree
      , correctTrees = allNotLeafSubTrees tree
      , correctFormulas = Data.Set.map display (allNotLeafSubTrees tree)
      }

feedback :: SubTreeInst -> String -> Bool
feedback SubTreeInst {correctFormulas, correctTrees, minInputTrees} input = judgeInput (subTreeStringParse input) (subFormulasStringParse (filter (/= ' ') input)) minInputTrees correctFormulas correctTrees

judgeInput :: Either ParseError (Set (SynTree Char)) -> Either ParseError (Set String) -> Integer -> Set String -> Set (SynTree Char) -> Bool
judgeInput (Right inputTreesSet) (Right inputFormulasSet) minInputTrees correctFormulas correctTrees = inputTreesSet `isSubsetOf` correctTrees && inputFormulasSet `isSubsetOf` correctFormulas && fromIntegral (size inputFormulasSet) >= minInputTrees
judgeInput _ _ _ _ _ = False
