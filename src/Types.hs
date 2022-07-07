{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

module Types
 (
 SynTree(..),
 collectLeaves,
 relabelShape,
 allsubtre
 )
where

import Data.List (sort)
import Control.Monad.State (get, put, runState)

data SynTree c
  = And {lefttree :: SynTree c, righttree :: SynTree c}
  | Or {lefttree :: SynTree c, righttree :: SynTree c}
  | Impl {lefttree :: SynTree c, righttree :: SynTree c}
  | Equi {lefttree :: SynTree c, righttree :: SynTree c}
  | Not {folltree :: SynTree c}
  | Leaf {leaf :: c}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

collectLeaves :: SynTree c -> [c]
collectLeaves = foldMap (:[])

relabelShape :: SynTree () -> [c] -> SynTree c
relabelShape t l = let (r,[]) = runState (traverse adorn t) l
                   in r
  where
    adorn _ = do {ys <- get; put (tail ys); return (head ys)}

gitSubTree :: SynTree c -> [SynTree c]
gitSubTree (And a b) = gitSubTree a ++ (And a b:gitSubTree b)
gitSubTree (Leaf a)=  [Leaf a]
gitSubTree (Or a b) = gitSubTree a ++ (Or a b:gitSubTree b)
gitSubTree (Not a) = Not a:gitSubTree a
gitSubTree (Impl a b) =gitSubTree a ++ (Impl a b:gitSubTree b)
gitSubTree (Equi a b) = gitSubTree a ++ (Equi a b:gitSubTree b)

allsubtre:: Ord c => SynTree c -> [SynTree c]
allsubtre a = sort $ gitSubTree a
