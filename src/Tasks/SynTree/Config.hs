{-# LANGUAGE RecordWildCards #-}

module Tasks.SynTree.Config (
  SynTreeConfig(..),
  SynTreeInst(..),
  checkSynTreeConfig,
  dSynTreeConfig,
  ) where

import Types (SynTree)
import Generate (rangeDepthForNodes, maxLeavesForNodes)

data SynTreeConfig =
  SynTreeConfig
  { maxnode :: Integer
  , minnode :: Integer
  , maxdepth :: Integer
  , electliteral :: String
  , mustcontain :: Integer
  ,addoper::Bool
  } deriving Show

checkSynTreeConfig :: SynTreeConfig -> Maybe String
checkSynTreeConfig SynTreeConfig{..}
    | minnode < 1
      = Just "Minimal number of nodes must be positive."
    | maxnode < minnode
      = Just "Maximal number of nodes must not be smaller than minimal number."
    | maxdepth < 1
      = Just "Non-positive depth makes no sense."
    | mustcontain < 1
      = Just "At least one literal occurs in each formula."
    | fromIntegral (length electliteral) < mustcontain
      = Just "You have provided too few literals."
    | fst (rangeDepthForNodes minnode) > maxdepth
      = Just "The number of nodes generated at the specific depth is less than the minimum number of nodes"
    | maxLeavesForNodes maxnode < mustcontain
      = Just "The syntax tree generated by the maximum number of nodes cannot contain too many must contain Literals"
    | 2 ^ (maxdepth - 1) < mustcontain
      = Just "The syntax tree with maximum depth cannot contain too many must contain Literals"
    | otherwise = Nothing

dSynTreeConfig :: SynTreeConfig
dSynTreeConfig =
  SynTreeConfig
  { maxnode = 10
  , minnode = 6
  , maxdepth = 6
  , electliteral = "ABCDE"
  , mustcontain = 3
  ,addoper = False
  }

data SynTreeInst =
  SynTreeInst
  { insSyntree :: SynTree Char
  , image :: String
  , correct :: String
  } deriving Show