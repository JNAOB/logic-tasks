{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tasks.SubTree.Config (
    SubTreeInst(..),
    SubTreeConfig(..),
    defaultSubTreeConfig,
    checkSubTreeConfig
    ) where


import Control.OutputCapable.Blocks (LangM, Language, OutputCapable, english, german)
import Data.Set (Set)
import GHC.Generics (Generic)
import Data.Map (Map)

import LogicTasks.Helpers (reject)
import Tasks.SynTree.Config(SynTreeConfig(..), checkSynTreeConfig, defaultSynTreeConfig)
import Trees.Helpers (maxLeavesForNodes)
import Trees.Types (BinOp, SynTree)




data SubTreeConfig =
  SubTreeConfig
    {
      syntaxTreeConfig :: SynTreeConfig
    , allowSameSubTree :: Bool
    , subTreeAmount :: Integer
    , extraText :: Maybe (Map Language String)
    , printSolution :: Bool
    , offerUnicodeInput :: Bool
    } deriving (Show,Generic)



defaultSubTreeConfig :: SubTreeConfig
defaultSubTreeConfig =
    SubTreeConfig
    { syntaxTreeConfig = defaultSynTreeConfig
    , allowSameSubTree = True
    , subTreeAmount = 3
    , extraText = Nothing
    , printSolution = False
    , offerUnicodeInput = False
    }



checkSubTreeConfig :: OutputCapable m => SubTreeConfig -> LangM m
checkSubTreeConfig subConfig@SubTreeConfig {..} =
    checkSynTreeConfig syntaxTreeConfig *> checkAdditionalConfig subConfig



checkAdditionalConfig :: OutputCapable m => SubTreeConfig -> LangM m
checkAdditionalConfig SubTreeConfig {syntaxTreeConfig = SynTreeConfig {..}, subTreeAmount}
    | subTreeAmount < 2 = reject $ do
        english "The task makes no sense if not at least two subtrees are generated."
        german "Es müssen mindestens zwei Unterbäume erzeugt werden."
    | minNodes - maxLeavesForNodes minNodes < subTreeAmount = reject $ do
        english "These settings do not allow for enough non-atomic subtrees."
        german "Mit diesen Einstellungen können nicht genügend nicht-triviale Unterbäume erzeugt werden."
    | otherwise = pure()



data SubTreeInst =
    SubTreeInst
    { tree :: SynTree BinOp Char
    , correctTrees :: Set (SynTree BinOp Char)
    , inputTreeAmount :: Integer
    , showArrowOperators :: Bool
    , showSolution :: Bool
    , addText :: Maybe (Map Language String)
    , unicodeAllowed :: Bool
    } deriving (Show,Generic)
