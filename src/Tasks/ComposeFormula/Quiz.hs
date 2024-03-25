{-# LANGUAGE RecordWildCards #-}

module Tasks.ComposeFormula.Quiz(
    generateComposeFormulaInst,
    ) where


import Trees.Generate (genSynTree)
import Test.QuickCheck (Gen, suchThat,)

import Tasks.ComposeFormula.Config (ComposeFormulaConfig(..), ComposeFormulaInst(..), TreeDisplayMode (FormulaDisplay))
import Trees.Helpers (binOp, bothKids)
import Data.Maybe (fromJust)
import Trees.Print (transferToPicture)
import Trees.Types (BinOp(Equi, Or, And))




generateComposeFormulaInst :: ComposeFormulaConfig -> Gen ComposeFormulaInst
generateComposeFormulaInst ComposeFormulaConfig {..} = do
    tree <- genSynTree syntaxTreeConfig
          `suchThat` \synTree -> binOp synTree `elem` map Just [And, Or, Equi]
    let (leftTree, rightTree) = bothKids tree
    return $ ComposeFormulaInst
      { operator = fromJust $ binOp tree
      , leftTree = leftTree
      , rightTree = rightTree
      , leftTreeImage = if fst treeDisplayModes == FormulaDisplay then Nothing else Just $ transferToPicture leftTree
      , rightTreeImage = if snd treeDisplayModes == FormulaDisplay then Nothing else Just $ transferToPicture rightTree
      , addExtraHintsOnAssociativity = extraHintsOnAssociativity
      , addText = extraText
      , showSolution = printSolution
      }
