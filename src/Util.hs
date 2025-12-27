{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}

module Util where


import Control.OutputCapable.Blocks (
  GenericOutputCapable(..),
  LangM,
  OutputCapable,
  english,
  german,
  translate,
  yesNo,
  )
import Control.Monad (when)
import Data.List (delete)
import Test.QuickCheck(Gen, elements, suchThat)

import Config (BaseConfig(..), NormalFormConfig(..), FormulaConfig (..), FormulaInst (..))
import Formula.Types (Formula (atomics), getTable, lengthBound)
import Formula.Table (readEntries)
import Tasks.SynTree.Config (SynTreeConfig (availableAtoms), checkSynTreeConfig)
import Formula.Util (cnfDependsOnAllAtomics, dnfDependsOnAllAtomics)
import Trees.Helpers (synTreeDependsOnAllAtomics)


prevent :: OutputCapable m => Bool -> LangM m -> LangM m
prevent b = assertion $ not b



preventWithHint :: OutputCapable m => Bool -> LangM m -> LangM m -> LangM m
preventWithHint b desc hint = do
  yesNo (not b) desc
  when b (refuse $ indent hint)
  pure ()

printWithHint :: OutputCapable m => Bool -> LangM m -> LangM m -> LangM m
printWithHint b desc hint = do
  yesNo (not b) desc
  when b (indent hint)
  pure ()


pairwiseCheck :: Eq a => [(a,a,Int)] -> ([Int],[Int])
pairwiseCheck [] = ([],[])
pairwiseCheck ((x,y,index):xs)
    | x == y = (index:same,diff)
    | otherwise = (same,index:diff)
  where (same,diff) = pairwiseCheck xs



isOutside :: Int -> Int -> Int -> Bool
isOutside lower upper x = x < lower || x > upper



remove :: Int -> [Int] -> Gen [Int]
remove _ [] = pure []
remove 0 xs = pure xs
remove num xs = do
    out <- elements xs
    remove (num-1) $ delete out xs



withRatio :: Formula a => (Int,Int) -> a -> Bool
withRatio (0, 100) _ = True
withRatio (lower, upper) form =
    lengthTrueEntries <= max (min 1 upper) (percentage upper)
        && lengthTrueEntries >= max (min 1 lower) (percentage lower)
  where
    tableEntries = readEntries (getTable form)
    trueEntries = filter (== Just True) tableEntries
    percentage :: Int -> Int
    percentage = let totalEntries = 2 ^ length (atomics form) in \num -> totalEntries * num `div` 100
    lengthTrueEntries = length trueEntries



checkTruthValueRange :: OutputCapable m => (Int, Int) -> FormulaConfig -> LangM m
checkTruthValueRange (low,high) formulaConfig
    | isOutside 0 100 low || isOutside 0 100 high =
        refuse $ indent $ translate $ do
          german "Die Beschränkung der Wahr-Einträge liegt nicht zwischen 0 und 100 Prozent."
          english "The given restriction on true entries are not in the range of 0 to 100 percent."

    | low > high =
        refuse $ indent $ translate $ do
          german "Die Beschränkung der Wahr-Einträge liefert keine gültige Reichweite."
          english "The given restriction on true entries are not a valid range."

    | low == high =
        refuse $ indent $ translate $ do
          german "Die Beschränkung der Wahr-Einträge sollte ein gewissen Spielraum zulassen."
          english "The given restriction on true entries should allow for some flexibility."

    | checkRangeTooSmall =
        refuse $ indent $ translate $ do
          german "Die Beschränkung der Wahr-Einträge sollte mindestens eine Ausprägung ermöglichen."
          english "The restriction of true entries should allow for at least one value."

    | otherwise = pure ()
    where
      checkRangeTooSmall = case formulaConfig of
        FormulaCnf normalFormConfig -> let
          atomsAmount = length (usedAtoms (baseConf normalFormConfig))
          in
            checkRangeTooSmall' atomsAmount
        FormulaDnf normalFormConfig -> let
          atomsAmount = length (usedAtoms (baseConf normalFormConfig))
          in
            checkRangeTooSmall' atomsAmount
        FormulaArbitrary synTreeConf -> let
          atomsAmount = length (availableAtoms synTreeConf)
          in
            checkRangeTooSmall' atomsAmount
      checkRangeTooSmall' atomsAmount = (2 ^ atomsAmount * low `div` 100) + 1 > 2 ^ atomsAmount * high `div` 100



checkBaseConf :: OutputCapable m => BaseConfig -> LangM m
checkBaseConf BaseConfig{..}
    | any (<1) [minClauseLength, maxClauseLength] =
        refuse $ indent $ translate $ do
          english "At least one length parameter is negative."
          german "Mindestens eines der 'length'-Parameter ist negativ."

    | minClauseLength > maxClauseLength =
        refuse $ indent $ translate $ do
          english "The minimum clause length is greater than the maximum clause length."
          german "Die untere Grenze der Klausellänge ist höher als die obere."


    | length usedAtoms < minClauseLength =
        refuse $ indent $ translate $ do
          german "Zu wenige atomare Formeln für minimale Klausellänge."
          english "There are not enough atomic formulas available for minimal clause length."

    | length usedAtoms < maxClauseLength =
        refuse $ indent $ translate $ do
          german "Zu wenige atomare Formeln um maximale Klausellänge zu erreichen."
          english "There are not enough atomic formulas available to reach the maximal clause length."

    | null usedAtoms =
        refuse $ indent $ translate $ do
          german "Es wurden keine atomaren Formeln angegeben."
          english "You did not specify which atomic formulas should be used."

    | otherwise = pure ()



checkNormalFormConfig :: OutputCapable m => NormalFormConfig -> LangM m
checkNormalFormConfig NormalFormConfig {..}
    | any (<1) [minClauseAmount, maxClauseAmount] =
        refuse $ indent $ translate $ do
          german "Mindestens eines der 'amount'-Parameter ist negativ."
          english "At least one amount parameter is negative."

    | minClauseAmount > maxClauseAmount =
        refuse $ indent $ translate $ do
          german "Die untere Grenze der Klauselanzahl ist höher als die obere."
          english "The minimum amount of clauses is greater than the maximum amount."

    | minClauseAmount * minClauseLength baseConf < length (usedAtoms baseConf) =
        refuse $ indent $ translate $ do
          german $ unlines
            [ "Nicht immer genug Platz für alle atomaren Formeln in der Formel."
            , "(Mögliche Lösung: Eine der unteren Schranken erhöhen)"
            ]
          english $ unlines
            [ "Not always enough space in formula for all atomic formulas."
            , "(Possible solution: raise one of the lower bounds)"
            ]

    | minClauseAmount > 2 ^ length (usedAtoms baseConf) =
        refuse $ indent $ translate $ do
          german "Zu wenig atomare Formeln für gewünschte Anzahl an Klauseln."
          english "There are not enough atomic formulas for the desired number of clauses."

    | minClauseAmount > lengthBound (length (usedAtoms baseConf)) (maxClauseLength baseConf) =
        refuse $ indent $ translate $ do
          german "Zu kurze Klauseln für gewünschte Anzahl an Klauseln."
          english "Clauses are to short for the desired number of clauses."

    | otherwise = checkBaseConf baseConf

checkTruthValueRangeAndSynTreeConf :: OutputCapable m => (Int,Int) -> SynTreeConfig -> LangM m
checkTruthValueRangeAndSynTreeConf range synTreeConfig = do
  checkTruthValueRange range (FormulaArbitrary synTreeConfig)
  checkSynTreeConfig synTreeConfig
  pure ()

checkTruthValueRangeAndFormulaConf :: OutputCapable m => (Int, Int) -> FormulaConfig -> LangM m
checkTruthValueRangeAndFormulaConf range formulaConf = do
  -- checkFullRangeForSynTrees range formulaConf
  checkTruthValueRange range formulaConf
  case formulaConf of
    (FormulaCnf cnfCfg) -> checkNormalFormConfig cnfCfg
    (FormulaDnf dnfCfg) -> checkNormalFormConfig dnfCfg
    (FormulaArbitrary syntaxTreeConfig) -> checkSynTreeConfig syntaxTreeConfig
  pure ()

checkFullRangeForSynTrees :: OutputCapable m => (Int, Int) -> FormulaConfig -> LangM m
checkFullRangeForSynTrees (0, 100) (FormulaArbitrary _) = pure ()
checkFullRangeForSynTrees _ (FormulaArbitrary _) = refuse $ indent $ translate $ do
          german "Die Wahrheitswerte können bei einem Syntaxbaum nicht eingeschränkt werden. Wählen Sie (0,100)."
          english "The truth entries cannot be restricted in a syntax tree. Select (0,100)."
checkFullRangeForSynTrees _ _ = pure ()

vectorOfUniqueBy :: Int -> (a -> a -> Bool) -> Gen a -> Gen [a]
vectorOfUniqueBy 0 _ _ = pure []
vectorOfUniqueBy amount p gen = do
  xs <- vectorOfUniqueBy (amount - 1) p gen
  x <- gen `suchThat` \x' -> not (any (p x') xs)
  pure (x:xs)

formulaDependsOnAllAtoms :: FormulaInst -> Bool
formulaDependsOnAllAtoms (InstCnf cnf) = cnfDependsOnAllAtomics cnf
formulaDependsOnAllAtoms (InstDnf dnf) = dnfDependsOnAllAtomics dnf
formulaDependsOnAllAtoms (InstArbitrary tree) = synTreeDependsOnAllAtomics tree
