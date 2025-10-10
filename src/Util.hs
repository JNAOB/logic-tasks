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
import Test.QuickCheck(Gen, elements, suchThat, choose)

import Config (BaseConfig(..), NormalFormConfig(..), FormulaConfig (..), FormulaInst (..))
import Formula.Types (Formula, getTable, lengthBound)
import Formula.Table (readEntries)
import Tasks.SynTree.Config (SynTreeConfig(..), checkSynTreeConfig)
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
withRatio (lower,upper) form =
    length trueEntries <= max upperBound (if upper == 0 then 0 else 1)
        && length trueEntries >= max (if lower == 0 then 0 else 1) lowerBound
  where
    tableEntries = readEntries (getTable form)
    trueEntries = filter (== Just True) tableEntries
    percentage :: Int -> Int
    percentage num = length tableEntries *num `div` 100
    upperBound = percentage upper
    lowerBound = percentage lower



checkTruthValueRange :: OutputCapable m => (Int, Int) -> Int -> LangM m
checkTruthValueRange (low,high) total
    | isOutside 0 100 low || isOutside 0 100 high =
        refuse $ indent $ translate $ do
          german "Die Beschränkung der Wahr-Einträge liegt nicht zwischen 0 und 100 Prozent."
          english "The given restriction on true entries are not in the range of 0 to 100 percent."

    | low > high =
        refuse $ indent $ translate $ do
          german "Die Beschränkung der Wahr-Einträge liefert keine gültige Reichweite."
          english "The given restriction on true entries are not a valid range."

    | checkForSmallRange low high total =
        refuse $ indent $ translate $ do
          german "Die Beschränkung der Wahr-Einträge sollte ein gewissen Spielraum zulassen."
          english "The given restriction on true entries should allow for some flexibility."

    | low == high =
        refuse $ indent $ translate $ do
          german "Die Beschränkung der Wahr-Einträge sollte ein gewissen Spielraum zulassen."
          english "The given restriction on true entries should allow for some flexibility."

    | otherwise = pure ()



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
  checkTruthValueRange range (2 ^ length (availableAtoms synTreeConfig))
  checkSynTreeConfig synTreeConfig
  pure ()

checkTruthValueRangeAndFormulaConf :: OutputCapable m => (Int, Int) -> FormulaConfig -> LangM m
checkTruthValueRangeAndFormulaConf range formulaConf = do
  case formulaConf of
    (FormulaCnf cnfCfg) -> do
      checkTruthValueRange range (2 ^ length (usedAtoms (baseConf cnfCfg)))
      checkNormalFormConfig cnfCfg
      pure ()
    (FormulaDnf dnfCfg) -> do
      checkTruthValueRange range (2 ^ length (usedAtoms (baseConf dnfCfg)))
      checkNormalFormConfig dnfCfg
      pure ()
    (FormulaArbitrary syntaxTreeConfig) -> do
      checkTruthValueRange range (2 ^ length (availableAtoms syntaxTreeConfig))
      checkSynTreeConfig syntaxTreeConfig
      pure ()
  pure ()

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

checkForSmallRange :: Int -> Int -> Int -> Bool
checkForSmallRange low high total = ceiling (fromIntegral (low * total) / (100 :: Double)) >=
                                  (high * total) `div` 10

validBoundsPercentPosLiteral :: Int -> Gen (Int, Int)
validBoundsPercentPosLiteral numLiterals = do
    lowerNumberBarrier <- choose (0, numLiterals-1)
    let lowPercentBarrier = floor (((fromIntegral lowerNumberBarrier / fromIntegral numLiterals) * 100) :: Double)
    let highPercentBarrier = ceiling (((fromIntegral (lowerNumberBarrier + 1) / fromIntegral numLiterals) * 100) :: Double)

    percentPosLiteralsLow <- choose (0, lowPercentBarrier)
    percentPosLiteralsHigh <- choose (highPercentBarrier, 100)
    pure (percentPosLiteralsLow,percentPosLiteralsHigh)
