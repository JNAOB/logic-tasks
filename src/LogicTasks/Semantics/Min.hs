{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}

module LogicTasks.Semantics.Min where


import qualified LogicTasks.Semantics.Max as Max

import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  OutputMonad,
  english,
  german,
  translate,
  translations,
  localise,
  )
import Data.Maybe (fromMaybe)
import Test.QuickCheck (Gen)

import Config (BaseConfig(..), CnfConfig(..), MinMaxConfig(..), MinInst(..))
import Formula.Types (Dnf, Literal(..), amount, atomics, genDnf, getConjunctions, getTable)
import Formula.Util (mkCon, mkDnf, hasEmptyCon, isEmptyDnf)
import LogicTasks.Helpers (formulaKey)
import Util (tryGen, withRatio)




genMinInst :: MinMaxConfig -> Gen MinInst
genMinInst MinMaxConfig {cnfConf = CnfConfig {baseConf = BaseConfig{..},..},..} =
    MinInst <$> dnfInRange <*> pure extraText
   where
     getDnf = genDnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals
     dnfInRange = tryGen getDnf 100 $ withRatio $ fromMaybe (0,100) percentTrueEntries



description :: OutputMonad m => MinInst -> LangM m
description MinInst{..} = do
  paragraph $ do
    translate $ do
      german "Betrachten Sie die folgende Wahrheitstafel:"
      english "Consider the following truth table:"
    indent $ code $ show $ getTable dnf
    pure ()
  paragraph $ translate $ do
    german "Geben Sie eine zu der Tafel passende Formel in disjunktiver Normalform an. Verwenden Sie dazu Min-Terme."
    english "Provide a formula in disjunctive normal form, that corresponds to the table. Use minterms to do this."

  formulaKey

  paragraph $ indent $ do
    translate $ do
      let formulaStr = show $ mkDnf [mkCon [Literal 'A', Not 'B'], mkCon [Not 'C', Not 'D']]
      german $ unwords ["Ein Lösungsversuch für Formel", formulaStr, "könnte beispielsweise so aussehen: "]
      english $ unwords ["A solution attempt for the formula", formulaStr, "could look like this: "]
    translatedCode $ flip localise $ translations $ do
      german "(A und nicht B) oder (nicht C und nicht D)"
      english "(A and not B) or (not C and not D)"
    pure ()
  paragraph $ text (fromMaybe "" addText)
  pure ()


verifyStatic :: OutputMonad m => MinInst -> LangM m
verifyStatic MinInst{..}
    | isEmptyDnf dnf || hasEmptyCon dnf =
        refuse $ indent $ translate $ do
          german "Geben Sie bitte eine nicht-leere Formel an."
          english "Please give a non empty formula."

    | otherwise = pure ()



verifyQuiz :: OutputMonad m => MinMaxConfig -> LangM m
verifyQuiz = Max.verifyQuiz



start :: Dnf
start = mkDnf [mkCon [Literal 'A']]



partialGrade :: OutputMonad m => MinInst -> Dnf -> LangM m
partialGrade MinInst{..} sol = Max.partialMinMax corLits dnf sol allMinTerms False
  where
    corLits = atomics dnf
    allMinTerms = not $ all (\c -> amount c == length corLits) $ getConjunctions sol



completeGrade :: OutputMonad m => MinInst -> Dnf -> LangM m
completeGrade MinInst{..} = Max.completeMinMax dnf
