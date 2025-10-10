{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module LogicTasks.Semantics.Min where


import qualified LogicTasks.Semantics.Max as Max

import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  english,
  german,
  translate,
  translations,
  localise,
  )
-- import Data.Maybe (fromMaybe)
import Test.QuickCheck (Gen, suchThat)

import Config (BaseConfig(..), NormalFormConfig(..), MinMaxConfig(..), MinInst(..))
import Formula.Types (Dnf, Literal(..), amount, atomics, genDnf, getConjunctions, getTable)
import Formula.Util (mkCon, mkDnf, hasEmptyCon, isEmptyDnf)
import LogicTasks.Helpers (extra, formulaKey)
import Util (withRatio)
import Formula.Parsing.Delayed (Delayed, withDelayed, displayParseError, withDelayedSucceeding)
import Formula.Parsing (Parse(..))



genMinInst :: MinMaxConfig -> Gen MinInst
genMinInst MinMaxConfig {normalFormConf = NormalFormConfig {baseConf = BaseConfig{..},..},..} = do
    dnf <- dnfInRange
    pure $ MinInst {
      dnf
    , showSolution = printSolution
    , addText = extraText
    , unicodeAllowed = offerUnicodeInput
    }
   where
     getDnf = genDnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedAtoms True
     dnfInRange = getDnf `suchThat` withRatio (percentTrueEntries)



description :: OutputCapable m => MinInst -> LangM m
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

  formulaKey unicodeAllowed

  paragraph $ indent $ do
    translate $ do
      let formulaStr = show $ mkDnf [mkCon [Positive 'A', Negative 'B'], mkCon [Negative 'C', Negative 'D']]
      german $ unwords ["Ein Lösungsversuch für Formel", formulaStr, "könnte beispielsweise so aussehen: "]
      english $ unwords ["A solution attempt for the formula", formulaStr, "could look like this: "]
    translatedCode $ flip localise $ translations exampleCode

    pure ()
  extra addText
  pure ()
    where
      exampleCode | unicodeAllowed = do
                      german "(A ∧ ¬B) oder (nicht C und nicht D)"
                      english "(A ∧ ¬B) or (not C and not D)"
                  | otherwise      = do
                      german "(A und nicht B) oder (nicht C und nicht D)"
                      english "(A and not B) or (not C and not D)"


verifyStatic :: OutputCapable m => MinInst -> LangM m
verifyStatic MinInst{..}
    | isEmptyDnf dnf || hasEmptyCon dnf =
        refuse $ indent $ translate $ do
          german "Geben Sie bitte eine nicht-triviale Formel an."
          english "Please give a non-trivial formula."

    | otherwise = pure ()



verifyQuiz :: OutputCapable m => MinMaxConfig -> LangM m
verifyQuiz = Max.verifyQuiz



start :: Dnf
start = mkDnf [mkCon [Positive 'A']]

partialGrade :: OutputCapable m => MinInst -> Delayed Dnf -> LangM m
partialGrade inst = (partialGrade' inst `withDelayed` parser) displayParseError

partialGrade' :: OutputCapable m => MinInst -> Dnf -> LangM m
partialGrade' MinInst{..} sol = Max.partialMinMax correctAtoms dnf sol allMinTerms False
  where
    correctAtoms = atomics dnf
    allMinTerms = not $ all (\c -> amount c == length correctAtoms) $ getConjunctions sol


completeGrade :: OutputCapable m => MinInst -> Delayed Dnf -> LangM m
completeGrade inst = completeGrade' inst `withDelayedSucceeding` parser

completeGrade' :: OutputCapable m => MinInst -> Dnf -> LangM m
completeGrade' MinInst{..} = Max.completeMinMax showSolution dnf
