{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module LogicTasks.Semantics.Fill where


import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  english,
  german,
  translate,
  extendedMultipleChoice,
  MinimumThreshold (MinimumThreshold),
  Punishment (Punishment),
  TargetedCorrect (TargetedCorrect),
  ArticleToUse (DefiniteArticle),
  translations,
  Rated, reRefuse,
  )
-- import Data.Maybe (fromMaybe)
import Test.QuickCheck(Gen, suchThat)

import Config ( FillConfig(..), FillInst(..), FormulaInst (..), FormulaConfig (..))
import Formula.Table (gapsAt, readEntries)
import Formula.Types (TruthValue, availableLetter, atomics, getTable, truth)
import Util (
  isOutside,
  pairwiseCheck,
  preventWithHint,
  remove,
  withRatio,
  checkTruthValueRangeAndFormulaConf,
  formulaDependsOnAllAtoms
  )
import LogicTasks.Helpers (extra)
import Trees.Generate (genSynTree)
import LogicTasks.Util (genCnf', genDnf', displayFormula, usesAllAtoms, isEmptyFormula)
import qualified Data.Map as Map (fromAscList)
import GHC.Real ((%))
import Control.Applicative (Alternative)
import Control.Monad (when)
import Data.Foldable.Extra (notNull)


genFillInst :: FillConfig -> Gen FillInst
genFillInst FillConfig{..} = do
    let percentTrueEntries' = percentTrueEntries

    formula <- flip suchThat formulaDependsOnAllAtoms $ case formulaConfig of
      (FormulaArbitrary syntaxTreeConfig) ->
        InstArbitrary <$> genSynTree syntaxTreeConfig `suchThat` withRatio percentTrueEntries'
      (FormulaCnf cnfCfg) ->
        InstCnf <$> genCnf' cnfCfg `suchThat` withRatio percentTrueEntries'
      (FormulaDnf dnfCfg) ->
        InstDnf <$> genDnf' dnfCfg `suchThat` withRatio percentTrueEntries'

    let
      entries = readEntries $ getTable formula
      tableLen = length entries
      gapCount = max (tableLen * percentageOfGaps `div` 100) 1
    gaps <- remove (tableLen - gapCount) [1..tableLen]
    let missingValues = [ b | (i, Just b) <- zip ([1..] :: [Int]) entries, i `elem` gaps]
    pure $ FillInst {
      formula
    , missing = gaps
    , missingValues
    , showSolution = printSolution
    , addText = extraText
    }




description :: OutputCapable m => Bool -> FillInst -> LangM m
description inputHelp FillInst{..} = do
  paragraph $ do
    translate $ do
      german  "Betrachten Sie die folgende Formel:"
      english "Consider the following formula:"
    indent $ code $ availableLetter (atomics formula) : " = " ++ displayFormula formula
    pure ()
  paragraph $ do
    translate $ do
      german "Füllen Sie in der zugehörigen Wahrheitstafel alle Lücken mit einem passenden Wahrheitswert (Wahr oder Falsch)."
      english "Fill all blanks in the corresponding truth table with truth values (True or False)."
    indent $ code $ show $ gapsAt (getTable formula) missing
    pure ()
  when inputHelp $ paragraph $ translate $ do
    german "Geben Sie als Lösung eine Liste der fehlenden Wahrheitswerte an, wobei das erste Element der Liste der ersten Lücke von oben entspricht, das zweite Element der zweiten Lücke, etc."
    english "Provide the solution as a list of truth values. The first element of the list fills the first blank from the top, the second element fills the second blank, etc."

  paragraph $ translate $ do
    german "Die Eingabe der Werte kann binär (0 = falsch, 1 = wahr), ausgeschrieben (falsch, wahr) oder als Kurzform (f, w) erfolgen."
    english "Values can be submitted in binary form (0 = false, 1 = true), by entering the entire word (false, true) or by giving a shorthand (f, t)."

  when inputHelp $ paragraph $ indent $ do
    translate $ do
      german "Ein Lösungsversuch im Fall von vier Lücken könnte so aussehen:"
      english "A solution attempt for four blanks could look like this:"
    code "[0,1,1,1]"
    pure ()

  extra addText
  pure ()


verifyStatic :: OutputCapable m => FillInst -> LangM m
verifyStatic FillInst{..}
    | isEmptyFormula formula =
        refuse $ indent $ translate $ do
          german "Geben Sie bitte eine nicht-triviale Formel an."
          english "Please give a non-trivial formula."

    | any (> 2^length (atomics formula)) missing || any (<=0) missing =
    refuse $ indent $ translate $ do
      english "At least one of the given indices does not exist."
      german "Mindestens einer der angegebenen Indizes existiert nicht."


    | null missing =
        refuse $ indent $ translate $ do
          german "Es muss mindestens eine zu findende Lücke geben."
          english "At least one blank has to be specified."

    | otherwise = pure()



verifyQuiz :: OutputCapable m => FillConfig -> LangM m
verifyQuiz FillConfig{..}
    | isOutside 1 100 percentageOfGaps =
        refuse $ indent $ translate$ do
          german "Der prozentuale Anteil an Lücken muss zwischen 1 und 100 liegen."
          english "The percentage of gaps has to be set between 1 and 100."

    | not $ usesAllAtoms formulaConfig =
        refuse $ indent $ translate $ do
          german "Bei dieser Aufgabe müssen alle verfügbaren Atome verwendet werden."
          english "All available atoms must be used for this task."

    | otherwise = checkTruthValueRangeAndFormulaConf range formulaConfig
  where
    range = percentTrueEntries



start :: [TruthValue]
start = []

partialGrade :: OutputCapable m => FillInst -> [TruthValue] -> LangM m
partialGrade FillInst{..} sol = do
  preventWithHint (solLen /= missingLen)
    (translate $ do
      german "Ihre Abgabe hat die korrekte Länge?"
      english "Your submission has the correct length?"
    )
    (translate $ do
      german $ "Ihre Abgabe muss genau " ++ show missingLen ++ " Einträge enthalten."
      english $ "Your submission must contain exactly " ++ show missingLen ++ " entries."
    )

  pure ()
    where
      boolSol = map truth sol
      solLen = length boolSol
      missingLen = length missing

completeGrade :: (OutputCapable m, Alternative m, Monad m) => FillInst -> [TruthValue] -> Rated m
completeGrade FillInst{..} sol = reRefuse
  (extendedMultipleChoice
    (MinimumThreshold (1 % 2))
    (Punishment 0)
    (TargetedCorrect (length solution))
    DefiniteArticle
    what
    solutionDisplay
    solution
    submission)
  $ when (notNull diff && not showSolution) $ translate $ do
    german $ "Ihre Abgabe enthält " ++ displayMistake ++ " Fehler."
    english $ "Your submission contains " ++ displayMistake ++ " mistakes."

  where
    boolSol = map truth sol
    zippedShort = zip3 boolSol missingValues [1..]
    (_,diff) = pairwiseCheck zippedShort
    displayMistake = show $ length diff
    what = translations $ do
      german "Wahr-Werte"
      english "True values"
    solutionDisplay | showSolution = Just $ show missingValues
                    | otherwise = Nothing
    solution = Map.fromAscList $ zip [1 :: Int ..] missingValues
    submission = Map.fromAscList $ zip [1 :: Int ..] boolSol
