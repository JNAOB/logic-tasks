{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module LogicTasks.Semantics.Decide where


import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  Language (..),
  OutputCapable,
  english,
  german,
  translate,
  Rated,
  translations,
  ArticleToUse (DefiniteArticle),
  reRefuse,
  extendedMultipleChoice,
  MinimumThreshold (MinimumThreshold),
  Punishment (Punishment),
  TargetedCorrect (TargetedCorrect),
  localise,
  )
import Data.List.Extra ((\\), intercalate)
import Data.Map (Map, fromList)
import Test.QuickCheck (Gen, suchThat)

import Config (DecideConfig(..), DecideInst(..), FormulaConfig (..), FormulaInst (..), DecideChoice (..), showChoice)
import Formula.Table (flipAt, readEntries)
import Formula.Types (atomics, availableLetter, getTable)
import Util (isOutside, remove, withRatio, checkTruthValueRangeAndFormulaConf, formulaDependsOnAllAtoms)
import LogicTasks.Helpers (extra, reject)
import Control.Monad (when)
import Trees.Generate (genSynTree)
-- import Data.Maybe (fromMaybe)
import LogicTasks.Util (genCnf', genDnf', displayFormula, usesAllAtoms, isEmptyFormula)
import Control.Applicative (Alternative)
import GHC.Real ((%))



genDecideInst :: DecideConfig -> Gen DecideInst
genDecideInst DecideConfig{..} = do
    let percentTrueEntries' = percentTrueEntries
    -- jscpd:ignore-start
    formula <- flip suchThat formulaDependsOnAllAtoms $ case formulaConfig of
      (FormulaArbitrary syntaxTreeConfig) ->
        InstArbitrary <$> genSynTree syntaxTreeConfig  `suchThat` withRatio percentTrueEntries'
      (FormulaCnf cnfCfg) ->
        InstCnf <$> genCnf' cnfCfg `suchThat` withRatio percentTrueEntries'
      (FormulaDnf dnfCfg) ->
        InstDnf <$> genDnf' dnfCfg `suchThat` withRatio percentTrueEntries'
    -- jscpd:ignore-end

    let
      tableLen = length $ readEntries $ getTable formula
      mistakeCount = max (tableLen * percentageOfChanged `div` 100) 1
    mistakes <- remove (tableLen - mistakeCount) [1..tableLen]
    pure $ DecideInst {
      formula
    , changed = mistakes
    , showSolution = printSolution
    , addText = extraText
    }



description :: OutputCapable m => Bool -> DecideInst -> LangM m
description withDropdowns DecideInst{..} = do
  paragraph $ do
    translate $ do
      english "Consider the following formula:"
      german "Betrachten Sie die folgende Formel:"
    indent $ code $ availableLetter (atomics formula) : " = " ++ displayFormula formula
    pure ()
  paragraph $ do
    translate $ do
      english "Decide for each row of the truth table whether the truth value in the last column is correct or incorrect."
      german "Entscheiden Sie für jede Tabellenzeile, ob der Wahrheitswert in der letzten Spalte der Wahrheitstafel korrekt oder fehlerhaft ist."
    indent $ code $ show (flipAt (getTable formula) changed)
    pure ()
  if withDropdowns
    then do
      paragraph $ do
        translate $ do
          english "For this, consider the repeated truth table below. "
          english "Next to each row a selection menu with these three options is given:"
          german "Betrachten Sie dazu die folgende erneute Darstellung der Wahrheitstafel. "
          german "Neben jeder Zeile befindet sich ein Auswahlmenü mit diesen drei Optionen:"
        translatedCode $ flip localise $ translations $ do
          english $ intercalate ", " $ map (showChoice English) [Correct,Wrong,NoAnswer]
          german $ intercalate ", " $ map (showChoice German) [Correct,Wrong,NoAnswer]
        translate $ do
          english "Choose the appropriate option for each row."
          german "Wählen Sie für jede Zeile die passende Option aus."
        pure ()
    else do
      paragraph $ do
        translate $ do
          english "Give the solution as a list of decisions. "
          english "A decision can be one of the following three options:"

          german "Geben Sie die Lösung als eine Liste von Entscheidungen an. "
          german "Als Entscheidung stehen Ihnen die folgenden drei Optionen zur Verfügung:"

        translatedCode $ flip localise $ translations $ do
          english $ intercalate ", " $ map (showChoice English) [Correct,Wrong,NoAnswer]
          german $ intercalate ", " $ map (showChoice German) [Correct,Wrong,NoAnswer]

        translate $ do
          english "Make sure to assign a decision to each row. "
          english "The n-th list element corresponds to the n-th row. "
          english "A solution attempt for a table with four rows could look like this: "
          german "Stellen Sie sicher, dass Sie jeder Zeile eine Entscheidung zuordnen. "
          german "Das n-te Listenelement entspricht der n-ten Zeile. "
          german "Ein Lösungsversuch für eine Tabelle mit vier Zeilen könnte so aussehen: "
        translatedCode $ flip localise $ translations $ do
          english $ "[" ++ intercalate ", " (map (showChoice English) [Correct,Correct,Wrong,NoAnswer]) ++ "]"
          german $ "[" ++ intercalate ", " (map (showChoice German) [Correct,Correct,Wrong,NoAnswer]) ++ "]"

        pure ()

      pure ()
  extra addText
  pure ()


verifyStatic :: OutputCapable m => DecideInst -> LangM m
verifyStatic DecideInst{..}
    | isEmptyFormula formula =
        refuse $ indent $ translate $ do
          english "Please give a non-trivial formula."
          german "Geben Sie bitte eine nicht-triviale Formel an."

    | any (> 2^length (atomics formula)) changed || any (<=0) changed =
        refuse $ indent $ translate $ do
          english "At least one of the given indices does not exist."
          german "Mindestens einer der angegebenen Indizes existiert nicht."



    | null changed =
        refuse $ indent $ translate $ do
          english "At least one mistake has to be specified."
          german "Es muss mindestens einen zu findenden Fehler geben."

    | otherwise = pure ()



verifyQuiz :: OutputCapable m => DecideConfig -> LangM m
verifyQuiz DecideConfig{..}
    | isOutside 1 100 percentageOfChanged =
        refuse $ indent $ translate $ do
          english "The percentage of mistakes has to be set between 1 and 100."
          german "Der prozentuale Anteil an Fehlern muss zwischen 1 und 100 liegen."

    | not $ usesAllAtoms formulaConfig =
        refuse $ indent $ translate $ do
          german "Bei dieser Aufgabe müssen alle verfügbaren Atome verwendet werden."
          english "All available atoms must be used for this task."

    | otherwise = checkTruthValueRangeAndFormulaConf range formulaConfig
  where
    range = percentTrueEntries



start :: [DecideChoice]
start = replicate 4 NoAnswer

partialGrade :: OutputCapable m =>  DecideInst -> [DecideChoice] -> LangM m
partialGrade inst sol
  | lengthDiff > 0 = reject $ do
      german "Die Anzahl der Listenelemente stimmt nicht mit der Anzahl an Zeilen überein. "
      german $ "Fügen Sie " ++ show lengthDiff ++ " weitere Listeneinträge hinzu."
      english "The amount of list elements does not match the amount of table rows."
      english $ "Add " ++ show lengthDiff ++ " more list entries."
  | otherwise = pure ()
  where
    tableLen = length $ readEntries $ getTable $ formula inst
    lengthDiff = tableLen - length sol


completeGrade
  :: (OutputCapable m,Alternative m, Monad m)
  => DecideInst
  -> [DecideChoice]
  -> Rated m
completeGrade DecideInst{..} sol = reRefuse
  (withExtendedMultipleChoice
    (fromIntegral tableLen)
    tableLen
    what
    Nothing
    (fromList $ answerListWrong ++ answerListCorrect)
    solMap
    )
    $ when (showSolution && not (all correctOption indexed && tableLen == length indexed)) $ indent $ do
      paragraph $ do
        translate $ do
          english "The correct solution is:"
          german "Die korrekte Lösung ist:"
        translatedCode $ flip localise $ translations $ do
          english $
            "[" ++ intercalate ", " (map (\i -> showChoice English $ if i `elem` changed then Wrong else Correct) [1..tableLen]) ++ "]"
          german $
            "[" ++ intercalate ", " (map (\i -> showChoice German $ if i `elem` changed then Wrong else Correct) [1..tableLen]) ++ "]"
        pure ()

      paragraph $ translate $ do
        english "All of the above table rows given in the above list contain a wrong entry. "
        english "Every other row of the table contains a correct entry. "
        english "Please compare with the correct version of the table:"
        german "Die obige Liste enthält alle Zeilen der obigen Tafel, welche einen falschen Eintrag enthalten. "
        german "Alle anderen Zeilen der Tafel enthalten einen korrekten Eintrag. "
        german "Vergleichen Sie mit der richtigen Tafel für diese Formel:"
      code $ show table
      pure ()
    where
      indexed = filter ((/=NoAnswer) . snd) $ zip [1..] sol
      solMap = fromList $ map (,True) indexed
      table = getTable formula
      tableLen = length $ readEntries table
      restOf = [1..tableLen] \\ changed
      answerListWrong = map ((,True) . (,Wrong)) changed ++ map ((,False) . (,Wrong)) restOf
      answerListCorrect = map ((,False) . (,Correct)) changed ++ map ((,True) . (,Correct)) restOf
      correctOption (i,c) = case c of
        Correct -> i `elem` restOf
        _   -> i `elem` changed

      what = translations $ do
        german "Antworten"
        english "answers"


withExtendedMultipleChoice
  :: (Ord a, OutputCapable m)
  => Integer
  -> Int
  -> Map Language String
  -> Maybe String
  -> Map a Bool
  -> Map a Bool
  -> Rated m
withExtendedMultipleChoice options changed =
  extendedMultipleChoice
    (MinimumThreshold (1 % 2))
    (Punishment (1 % options))
    (TargetedCorrect changed)
    DefiniteArticle
