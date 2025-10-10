{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module LogicTasks.Semantics.Max where


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
import Data.List ((\\))
-- import Data.Maybe (fromMaybe)
import Test.QuickCheck (Gen, suchThat)

import Config (BaseConfig(..), NormalFormConfig(..),  MaxInst(..), MinMaxConfig(..))
import Formula.Util (hasEmptyClause, isEmptyCnf, mkClause, mkCnf)
import Formula.Table (readEntries)
import Formula.Types (Cnf, Formula, Literal(..), amount, atomics, genCnf, getClauses, getTable)
import LogicTasks.Helpers (formulaKey, example, extra)
import Util (checkTruthValueRange, pairwiseCheck, prevent, preventWithHint, withRatio, checkNormalFormConfig)
import Control.Monad (when)
import Formula.Parsing.Delayed (Delayed, withDelayed, displayParseError, withDelayedSucceeding)
import Formula.Parsing (Parse(..))



genMaxInst :: MinMaxConfig -> Gen MaxInst
genMaxInst MinMaxConfig {normalFormConf = NormalFormConfig {baseConf = BaseConfig{..},..},..} = do
    cnf <- cnfInRange
    pure $ MaxInst {
      cnf
    , showSolution = printSolution
    , addText = extraText
    , unicodeAllowed = offerUnicodeInput
    }
  where
    getCnf = genCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedAtoms True
    cnfInRange = getCnf `suchThat` withRatio (percentTrueEntries)



description :: OutputCapable m => MaxInst -> LangM m
description MaxInst{..} = do
  paragraph $ do
    translate $ do
      german "Betrachten Sie die folgende Wahrheitstafel:"
      english "Consider the following truth table:"
    indent $ code $ show $ getTable cnf
    pure ()
  paragraph $ translate $ do
    german "Geben Sie eine zu der Tafel passende Formel in konjunktiver Normalform an. Verwenden Sie dazu Max-Terme."
    english "Provide a formula in conjunctive normal form, that corresponds to the table. Use maxterms to do this."

  formulaKey unicodeAllowed

  -- jscpd:ignore-start
  paragraph $ indent $ do
    translate $ do
      let formulaStr = show $ mkCnf [mkClause [Positive 'A', Negative 'B'], mkClause [Negative 'C', Negative 'D']]
      german $ unwords ["Ein Lösungsversuch für Formel", formulaStr, "könnte beispielsweise so aussehen: "]
      english $ unwords ["A solution attempt for the formula", formulaStr, "could look like this: "]
    translatedCode $ flip localise $ translations exampleCode
    pure ()
  -- jscpd:ignore-end

  extra addText
  pure ()
    where
      exampleCode | unicodeAllowed = do
                      german "(A ∨ ¬B) und (nicht C oder nicht D)"
                      english "(A ∨ ¬B) and (not C or not D)"
                  | otherwise      = do
                      german "(A oder nicht B) und (nicht C oder nicht D)"
                      english "(A or not B) and (not C or not D)"


verifyStatic :: OutputCapable m => MaxInst -> LangM m
verifyStatic MaxInst{..}
    | isEmptyCnf cnf || hasEmptyClause cnf =
        refuse $ indent $ translate $ do
          german "Geben Sie bitte eine nicht-triviale Formel an."
          english "Please give a non-trivial formula."

    | otherwise = pure()



verifyQuiz :: OutputCapable m => MinMaxConfig -> LangM m
verifyQuiz MinMaxConfig{..} = do
  checkTruthValueRange (low,high) (2 ^ length (usedAtoms (baseConf normalFormConf)))
  checkNormalFormConfig normalFormConf
  pure ()
  where
    (low,high) = percentTrueEntries



start :: Cnf
start = mkCnf [mkClause [Positive 'A']]



partialMinMax :: (OutputCapable m, Formula f) => [Char] -> f -> f -> Bool -> Bool -> LangM m
partialMinMax correctAtoms correct solution allValidTerms isMaxTermTask = do
  preventWithHint (not $ null extraAtoms)
    (translate $ do
      german "Angegebene atomare Formeln kommen in Aufgabe vor?"
      english "Given atomic formulas are used in task?"
    )

    (paragraph $ do
      translate $ do
        german "Es sind unbekannte atomare Formeln enthalten. Diese atomaren Formeln kommen in der korrekten Lösung nicht vor: "
        english "Your submission contains unknown atomic formulas. These do not appear in a correct solution: "
      itemizeM $ map (text . show) extraAtoms
      pure ()
    )

  preventWithHint (not $ null missing)
    (translate $ do
      german "Alle atomaren Formeln kommen vor?"
      english "All atomic formulas are contained in solution?"
    )

    (paragraph $ do
      translate $ do
        german "Es fehlen atomare Formeln. Fügen Sie diese atomaren Formeln der Abgabe hinzu: "
        english "Some atomic formulas are missing. Add these atomic formulas to your submission: "
      itemizeM $ map (text . show) missing
      pure ()
    )

  prevent allValidTerms $
    translate $ do
      german $ "Alle " ++ gSubElements ++ " sind " ++ gTerms ++ "?"
      english $ "All " ++ eSubElements ++ " are " ++ eTerms ++ "?"

  preventWithHint (solLen < corrLen)
    (translate $ do
      german $ "Genügend " ++ gTerms ++ " in Lösung?"
      english $ "Solution contains enough " ++ eTerms ++ "?"
    )

    (paragraph $ do
      translate $ do
        german $ "Die angegebene Formel enthält zu wenige " ++ gTerms ++ ". Fügen sie "
        english $ "The formula does not contain enough " ++ eTerms ++ ". Add "
      text diff
      translate $ do
        german " hinzu!"
        english "!"
      pure ()
    )

  preventWithHint (solLen > corrLen)
    (translate $ do
      german $ "Nicht zu viele " ++ gTerms ++ " in Lösung?"
      english $ "Not too many " ++ eTerms ++ " in solution?"
    )

    (paragraph $ do
      translate $ do
        german $ " Die angegebene Formel enthält zu viele " ++ gTerms ++ ". Entfernen sie "
        english $ "The formula contains too many " ++ eTerms ++ ". Remove "
      text $ diff ++ "!"
      pure ()
    )
  pure ()
 where
    solAtoms = atomics solution
    extraAtoms = solAtoms \\ correctAtoms
    missing = correctAtoms \\ solAtoms
    table = getTable correct
    corrLen = length $ filter (== Just False) (readEntries table)
    solLen = amount solution
    diff = show $ abs (solLen - corrLen)
    (gTerms, gSubElements, eTerms, eSubElements)= if isMaxTermTask
      then ("Maxterme", "Klauseln", "maxterms", "clauses")
      else ("Minterme", "Konjunktionen", "minterms", "conjunctions")

partialGrade :: OutputCapable m => MaxInst -> Delayed Cnf -> LangM m
partialGrade inst = (partialGrade' inst `withDelayed` parser) displayParseError

partialGrade' :: OutputCapable m => MaxInst -> Cnf -> LangM m
partialGrade' MaxInst{..} sol = partialMinMax correctAtoms cnf sol allMaxTerms True
  where
    correctAtoms = atomics cnf
    allMaxTerms = not $ all (\c -> amount c == length correctAtoms) $ getClauses sol



completeMinMax :: (OutputCapable m, Formula f, Show f) => Bool -> f -> f -> LangM m
completeMinMax showSolution correct solution =
    preventWithHint (not $ null diff)
      (translate $ do
         german "Lösung liefert korrekte Wahrheitstabelle?"
         english "Solution gives correct truth table?"
      )

      (do
        paragraph $ do
          translate $ do
            german "Es existieren falsche Einträge in den folgenden Tabellenzeilen: "
            english "The following rows are not correct: "
          itemizeM $ map (text . show) diff
          pure ()
        when showSolution $ example (show correct) $ do
          english "A possible solution for this task is:"
          german "Eine mögliche Lösung für die Aufgabe ist:"
        pure ()
      )
  where
    solTable = getTable solution
    (_,diff) = pairwiseCheck (zip3 (readEntries solTable) (readEntries $ getTable correct) [1..])

completeGrade :: OutputCapable m => MaxInst -> Delayed Cnf -> LangM m
completeGrade inst = completeGrade' inst `withDelayedSucceeding` parser

completeGrade' :: OutputCapable m => MaxInst -> Cnf -> LangM m
completeGrade' MaxInst{..} = completeMinMax showSolution cnf
