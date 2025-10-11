{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module LogicTasks.Semantics.Pick where


import Control.Monad (when)
import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  english,
  german,
  translate,
  singleChoiceSyntax,
  singleChoice,
  ArticleToUse (DefiniteArticle),
  translations,
  )

import Test.QuickCheck (Gen, suchThat, elements)

import Config (Number(..), PickConfig(..), PickInst(..), FormulaConfig (..), FormulaInst (..), BaseConfig (..), NormalFormConfig(..))
import Formula.Util (isSemanticEqual)
import Formula.Types (availableLetter, getTable, Formula (atomics))
import Formula.Printing (showIndexedList)
import LogicTasks.Helpers (extra)
import Data.Maybe (fromJust)
import Trees.Generate (genSynTree)
import Tasks.SynTree.Config (SynTreeConfig (..))
import Util (withRatio, vectorOfUniqueBy, checkTruthValueRangeAndFormulaConf, formulaDependsOnAllAtoms)
import LogicTasks.Util (genCnf', genDnf', displayFormula, usesAllAtoms, isEmptyFormula, checkForSmallRange)


genPickInst :: PickConfig -> Gen PickInst
genPickInst PickConfig{..} = do
  let percentTrueEntries' = percentTrueEntries
  formulas <- vectorOfUniqueBy
    amountOfOptions
    isSemanticEqual
    $ flip suchThat formulaDependsOnAllAtoms $ case formulaConfig of
        (FormulaArbitrary syntaxTreeConfig) ->
          InstArbitrary <$> genSynTree syntaxTreeConfig `suchThat` withRatio percentTrueEntries'
        (FormulaCnf cnfCfg) ->
          InstCnf <$> genCnf' cnfCfg `suchThat` withRatio percentTrueEntries'
        (FormulaDnf dnfCfg) ->
          InstDnf <$> genDnf' dnfCfg `suchThat` withRatio percentTrueEntries'

  correct <- elements [1..amountOfOptions]

  pure $ PickInst {
    formulas,
    correct,
    showSolution = printSolution,
    addText = extraText
  }




description :: OutputCapable m => Bool -> PickInst -> LangM m
description inputHelp PickInst{..} = do
    paragraph $ do
      translate $ do
        german "Betrachten Sie die folgende Formel:"
        english "Consider the following formula:"
      indent $ code $ availableLetter (atomics correctFormula) : " = " ++ displayFormula correctFormula
      pure ()
    paragraph $ do
      translate $ do
        german "Welche der folgenden Wahrheitstafeln passt zu der Formel? Geben Sie die richtige Tafel durch ihre Nummer an."
        english "Which of the following truth tables represents the formula? Specify the correct table by giving its number."
      indent $ code $ showIndexedList 120 5 $ map getTable formulas
      pure ()
    when inputHelp $ paragraph $ indent $ do
      translate $ do
        german "Ein Lösungsversuch könnte so aussehen: "
        english "A solution attempt could look like this: "
      code "1"
      pure ()
    extra addText
    pure ()
  where
    correctFormula = formulas !! (correct - 1)


verifyStatic :: OutputCapable m => PickInst -> LangM m
verifyStatic PickInst{..}
    | null formulas =
        refuse $ indent $ translate $ do
          german "Die Liste der Formeln ist leer."
          english "The list of formulas is empty."

    | any isEmptyFormula formulas =
        refuse $ indent $ translate $ do
          german "Mindestens eine der Formeln ist für diese Aufgabe nicht geeignet."
          english "At least one given formula is not suitable for this task."

    | length formulas < correct || correct <= 0 =
        refuse $ indent $ translate $ do
          german "Der angegebene Index existiert nicht."
          english "The given index does not exist."

    | otherwise = pure()



verifyQuiz :: OutputCapable m => PickConfig -> LangM m
verifyQuiz PickConfig{..}

    | amountOfOptions < 2 =
        refuse $ indent $ translate $ do
          german "Es muss mindestens zwei Optionen geben."
          english "At least two options need to be given."

    | not $ hasMinUniqueAtoms 2 formulaConfig =
        refuse $ indent $ translate $ do
          german "Es muss mindestens zwei unterschiedliche Atome geben."
          english "At least two unique atoms are required."


    | doesOvershootOptions formulaConfig =
        refuse $ indent $ translate $ do
          german "Die Anzahl Optionen übersteigt die Anzahl möglicher, unterschiedlicher Formeln."
          english "The amount of options is higher than the amount of possible, distinct formulas."

    | not $ usesAllAtoms formulaConfig =
        refuse $ indent $ translate $ do
          german "Bei dieser Aufgabe müssen alle verfügbaren Atome verwendet werden."
          english "All available atoms must be used for this task."

    | checkForSmallRange rangeL rangeH (2 ^ minAmountOfUniqueAtoms syntaxTreeConfig) =
        refuse $ indent $ translate $ do
          german "Die Beschränkung der Wahr-Einträge sollte mindestens 2 Ausprägungen erlauben."
          english "The given restriction on True entries should allow at least 2 values."

    | otherwise = checkTruthValueRangeAndFormulaConf range formulaConfig
  where
    range@(rangeL, rangeH) = percentTrueEntries
    hasMinUniqueAtoms x (FormulaArbitrary syntaxTreeConfig) = minAmountOfUniqueAtoms syntaxTreeConfig >= x
    hasMinUniqueAtoms _ _ = True
    doesOvershootOptions (FormulaArbitrary syntaxTreeConfig)
      = amountOfOptions > 4*2^ length (availableAtoms syntaxTreeConfig)
    doesOvershootOptions (FormulaCnf cnfCfg)
      = amountOfOptions > 4*2^ length (usedAtoms (baseConf cnfCfg))
    doesOvershootOptions (FormulaDnf dnfCfg)
      = amountOfOptions > 4*2^ length (usedAtoms (baseConf dnfCfg))



start :: Number
start = Number Nothing

partialGrade :: OutputCapable m => PickInst -> Number -> LangM m
partialGrade _ (Number Nothing) = refuse $ indent $
        translate $ do
          german "Es wurde kein Index angegeben."
          english "You did not give an index."
partialGrade PickInst{formulas} (Number (Just index)) = singleChoiceSyntax True [1..length formulas] index

completeGrade :: OutputCapable m => PickInst -> Number -> LangM m
completeGrade PickInst{..} (Number index) = singleChoice
  DefiniteArticle
  what
  displaySolution
  correct
  (fromJust index)
    where
      what = translations $ do
        german "Index"
        english "index"
      displaySolution | showSolution = Just $ show correct
                      | otherwise = Nothing
