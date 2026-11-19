{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.DecomposeFormula where


import Capabilities.Cache (MonadCache)
import Capabilities.LatexSvg (MonadLatexSvg)
import Control.OutputCapable.Blocks (
  GenericOutputCapable (paragraph, translatedCode, refuse, image),
  LangM,
  OutputCapable,
  extra,
  ($=<<),
  collapsed,
  english,
  german,
  localise,
  translate,
  translations,
  )

import LogicTasks.Helpers (example, focus, instruct, basicOpKey, arrowsKey', reject)
import Trees.Types (TreeFormulaAnswer(..))
import Tasks.DecomposeFormula.Config (DecomposeFormulaInst(..), DecomposeFormulaConfig, checkDecomposeFormulaConfig)
import Trees.Print (display, transferToPicture)
import Control.Monad (when, unless)
import Data.Maybe (isNothing, fromJust)
import Trees.Helpers (collectLeaves, collectUniqueBinOpsInSynTree, swapKids)
import Data.Containers.ListUtils (nubOrd)
import LogicTasks.Syntax.TreeToFormula (cacheTree)
import Formula.Parsing (Parse(parser), formulaSymbolParser)
import Formula.Parsing.Delayed (Delayed, withDelayedSucceeding, parseDelayedWithAndThen, complainAboutMissingParenthesesIfNotFailingOn)
import Tasks.SynTree.Config (checkArrowOperatorsToShow)




description :: OutputCapable m => DecomposeFormulaInst -> LangM m
description DecomposeFormulaInst{..} = do

    instruct $ do
      english "Consider the following propositional logic formula:"
      german "Betrachten Sie die folgende aussagenlogische Formel:"

    focus (display tree)

    instruct $ do
      english "Create the syntax tree for this formula and swap the two subtrees at the root. "
      english "Finally, enter the formula for the resulting syntax tree."
      german "Erstellen Sie den Syntaxbaum zu dieser Formel und tauschen Sie die beiden Teilbäume an der Wurzel. "
      german "Geben Sie anschließend die Formel für den entstandenen Syntaxbaum ein."

    instruct $ do
      english "(You are allowed to add arbitrarily many additional pairs of brackets in the formula.)"
      german "(In der Formel dürfen Sie beliebig viele zusätzliche Klammerpaare hinzufügen.)"

    collapsed True (translations $ do
      english "Notes on notation:"
      german "Notationshinweise:")
      (do
        basicOpKey unicodeAllowed
        arrowsKey' arrowOperatorsToShow
        pure()
      )

    paragraph $ do
      translate $ do
        english "A solution attempt could look like this: "
        german "Ein Lösungsversuch könnte beispielsweise so aussehen: "
      translatedCode $ flip localise $ translations exampleCode
      pure ()

    extra addText
    pure ()
      where
        exampleCode | unicodeAllowed = do
                      english "(A ∨ ¬B) and C"
                      german "(A ∨ ¬B) und C"
                    | otherwise      = do
                      english "(A or not B) and C"
                      german "(A oder nicht B) und C"


verifyInst :: OutputCapable m => DecomposeFormulaInst -> LangM m
verifyInst DecomposeFormulaInst {..}
  | not $ checkArrowOperatorsToShow arrowOperatorsToShow = reject $ do
      english "The field arrowOperatorsToShow contains a binary operator which is no arrow."
      german "Das Feld arrowOperatorsToShow enthält einen binären Operator, der kein Pfeil ist."
  | otherwise = pure ()



verifyConfig :: OutputCapable m => DecomposeFormulaConfig -> LangM m
verifyConfig = checkDecomposeFormulaConfig



start :: TreeFormulaAnswer
start = TreeFormulaAnswer Nothing


partialGrade :: OutputCapable m => DecomposeFormulaInst -> Delayed TreeFormulaAnswer -> LangM m
partialGrade = parseDelayedWithAndThen parser complainAboutMissingParenthesesIfNotFailingOn formulaSymbolParser . partialGrade'

partialGrade' :: OutputCapable m => DecomposeFormulaInst -> TreeFormulaAnswer -> LangM m
partialGrade' DecomposeFormulaInst{..} sol = do

  when (isNothing solTree) $ reject $ do
    english "You did not submit a solution."
    german "Die eingereichte Lösung ist leer."

  when (any (`notElem` origLiterals) solLiterals) $ reject $ do
    english "The submitted solution contains unknown atomic formulas."
    german "Die eingereichte Lösung beinhaltet unbekannte atomare Formeln."

  unless (length origLiterals == length solLiterals) $ reject $ do
    english "The submitted solution does not contain all atomic formulas present in the original formula."
    german "Die eingereichte Lösung beinhaltet nicht alle atomaren Formeln aus der ursprünglichen Formel."

  unless (length origOperators == length solOperators) $ reject $ do
    english "The submitted solution does not contain the right amount of different operators."
    german "Die eingereichte Lösung beinhaltet nicht die richtige Anzahl an unterschiedlichen Operatoren."

  pure ()
    where solTree = maybeTree sol
          origLiterals = nubOrd $ collectLeaves tree
          solLiterals = nubOrd $ collectLeaves $ fromJust solTree
          origOperators = collectUniqueBinOpsInSynTree tree
          solOperators = collectUniqueBinOpsInSynTree $ fromJust solTree



completeGrade
  :: (OutputCapable m, MonadCache m, MonadLatexSvg m)
  => FilePath
  -> DecomposeFormulaInst
  -> Delayed TreeFormulaAnswer
  -> LangM m
completeGrade path inst = completeGrade' path inst `withDelayedSucceeding` parser

completeGrade'
  :: (OutputCapable m, MonadCache m, MonadLatexSvg m)
  => FilePath
  -> DecomposeFormulaInst
  -> TreeFormulaAnswer
  -> LangM m
completeGrade' path DecomposeFormulaInst{..} sol
  | solTree /= swappedTree = refuse $ do
    instruct $ do
      english "The submitted solution is not correct."
      german "Die eingereichte Lösung ist nicht korrekt."

    instruct $ do
      english "The original syntax tree looks like this:"
      german "Der originale Syntaxbaum sieht so aus:"

    image $=<< cacheTree (transferToPicture tree) path

    instruct $ do
      english "The syntax tree for your entered formula looks like this:"
      german "Der Syntaxbaum für Ihre eingegebene Formel sieht so aus:"

    image $=<< cacheTree (transferToPicture solTree) path

    when showSolution $ do
      example (display swappedTree) $ do
        english "The solution for this task is:"
        german "Die Lösung für diese Aufgabe ist:"

      instruct $ do
        english "The corresponding syntax tree looks like this:"
        german "Der zugehörige Syntaxbaum sieht so aus:"

      image $=<< cacheTree (transferToPicture swappedTree) path

      pure ()

    pure ()

  | otherwise =
      instruct $ do
        english "The submitted solution is correct."
        german "Die eingereichte Lösung ist korrekt."

    where solTree = fromJust $ maybeTree sol
          swappedTree = swapKids tree
