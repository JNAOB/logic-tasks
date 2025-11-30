{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.SimplestFormula where


import Control.OutputCapable.Blocks (
  GenericOutputCapable (translatedCode),
  LangM,
  OutputCapable,
  english,
  german,
  paragraph,
  translate,
  localise,
  translations,
  MinimumThreshold (MinimumThreshold),
  ArticleToUse (DefiniteArticle),
  translations,
  Rated,
  reRefuse,
  printSolutionAndAssertMinimum, collapsed
  )
import Data.List (nub, sort)
import Data.Maybe (isNothing, fromJust)
import GHC.Real ((%))
import LogicTasks.Helpers (basicOpKey, extra, focus, instruct, reject, arrowsKey)
import Tasks.SuperfluousBrackets.Config (
    checkSuperfluousBracketsConfig,
    SuperfluousBracketsConfig(..),
    SuperfluousBracketsInst(..)
    )
import Trees.Helpers
import Trees.Types
import Control.Monad (when)
import Formula.Parsing.Delayed (Delayed, parseDelayedWithAndThen, complainAboutMissingParenthesesIfNotFailingOn, withDelayedSucceeding)
import Formula.Parsing (Parse(..), formulaSymbolParser)
import Formula.Util (isSemanticEqual)
import Trees.Parsing()
import Control.Applicative (Alternative)




description :: OutputCapable m => SuperfluousBracketsInst -> LangM m
description SuperfluousBracketsInst{..} = do
    instruct $ do
      english "Remove all unnecessary pairs of brackets in the given formula."
      german "Entfernen Sie alle unnötigen Klammer-Paare in der gegebenen Formel."

    focus stringWithSuperfluousBrackets

    instruct $ do
      english "Give your answer as a propositional logic formula again."
      german "Geben Sie die Lösung wieder in Form einer aussagenlogischen Formel an."

    collapsed False (do
      english "Additional hints:"
      german "Weitere Hinweise:")
      (do

        paragraph $ do
          translate $ do
            english "For example, if (A ∨ B) is the given formula, then the following solution is correct:"
            german "Ist z.B. (A ∨ B) die gegebene Formel, dann ist die folgende Lösung korrekt:"
          translatedCode $ flip localise $ translations exampleCode
          pure ()

        paragraph $ translate $ do
          german "Sie können dafür die ursprüngliche Formel in das Abgabefeld kopieren und unnötige Klammern entfernen, oder leer startend die folgenden Schreibweisen nutzen:"
          english "You can copy the original formula into the submission field and remove unnecessary brackets, or start from scratch and use the following syntax:"

        basicOpKey unicodeAllowed

        when showArrowOperators arrowsKey

        instruct $ do
          english "Due to the associativity of ∧ and ∨, it is not necessary to use brackets in subformulas consisting of three or more atomic formulas connected by the same logical operator, for example in:"
          german "Aufgrund der Assoziativität von ∧ und ∨ muss in Teilformeln, die aus drei oder mehr atomaren Formeln verknüpft mit dem gleichen logischen Operator bestehen, nicht geklammert werden, z.B. bei:"

        focus "A ∧ B ∧ C"

        instruct $ do
          english "Similarly, brackets are not necessary for one or more consecutive negations directly in front of an atomic formula, for example in:"
          german "Genauso sind Klammern bei einer oder mehreren Negationen direkt vor einer atomaren Formel nicht nötig, z.B. bei"

        focus "¬¬A"

        instruct $ do
          english "Remove all unnecessary pairs of brackets in the given formula (regarding associativity not just concerning atomic formulas)."
          german "Entfernen Sie alle unnötigen Klammer-Paare in der gegebenen Formel (hinsichtlich Assoziativität nicht nur atomare Formeln betreffend)."

        pure ()
      )

    extra addText
    pure ()
      where
        exampleCode | unicodeAllowed = do
                      german "A ∨ B"
                      english "A ∨ B"
                    | otherwise      = do
                      german "A oder B"
                      english "A or B"


verifyInst :: OutputCapable m => SuperfluousBracketsInst -> LangM m
verifyInst _ = pure()



verifyConfig :: OutputCapable m => SuperfluousBracketsConfig -> LangM m
verifyConfig = checkSuperfluousBracketsConfig



start :: FormulaAnswer
start = FormulaAnswer Nothing


partialGrade :: OutputCapable m => SuperfluousBracketsInst -> Delayed FormulaAnswer -> LangM m
partialGrade = parseDelayedWithAndThen parser complainAboutMissingParenthesesIfNotFailingOn formulaSymbolParser . partialGrade'

partialGrade' :: OutputCapable m => SuperfluousBracketsInst -> FormulaAnswer -> LangM m
partialGrade' SuperfluousBracketsInst{..} f
    | isNothing $ maybeForm f =
      reject $ do
        english "Your submission is empty."
        german "Sie haben keine Formel angegeben."

    | any (`notElem` correctAtoms) atoms =
      reject $ do
        english "Your submission contains unknown atomic formulas."
        german "Ihre Abgabe beinhaltet unbekannte atomare Formeln."

    | any (`notElem` atoms) correctAtoms =
      reject $ do
        english "Your submission does not contain all atomic formulas present in the original formula."
        german "Ihre Abgabe beinhaltet nicht alle atomaren Formeln aus der ursprünglichen Formel."

    | opsNum > correctOpsNum =
      reject $ do
        english "Your submission contains more logical operators than the original formula."
        german "Ihre Abgabe beinhaltet mehr logische Operatoren als die ursprüngliche Formel."

    | opsNum < correctOpsNum =
      reject $ do
        english "Your submission contains fewer logical operators than the original formula."
        german "Ihre Abgabe beinhaltet weniger logische Operatoren als die ursprüngliche Formel."

    | not $ isDerivedByRemovingBrackets (show pForm) stringWithSuperfluousBrackets =
      reject $ do
        english "Your submission cannot be obtained from the original formula by removing brackets."
        german "Ihre Abgabe lässt sich nicht durch Entfernen von Klammern aus der ursprünglichen Formel erhalten."

    | otherwise = pure()
  where
    pForm = fromJust $ maybeForm f
    atoms = sort $ nub $ collectLeaves pForm
    opsNum = numOfOpsInFormula pForm
    correctAtoms = sort $ nub $ collectLeaves tree
    correctOpsNum = numOfOps tree

completeGrade :: (OutputCapable m, Alternative m, Monad m) => SuperfluousBracketsInst -> Delayed FormulaAnswer -> Rated m
completeGrade inst = completeGrade' inst `withDelayedSucceeding` parser

completeGrade' :: (OutputCapable m, Alternative m, Monad m) => SuperfluousBracketsInst -> FormulaAnswer -> Rated m
completeGrade' inst sol
  | show sol == simplestString inst = rate 1
  | synTreeEquivalent && isDerivedByRemovingBrackets (simplestString inst) (show submission) = reRefuse (rate percentage) (translate $ do
    german ("Sie haben " ++ show superfluousBracketPairsSubmission ++ " überflüssige" ++ (if isSingular then "s " else " ") ++ "Klammerpaar" ++ (if isSingular then " " else "e ") ++ "in der Abgabe.")
    english ("You left " ++ show superfluousBracketPairsSubmission ++ " superfluous pair" ++ (if isSingular then " " else "s ") ++ "of brackets in your submission."))
  | synTreeEquivalent = reRefuse (rate 0) (translate $ do
    german "Ihre Formel ist semantisch äquivalent zur ursprünglich gegebenen, aber Sie haben nicht nur überflüssige Klammern entfernt."
    english "Your formula is semantically equivalent to the original one, but you have not just removed superfluous brackets.")
  | otherwise = reRefuse (rate 0) (translate $ do
    german "Sie haben die Formel verändert."
    english "You changed the formula.")

  where
    countBracketPairs :: String -> Integer
    countBracketPairs = fromIntegral . length . filter (== '(')
    submission = fromJust (maybeForm sol)
    synTreeSubmission = toSynTree submission
    bracketPairsSubmission = countBracketPairs $ show submission
    bracketPairsSolution = countBracketPairs $ simplestString inst
    bracketPairsMax = countBracketPairs $ stringWithSuperfluousBrackets inst
    superfluousBracketPairsSubmission = bracketPairsSubmission - bracketPairsSolution
    superfluousBracketPairsTask = bracketPairsMax - bracketPairsSolution
    synTreeEquivalent = isSemanticEqual synTreeSubmission (tree inst)
    percentage = (superfluousBracketPairsTask - superfluousBracketPairsSubmission) % superfluousBracketPairsTask
    isSingular = superfluousBracketPairsSubmission  == 1
    rate = printSolutionAndAssertMinimum
      (MinimumThreshold (1 % superfluousBracketPairsTask))
      DefiniteArticle
      (if showSolution inst then Just $ simplestString inst else Nothing)

-- | Checks whether the second string can be transformed into
--   the first string by removing only brackets.
--   Spaces are ignored.
isDerivedByRemovingBrackets :: String -> String -> Bool
isDerivedByRemovingBrackets [] [] = True
isDerivedByRemovingBrackets _ [] = False
isDerivedByRemovingBrackets [] (')' : ys) = isDerivedByRemovingBrackets [] ys
isDerivedByRemovingBrackets [] _ = False
isDerivedByRemovingBrackets (x : xs) (y : ys)
  | x == y = isDerivedByRemovingBrackets xs ys
  | y == '(' || y == ')' = isDerivedByRemovingBrackets (x : xs) ys
  | x == ' ' = isDerivedByRemovingBrackets xs (y : ys)
  | y == ' ' = isDerivedByRemovingBrackets (x:xs) ys
  |otherwise = False
