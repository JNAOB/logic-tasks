taskName: FindLiar
validation: Validate
=============================================

{-# LANGUAGE DeriveDataTypeable#-}

module Global where

import Data.Data (Data)
import Data.Text (Text)

import LogicTasks.Formula (TruthValue)
import Trees.Types (SynTree, BinOp)


newtype Table = Table [(Maybe (SynTree BinOp Char), [Maybe TruthValue])]
    deriving (Eq,Show)

data Namen = A | B | C
    deriving (Eq,Enum,Bounded,Show,Ord,Data)

allNamen :: [Namen]
allNamen = [minBound .. maxBound]

data Submission = Submission
  { submittedParts   :: [SynTree BinOp Char]
  , submittedFormula :: SynTree BinOp Char
  , submittedTable   :: Table
  , identifiedLiars  :: [Namen]
  }

data TaskData = TaskData
  { hints          :: [Text]
  , solutionValues :: [Bool]
  , formulaParts   :: [SynTree BinOp Char]
  } deriving (Data)


=============================================


module TaskSettings where

-- 2025: Weight 1.0 (in Logik, Task10)

import Control.OutputCapable.Blocks
  ( LangM
  , OutputCapable
  , indent
  , refuse
  , text
  )


emptyColumns, staticColumns, staticColsEnd, totalColumns, rows :: Int
emptyColumns  = 4
staticColumns = 3
staticColsEnd = 1
totalColumns  = staticColumns + emptyColumns + staticColsEnd
rows          = 2 ^ staticColumns

showSolution, printFeedbackImmediately :: Bool
showSolution             = True
printFeedbackImmediately = True

validateSettings :: OutputCapable m => LangM m
validateSettings
  | emptyColumns < 3 = refuse $ indent $ text
      "Die Anzahl der leeren Spalten ist kleiner als die Anzahl der Hinweise."
  | totalColumns > 18 = refuse $ indent $ text $
      "Die Tabelle enthält zu viele Spalten. " ++
      "Das Eingabeformular kann bei über 18 Spalten nicht mehr korrekt angezeigt werden."
  | otherwise = pure ()


=============================================


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module TaskData (getTask) where

import Control.Monad.Random (MonadRandom)
import Data.Char (digitToInt)
import Data.List (sortOn, transpose)
import Data.String (fromString)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Numeric (showBin)
import Test.QuickCheck.Gen
import Yesod

import FlexTask.Form
import FlexTask.GenUtil (fromGen)
import LogicTasks.Formula (TruthValue(..))
import LogicTasks.Forms (tableForm)
import FindLiarTask

import Global
import TaskSettings



getTask :: MonadRandom m => m (TaskData, String, Rendered Widget)
getTask = fromGen $ do
    d@(x, y, z, _) <- taskData `suchThat` (`notElem`
        [ (('A',True),('B',False),('C',False),False) -- Task in exercise sheet 2
        , (('A',False),('B',True),('C',False),False) -- Example from lecture
        , (('A',False),('B',False),('C',False),False) -- Everybody is lying
        , (('A',False),('B',False),('C',False),True) -- Everybody is lying
        , (('A',True),('B',True),('C',True),False) -- Everybody tells the truth
        , (('A',True),('B',True),('C',True),True) -- Everybody tells the truth
        ]
      )
    let (formulas, unsortedHints) = makeHintsAndFormula d
        dataSortedOnName = sortOn fst [x, y, z]
        (formulaParts, hints) = unzip $ sortOn snd $ zip formulas unsortedHints
        solutionValues = map snd dataSortedOnName
    pure ( TaskData {hints, solutionValues, formulaParts}
         , checkers
         , form
         )
  where
    form = addCss formulaCss $
      formify @([Text], Text) Nothing
        ( list Vertical basicField [ "1) ", "2) ", "3) "]
          >-
          basic (addCssClass "formula-input" "Gesamtformel F = ")
        ) $$>
      tableForm emptyColumns rows ["A","B","C"] ["F"] $$>
      formify @(MultipleChoice Namen) Nothing
        (multipleChoiceEnum (Buttons Vertical) "Wer lügt?" (fromString . show @Namen))

    formulaCss = [cassius|
      .flex-form-div .formula-input
        width: 42%
      .truth-table
        th, td
          border: 1px solid black
          border-collapse: collapse
          text-align: center
          min-width: 2em
        table tr th:nth-child(-n+3)
          width: 20px
        table tr td:nth-child(-n+3)
          height: 25px
        table tr th:nth-child(n+4)
          width: 125px
        table tr th:last-child
          width: 25px
        table tr th:last-child
          padding: 10px 0px
      |]

checkers :: String
checkers = [i|
{-\# OPTIONS_GHC -Wno-unused-record-wildcards \#-}
{-\# LANGUAGE ApplicativeDo \#-}
{-\# LANGUAGE OverloadedStrings \#-}
{-\# LANGUAGE RecordWildCards \#-}

module Check (checkSemantics, checkSyntax) where

import Control.Monad (when)
import Data.Containers.ListUtils (nubOrd)
import Data.List (intercalate, transpose)
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.Text (Text, unpack)

import Control.OutputCapable.Blocks
import LogicTasks.Formula
  ( Formula(..)
  , TruthValue(..)
  , isSemanticEqual
  )
import Trees.Formula ()
import Trees.Print (simplestDisplay)
import Trees.Types
  ( SynTree(..)
  , BinOp(..)
  )

import Global


toAllocationInverted :: [Namen] -> [(Char,Bool)]
toAllocationInverted liars =
  map (\\x -> (nameMatch x,  x `notElem` liars)) allNamen
  where
    nameMatch A = 'A'
    nameMatch B = 'B'
    nameMatch C = 'C'

printAllocation :: [(Char,Bool)] -> String
printAllocation =
  intercalate ", " . map (\\(a,b) ->
    [a] ++ "=" ++ show (if b then (1 :: Int) else (0 :: Int)))

feedbackCompareHints :: OutputCapable m => Text -> [Namen] -> LangM m
feedbackCompareHints unmatchedHint identifiedLiars = do
  indent $ text $
    "Widerspruch gefunden. " ++
    selectionDisplay ++
    " Das passt jedoch nicht zu dem folgenden Hinweis: " ++
    unpack unmatchedHint
  where
    selectionDisplay = if null identifiedLiars
      then
        "Laut Auswahl lügt niemand."
      else
        "Die ausgewählten Lügner sind: " ++
        show identifiedLiars ++ "."


feedbackCompareChosenLiars :: OutputCapable m => [(Char, Bool)] -> SynTree BinOp Char -> LangM m
feedbackCompareChosenLiars allocationFromLiars wrongLiar = do
  indent $ text $
    "Widerspruch gefunden: Die zu der getroffenen Lügner-Auswahl gehörende Belegung ist " ++
    printAllocation allocationFromLiars ++ "." ++
    " Jedoch wertet die angegebene Teilformel " ++
    simplestDisplay wrongLiar ++
    " unter dieser Belegung zu 0 (falsch) aus."

checkSyntax :: OutputCapable m => TaskData -> Submission -> LangM m
checkSyntax TaskData{..} Submission{..} = do
  assertion (all (all (`elem` ['A','B','C']) . atomics) (submittedFormula : submittedParts)) $ text
    "Alle angegebenen Formeln enthalten nur die bekannten atomaren Formeln A, B und C?"
  when (nubOrd atomicRows /= atomicRows) $
    refuse $ indent $ text $
      "Mindestens eine Belegung kommt in der Wahrheitstafel mehrfach vor. " ++
      "Es müssen alle Belegungen genau einmal vorkommen."
  let theTable = #{startingTable} --ignore-length
  when (atomicCols == map reverse theTable) $
    refuse $ indent $ text $
      "Die Spalten der atomaren Formeln sind invertiert. " ++
      "Bitte legen Sie die Tafel so an wie in der Vorlesung vorgegeben."
  assertion (atomicCols == theTable) $ text
    "Spalten der atomaren Formeln ergeben eine korrekt geformte (aufsteigend geordnete) Wahrheitstafel?"
  #{if printFeedbackImmediately then findContradictions True "submittedParts" else ""}
  pure ()
  where
    Table xs   = submittedTable
    columns    = map snd xs
    atomicCols = take #{staticColumns} columns
    atomicRows = filter (notElem Nothing) $ transpose atomicCols

truthValuesToBinaryToIndex :: [Bool] -> Int
truthValuesToBinaryToIndex =
  foldl (\\acc value -> acc * 2 + if value then 1 else 0) 0

generateTruthTable :: [Bool] -> [[Maybe TruthValue]]
generateTruthTable solutionValues =
  let solIndex = truthValuesToBinaryToIndex solutionValues
  in [[ if i == solIndex
          then Just (TruthValue True)
          else Just (TruthValue False)
      | i <- [0..7] ]]

checkSemantics :: OutputCapable m => FilePath -> TaskData -> Submission -> Rated m
checkSemantics _ TaskData{..} Submission{..} = do
  let correctParts = zipWith isSemanticEqual formulaParts submittedParts
  yesNo (and correctParts) $ text
    "Aussagen sind korrekt übersetzt?"
  let correctFormula = isSemanticEqual submittedFormula solutionFormula ||
                       isSemanticEqual submittedFormula (foldr1 (Binary And) submittedParts)
  yesNo correctFormula $ text
    "Gesamtformel ist korrekt zusammengestellt?"
  #{if not printFeedbackImmediately then findContradictions False "submittedParts" else ""}
  let correctLast = drop #{staticColumns+emptyColumns} columns == generateTruthTable solutionValues
  yesNo correctLast $ text
    "Spalte F der Wahrheitstafel ist korrekt ausgefüllt?"
  let correctNames = identifiedLiars == listOfLiars
  yesNo correctNames $ text
    "Auswahl der Lügner ist korrekt?"
  let points = (if and correctParts then 0.4 else 0.0) +
               (if correctFormula   then 0.2 else 0.0) +
               (if correctLast      then 0.2 else 0.0) +
               (if correctNames     then 0.2 else 0.0)
  res <- printSolutionAndAssertWithMinimum (MinimumThreshold (1 % 4)) maybeAnswer points
  pure res
  where
    Table xs        = submittedTable
    solutionFormula = foldr1 (Binary And) formulaParts
    listOfLiars     = map fst $ filter (not . snd) (zip allNamen solutionValues)
    columns         = map snd xs
    maybeAnswer     = #{if showSolution then "Just (False, IndefiniteArticle, " ++ solutionCode ++ ")" else "Nothing"}
|]
  where
    solutionCode =
      [i|"Formel: " ++ simplestDisplay solutionFormula ++ "\\nKorrekte Einträge in Wahrheitstafel.\\nLügner: " ++ show listOfLiars|]

    findContradictions :: Bool -> String -> String
    findContradictions refusal submittedTrees =
      let mode = if refusal then "refuse $" else "" :: String
      in [i|
  let allocationFromLiars = toAllocationInverted identifiedLiars
      evaluated           = not . fromJust . evaluate allocationFromLiars
      wrongLiars          = [formula | formula <- #{submittedTrees}, evaluated formula]
  case wrongLiars of
    liar:_ -> #{mode} feedbackCompareChosenLiars allocationFromLiars liar
    [] -> pure ()
  let unmatchedHints = [hint | (formula, hint) <- zip formulaParts hints, evaluated formula]
  case unmatchedHints of
    hint:_ -> #{mode} feedbackCompareHints hint identifiedLiars
    [] -> pure ()
    |]

startingTable :: [[Maybe TruthValue]]
startingTable = map (Just . TruthValue . toEnum . digitToInt) <$>
  transpose (pad . (`showBin` "") <$> [0 .. rows - 1])
  where pad s = replicate (staticColumns - length s) '0' ++ s


=============================================


{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Description (description) where

import Data.Text (unpack)

import Control.OutputCapable.Blocks
import LogicTasks.Keys (basicOpKey, arrowsKey)

import Global


description :: OutputCapable m => FilePath -> TaskData -> LangM m
description _ TaskData{..} = do
  paragraph $ text
    ("Ein Logiker irrt auf dem Campus herum, um seinen Hörsaal zu finden. " ++
      "Er fragt drei Studierende (mit den ausgefallenen Namen A, B, C), wo er den Raum LB 107 finden kann. " ++
      "Diese Studierenden sagen (jeweils) immer die Wahrheit oder lügen immer. " ++
      "Um die Zuverlässigkeit der Antworten zu überprüfen, fragt er die drei Studierenden, ob die anderen lügen, " ++
      "und erhält folgende Antworten:")
  enumerateM (text . (++ ")") . show) (zip [1 ::Int ..] (map (text . unpack) hints))
  paragraph $ text
    ("Übersetzen Sie die Hinweise und die Kombination dieser jeweils in eine aussagenlogische Formel. " ++
      "Geben Sie die Formeln in die entsprechend benannten Textfelder ein.")
  paragraph $ text
    ("Wer lügt? Leiten Sie Ihr Ergebnis mittels (aufsteigend geordneter) Wahrheitstafel her. " ++
      "Füllen Sie dabei alle benannten Spalten vollständig aus. " ++
      "Sie können Hilfsspalten verwenden. Kreuzen Sie anschließend die entsprechenden Namen an.")
  paragraph $ text
    ("Verwenden Sie dabei die atomaren Formeln A, B, C mit der Interpretation, " ++
    "dass eine Zuordnung von 'wahr' dafür steht, dass die entsprechende Person die Wahrheit sagt.")
  collapsed True (translations $ do
    english "Notes on notation:"
    german "Notationshinweise:")
    (do
      basicOpKey True
      arrowsKey
      pure()
    )
  pure ()


=============================================


{-# LANGUAGE NamedFieldPuns #-}

module Parse (parseSubmission) where

import Data.List.Extra (chunksOf, transpose)

import Control.OutputCapable.Blocks
import Control.OutputCapable.Blocks.Generic (($>>=))
import Formula.Parsing.Delayed
  ( complainAboutMissingParenthesesIfNotFailingOn )
import FlexTask.Parser
  ( MultipleChoice(..)
  , Parse(..)
  , displayInputAnd
  , escaped
  , parseInstanceMultiChoice
  , parseWithFallback
  , parseWithOrReport
  , reportWithFieldNumber
  )
import LogicTasks.Formula (TruthValue)
import ParsingHelpers (fully)
import Trees.Parsing (liberalParser)
import Trees.Types (SynTree(Leaf), BinOp)

import qualified Formula.Parsing as FP

import Global
import TaskSettings


instance Parse TruthValue where
  formParser = escaped FP.parser

instance Parse (MultipleChoice Namen) where
  formParser = parseInstanceMultiChoice

makeTable :: [Maybe (SynTree BinOp Char)] -> [Maybe TruthValue] -> Table
makeTable headers values = Table $ zip allHeaders formattedTruthValues
  where
    allHeaders = map (Just . Leaf) "ABC" ++ headers ++ [Just $ Leaf 'F']
    formattedTruthValues = transpose $ chunksOf totalColumns values

parseSubmission :: (Monad m, OutputCapable (ReportT o m)) => String -> LangM' (ReportT o m) Submission
parseSubmission input =
  parseWithOrReport formParser reportWithFieldNumber input $>>= \(fs, f, headers, columns, liars) ->
    traverse parseIt fs $>>= \submittedParts ->
      parseIt f $>>= \submittedFormula ->
        traverse (traverse parseIt) headers $>>= \parsedHeaders ->
          pure $ Submission {
            submittedParts,
            submittedFormula,
            submittedTable = makeTable parsedHeaders columns,
            identifiedLiars = getChoices liars
            }
  where
    parseIt = parseWithFallback
      (fully liberalParser)
      (displayInputAnd complainAboutMissingParenthesesIfNotFailingOn)
      (fully FP.formulaSymbolParser)

=============================================

{-# language QuasiQuotes #-}
{-# language OverloadedStrings #-}
module FindLiarTask (taskData,
                     makeHintsAndFormula) where

import Data.Text (Text)
import Data.String.Interpolate (i)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen
import Trees.Types (BinOp(..), SynTree(..))
import BuildHints (hintFromFormula, isNot)


taskData :: Gen ((Char, Bool), (Char, Bool), (Char, Bool), Bool)
taskData = do
  permutation <- shuffle ['A','B','C']
  values <- vectorOf 3 arbitrary
  v <- arbitrary
  case zip permutation values of
    [(p0,v0), (p1,v1), (p2,v2)]
      -> return ((p0,v0), (p1,v1), (p2,v2), v)
    _
      -> error "This is impossible!"

makeHintsAndFormula :: ((Char, Bool), (Char, Bool), (Char, Bool), Bool) -> ([SynTree BinOp Char], [Text])
makeHintsAndFormula ((xn, xw), (yn, yw), (zn, zw), v) = (parts, hints)
  where
    xYnOrNotYn = if xw == yw        then Leaf yn       else Not (Leaf yn)
    yZnOrNotZn = if yw == zw        then Leaf zn       else Not (Leaf zn)
    zXnOrNotXn = if v               then Leaf xn       else Not (Leaf xn)
    zYnOrNotYn = if (xw == yw) == v then Not (Leaf yn) else Leaf yn

    parts = [px, py, pz]
    px = Binary Equi (Leaf xn) xYnOrNotYn
    py = Binary Equi (Leaf yn) yZnOrNotZn
    pz = Binary Equi (Leaf zn) $ (if xn > yn then flip else id) (Binary (if zw then Or else And)) zXnOrNotXn zYnOrNotYn

    hints = [hx, hy, hz]
    hx = [i|#{xn} sagt: "#{yn}#{if isNot xYnOrNotYn then " lügt" else " sagt die Wahrheit" :: String}."|]
    hy = [i|#{yn} sagt: "#{zn}#{if isNot yZnOrNotZn then " lügt" else " sagt die Wahrheit" :: String}."|]
    hz = hintFromFormula pz

=============================================

-- This is an alternative implementation of the riddle that can be used in place of the above module
-- (mind the difference in parameters for taskData!)

module FindLiarTask2 (taskData, makeHintsAndFormula) where

import Data.Text (Text)
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary (arbitrary)
import Trees.Types (BinOp(..), SynTree(..))
import BuildHints (hintFromFormula)


taskData :: Gen ((Char, Bool), (Char, Bool), (Char, Bool))
taskData = do
  permutation <- shuffle ['A','B','C']
  values <- vectorOf 3 arbitrary
  case zip permutation values of
    [(p0,v0), (p1,v1), (p2,v2)]
      -> return ((p0,v0), (p1,v1), (p2,v2))
    _
      -> error "This is impossible!"

makeHintsAndFormula :: ((Char, Bool), (Char, Bool), (Char, Bool)) -> ([SynTree BinOp Char], [Text])
makeHintsAndFormula ((xn, xw), (yn, yw), (zn, zw)) = (parts, hints)
  where
    xOperator  = if xw       then Or            else And
    xYnOrNotYn = if xw == yw then Leaf yn       else Not (Leaf yn)
    xZnOrNotZn = if xw == zw then Not (Leaf zn) else Leaf zn

    yOperator  = if yw       then Or            else And
    yXnOrNotXn = if yw == xw then Leaf xn       else Not (Leaf xn)
    yZnOrNotZn = if yw == zw then Leaf zn       else Not (Leaf zn)

    zOperator  = if zw       then And           else Or
    zXnOrNotXn = if zw == xw then Leaf xn       else Not (Leaf xn)
    zYnOrNotYn = if zw == yw then Leaf yn       else Not (Leaf yn)

    parts = [px, py, pz]
    px = Binary Equi (Leaf xn) $ (if yn > zn then flip else id) (Binary xOperator) xYnOrNotYn xZnOrNotZn
    py = Binary Equi (Leaf yn) $ (if xn > zn then flip else id) (Binary yOperator) yXnOrNotXn yZnOrNotZn
    pz = Binary Equi (Leaf zn) $ (if xn > yn then flip else id) (Binary zOperator) zXnOrNotXn zYnOrNotYn

    hints = [hintFromFormula px, hintFromFormula py, hintFromFormula pz]

=============================================

{-# language QuasiQuotes #-}
{-# language OverloadedStrings #-}

module BuildHints where

import Data.Text (Text)
import Data.String.Interpolate (i)
import Trees.Types (BinOp(..), SynTree(..))

hintFromFormula :: SynTree BinOp Char -> Text
hintFromFormula (Binary Equi (Leaf nameA) (Binary operator b c)) =
  [i|#{nameA} sagt: "#{nameFromLeaf b}#{word1}#{operatorName} #{nameFromLeaf c}#{word2}."|]
  where
    operatorName :: String
      | operator == Or                        = " oder"
      | operator == And && isNot b == isNot c = " und"
      | otherwise                             = ", aber"

    word1 :: String
      | isNot b == isNot c = ""
      | isNot b            = " lügt"
      | otherwise          = " sagt die Wahrheit"

    word2 :: String
      | isNot b && isNot c = " lügen"
      | isNot b            = " sagt die Wahrheit"
      |            isNot c = " lügt"
      | otherwise          = " sagen die Wahrheit"

    nameFromLeaf :: SynTree BinOp Char -> String
    nameFromLeaf (Leaf name)       = [name]
    nameFromLeaf (Not (Leaf name)) = [name]
    nameFromLeaf _                 = error "not a Leaf and not a Not Leaf"

hintFromFormula _ = error "formula not supported"

isNot :: SynTree b c -> Bool
isNot (Not _) = True
isNot _       = False

