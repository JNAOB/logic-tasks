
taskName: Markierungsalgorithmus
validation: Validate
=============================================

{-# LANGUAGE DeriveDataTypeable #-}

module Global where

import Data.Data (Data)
import Data.List.Extra (replace)

import LogicTasks.Formula (TruthValue)
import Trees.Types (SynTree, BinOp)
import FlexTask.Form (SingleChoiceSelection)


newtype CharAnswer = CharAnswer {unCharAnswer :: [Char]}
    deriving (Eq, Show)

data Submission = Submission
    { steps :: [Maybe CharAnswer]
    , output :: SingleChoiceSelection
    , model :: Maybe [(Char,TruthValue)]
    }

data Solution = Solution
    { correctSteps :: [(Int, [Char])]
    , correctOutput :: Bool
    , correctModel :: [(Char,Bool)]
    } deriving Data

data TaskData = TaskData
    { formula :: SynTree BinOp Char
    , solution :: Solution
    } deriving Data

withUnicodeImpl :: String -> String
withUnicodeImpl = replace "=>" "\\Rightarrow"

=============================================


module TaskSettings where

import Data.List ((\\))

import Control.OutputCapable.Blocks
import Horn
import Trees.Types (SynTree(..), BinOp(..))


atomics :: [Char]
atomics = getAllAtomics spirit

stepFields :: Int
stepFields =  length atomics + extraClauses

spirit :: [SynTree BinOp Char]
spirit = v3

extraClauses :: Int
extraClauses = 0

showSolution, printFeedbackImmediately :: Bool
showSolution = True
printFeedbackImmediately = True

minimumPoints :: Double
minimumPoints = 0.25

validateSettings :: OutputCapable m => LangM m
validateSettings
    | not (isHornFormulaI (foldr1 (Binary And) spirit)) = refuse $ indent $ text
        "Als spirit muss eine Liste von Hornklauseln angegeben werden."
    | extraClauses > length atomics = refuse $ indent $ text $
        "Es können nur so viele extra Klauseln hinzugefügt werden, " ++
            "wie in spirit bereits enthalten sind."
    | (atomics \\ ['A' .. 'L']) /= [] = refuse $ indent $ text $
        "Es dürfen nur Buchstaben aus dem Intervall ['A' .. 'L'], " ++
            "für die atomaren Formeln verwendet werden."
    | otherwise = pure ()


=============================================


{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TaskData (getTask) where

import Control.Monad.Random (MonadRandom)
import Data.String.Interpolate (i)
import Data.Text (Text, pack)
import Yesod
    ( RenderMessage(..)
    , SomeMessage(..)
    , fieldSettingsLabel
    )

import FlexTask.Form
import FlexTask.GenUtil (fromGen)
import Horn (makeHornFormula, startAlgorithm)

import Global
import TaskSettings


data Label = Step Int | Output | Model | Satisfiable | Unsatisfiable

instance RenderMessage a Label where
    renderMessage _ ("de":_) (Step n)      = pack ("Schritt " ++ show n ++ ":")
    renderMessage _ ("de":_) Output        = "Ausgabe:"
    renderMessage _ ("de":_) Model         = "Modell:"
    renderMessage _ ("de":_) Satisfiable   = "erfüllbar"
    renderMessage _ ("de":_) Unsatisfiable = "unerfüllbar"
    renderMessage _ _        (Step n)      = pack ("Step " ++ show n ++ ":")
    renderMessage _ _        Output        = "Output:"
    renderMessage _ _        Model         = "Model:"
    renderMessage _ _        Satisfiable   = "satisfiable"
    renderMessage _ _        Unsatisfiable = "unsatisfiable"

getTask :: MonadRandom m => m (TaskData, String, Rendered Widget)
getTask = fromGen $ do
    formula <- makeHornFormula spirit extraClauses
    let (steps, output, model) = startAlgorithm formula
        solution = Solution
            { correctSteps = steps
            , correctOutput = output
            , correctModel = model
            }
    pure (TaskData {formula, solution}, checkers, formify Nothing form)

form :: CompleteForm ([Maybe Text], SingleChoiceSelection, Maybe Text)
form =
  list Vertical basicField (map (fieldSettingsLabel . Step) [1..stepFields])
  >-
    singleChoice Dropdown (fieldSettingsLabel Output)
      [ SomeMessage Satisfiable
      , SomeMessage Unsatisfiable
      ]
    >| basic (fieldSettingsLabel Model)

checkers :: String
checkers = [i|

{-\# OPTIONS_GHC -Wno-unused-record-wildcards \#-}
{-\# LANGUAGE ApplicativeDo \#-}
{-\# LANGUAGE RecordWildCards \#-}
{-\# LANGUAGE LambdaCase \#-}

module Check (checkSyntax, checkSemantics) where

import Data.Foldable (traverse_)
import Data.List (intersperse, intercalate, sort, (\\\\))
import Data.Map (Map)
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import Data.Tuple.Extra (second)

import Control.Monad (unless, when)
import Control.Monad.State (State)
import Control.OutputCapable.Blocks
import FlexTask.Form (getAnswer)
import Horn
import LogicTasks.Formula (TruthValue(..))
import Trees.Types (BinOp(..), SynTree(..))
import Trees.Print (simplestDisplay)

import qualified Data.List.Extra as List (replace)

import Global


checking :: OutputCapable m => Bool -> State (Map Language String) () -> LangM m
checking condition msg = when condition $ refuse $ indent $ translate msg

buildLatex :: OutputCapable m => SynTree BinOp Char -> [(Int, [Char])] -> LangM m
buildLatex formula steps = do
    paragraph $ indent $ latex $ withUnicodeImpl $ foldr
      (\\(i,c) acc -> List.replace [c] ("\\\\underline{" ++ [c] ++ "^" ++ show i ++ "}") acc)
      (simplestDisplay formula)
      $ concatMap (\\(i,s) -> map (i,) s) steps
    paragraph $ traverse_ (\\(i,c) -> indent $ translate $ do
        german $  "Schritt " ++ show i ++ ": " ++ intersperse ',' c ++ "\\n"
        english $ "Step " ++ show i ++ ": " ++ intersperse ',' c ++ "\\n") steps
    pure ()

displayAllocation :: (Char,Bool) -> String
displayAllocation (c,w) = "\\\\alpha(" ++ [c] ++  ")=" ++ show (fromEnum w)


checkSyntax :: OutputCapable m => TaskData -> Submission -> LangM m
checkSyntax TaskData{..} Submission{..} = do
    checking (all isNothing steps && isNothing model) $ do
        german "Es dürfen nicht alle Texteingabefelder leer sein."
        english "Not all text input fields may be left blank."
    checking (any isJust (dropWhile isJust steps)) $ do
        german "Es dürfen keine Schritte übersprungen werden."
        english "Steps cannot be skipped."
    checking (getAnswer output == 2 && isJust model) $ do
        german $ "Widerspruch gefunden: " ++
            "Die Formel sei unerfüllbar, dennoch wurde eine erfüllende Belegung (Modell) angegeben."
        english $ "Contradiction found: " ++
            "The formula is said to be unsatisfiable, yet a satisfying assignment (model) was provided."

    paragraph $ translate $ do
        german "Die Einsendung wird interpretiert als:"
        english "The submission is interpreted as:"
    buildLatex formula stepsSubmitted

    paragraph $ indent $ do
        translate $ do
          german $ germanOutput ++ germanWith
          english $ englishOutput ++ englishWith
        modelDisplay
        pure ()
    #{if printFeedbackImmediately then feedbackCode else ""}

    pure ()
  where
    stepsSubmitted = zip [1..] (map unCharAnswer (catMaybes steps))
    (germanOutput, englishOutput) = case getAnswer output of
      2 -> ("\\"unerfüllbar\\"","\\"unsatisfiable\\"")
      1 -> ("\\"erfüllbar\\"","\\"satisfiable\\"")
      _ -> ("","")
    (germanWith, englishWith, modelDisplay) = case model of
      Nothing -> ("","",pure ())
      Just m  -> ( ", mit"
                 , ", with"
                 , latex $ intercalate ",\\\\ " $
                     map (displayAllocation . second truth) m
                 )


checkSemantics :: OutputCapable m => FilePath -> TaskData -> Submission -> Rated m
checkSemantics _ TaskData{solution = Solution{..},..} Submission{..} = do
    let (altSteps,_,altModel) = newSolution formula stepsSubmitted
    let stepsCorrect = map (sort . snd) altSteps == map (sort . snd) stepsSubmitted
    yesNo stepsCorrect $ translate $ do
        german "Schritte richtig?"
        english "Steps correct?"
    let outputCorrect = (if correctOutput then 1 else 2) == getAnswer output
    yesNo outputCorrect $ translate $ do
        german "Ausgabe richtig?"
        english "Output correct?"
    let modelCorrect = altModel == sort (maybe [] (map (second truth)) model)
    yesNo modelCorrect $ translate $ do
        german "Modell richtig?"
        english "Model correct?"
    let points = (if stepsCorrect then 0.4 else 0.0) +
                 (if outputCorrect then 0.2 else 0.0) +
                 (if modelCorrect then 0.4 else 0.0)
    #{if showSolution then solutionCode else ""}
    #{if printFeedbackImmediately then "" else feedbackCode}

    unless (points >= #{minimumPoints}) $ refuse $ pure ()
    return points
  where
    stepsSubmitted = zip [1 :: Int ..] (map unCharAnswer (catMaybes steps))
|]
  where
    feedbackCode :: String
    feedbackCode = [i|
    let marked = map unCharAnswer $ catMaybes steps
        clauses = getClauses formula
        allAtomics = sort $ getAllAtomics clauses
    checking ((sort (concat marked) \\\\ allAtomics) /= []) $ do
        german $ "Hinweis: Markiert werden können nur jene atomare Formeln, " ++
            "welche in der Formel vorkommen und nicht bereits markiert worden sind."
        english "Hint: Only those atomic formulas that occur in the formula and are not yet marked can be marked."
    checking (any ((> 1) . length) (drop 1 marked)) $ do
        german "Hinweis: Mit Ausnahme von Schritt 1, darf immer nur eine atomare Formel pro Schritt markiert werden."
        english "Hint: Apart from step 1, exactly one atomic formula may be marked in each step."
    checking ((\\case
        (Just step : _) -> sort (getFacts clauses) /= sort (unCharAnswer step)
        _               -> False) steps) $ do
            german "Hinweis: Im ersten Schritt müssen alle Fakten markiert werden."
            english "Hint: In the first step, all facts must be marked."
    checking (isJust model && sort (map fst $ fromJust model) /= allAtomics) $ do
        german "Hinweis: Im Modell muss jede atomare Teilformel genau einmal vorkommen."
        english "Hint: The model must contain each atomic subformula exactly once."
    |]

    solutionCode :: String
    solutionCode = [i|
    when (points /= 1) $ paragraph $ do
        translate $ do
            english "A correct solution is:"
            german "Eine korrekte Lösung ist:"
        buildLatex formula correctSteps
        paragraph $ do
            indent $ translate $ do
                german $ if correctOutput then "\\"erfüllbar\\", mit" else "\\"unerfüllbar\\""
                english $ if correctOutput then "\\"satisfiable\\", mit" else "\\"unsatisfiable\\""
            case correctModel of
                [] -> pure ()
                m  -> indent $ latex $ intercalate ",\\\\ " $
                    map displayAllocation $ sort m
            pure ()
        pure ()
|]


=============================================


{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Description (description) where

import Control.OutputCapable.Blocks
import Trees.Print (simplestDisplay)

import Global


description :: OutputCapable m => FilePath -> TaskData -> LangM m
description _ TaskData{..} = do
    paragraph $ translate $ do
        german "Gegeben ist eine Hornformel, welche sich bereits in Implikationsschreibweise befindet:"
        english "A Horn formula is given, which is already in implicational form."
    indent $ latex $ withUnicodeImpl $ simplestDisplay formula
    paragraph $ translate $ do
        german "Wenden Sie den Markierungsalgorithmus aus der Vorlesung auf die Formel an."
        english "Apply the marking algorithm from the lecture to the formula."
    indent $ translate $ do
        german "Vervollständigen Sie das Protokoll."
        english "Complete the protocol."
    paragraph $ translate $ do
        english "Use the following notations:"
        german "Beachten Sie die folgenden Schreibweisen:"
    indent $ translate $ do
        english "Example of a listing of the atomic formulas marked in a step:"
        german "Beispiel für die Angabe der in einem Schritt markierten atomaren Formeln:"
    code "A, B"
    indent $ translate $ do
        english "Example of a Model:"
        german "Beispiel für die Angabe eines Modells:"
    code "A:1, B:0"
    paragraph $ translate $ do
        german "Eingabefelder, die für die Lösung nicht erforderlich sind, können freigelassen werden."
        english "You can leave input fields blank if they’re not needed for the solution."
    pure ()


=============================================


{-# LANGUAGE NamedFieldPuns #-}

module Parse (parseSubmission) where

import Text.Parsec

import Control.OutputCapable.Blocks
  ( LangM'
  , ReportT
  , OutputCapable
  )
import Control.OutputCapable.Blocks.Generic (($>>=))
import FlexTask.Parser
  ( Parse(..)
  , escaped
  , formParser
  , parseWithOrReport
  , reportWithFieldNumber
  )
import LogicTasks.Formula (TruthValue(..))

import qualified LogicTasks.Parsing as P

import Global


instance Parse CharAnswer where
  formParser = escaped $ do CharAnswer
     <$>    (do spaces
                c <- satisfy (`elem` ['A' .. 'Z'])
                spaces
                pure c) `sepBy` char ','

instance Parse [(Char,TruthValue)] where
  formParser = escaped $ do
    result <- tupleParser `sepBy` char ','
    pure result
    where
      tupleParser = do
        spaces
        c <- satisfy (`elem` ['A'..'Z'])
        spaces
        _ <- char ':'
        spaces
        t <- P.parser
        spaces
        pure (c,t)

parseSubmission ::
  (Monad m, OutputCapable (ReportT o m))
  => String
  -> LangM' (ReportT o m) Submission
parseSubmission input = parseWithOrReport formParser reportWithFieldNumber input $>>= \(steps, output, model) ->
  pure $ Submission {steps, output, model}


==============================================

module Horn where

import Data.Char (toLower)
import Data.Containers.ListUtils (nubOrd)
import Data.List.Extra (notNull, sort)
import Test.QuickCheck.Gen

import Trees.Types (BinOp(..), SynTree(..))
import Trees.Helpers (collectLeaves)


type Protocol = [(Int,[Char])]
type Allocation = [(Char,Bool)]


v1, v2 :: [SynTree BinOp Char]
v1 =
  [ Binary Impl (Leaf 'B') (Leaf 'A')
  , Binary Impl (Leaf '1') (Leaf 'B')
  , Binary Impl (Leaf 'C') (Leaf 'A')
  , Binary Impl (Leaf 'C') (Leaf '0')
  ]
v2 =
  [ Binary Impl (Leaf 'A') (Leaf 'B')
  , Binary Impl (Leaf '1') (Leaf 'A')
  , Binary Impl (Binary And (Leaf 'B') (Leaf 'A')) (Leaf '0')
  , Binary Impl (Leaf 'D') (Leaf '0')
  ]

v3, v4 :: [SynTree BinOp Char]
v3 =
  [ Binary Impl (Leaf '1') (Leaf 'A')
  , Binary Impl (Leaf '1') (Leaf 'B')
  , Binary Impl (Leaf 'A') (Leaf 'C')
  , Binary Impl (Leaf 'B') (Leaf 'D')
  , Binary Impl (Leaf 'D') (Leaf 'E')
  , Binary Impl (Leaf 'E') (Leaf '0')
  ]

v4 =
  [ Binary Impl (Leaf '1') (Leaf 'A')
  , Binary Impl (Leaf '1') (Leaf 'B')
  , Binary Impl (Leaf 'A') (Leaf 'C')
  , Binary Impl (Leaf 'B') (Leaf 'D')
  , Binary Impl (Leaf 'D') (Leaf 'E')
  , Binary Impl (Leaf 'F') (Leaf '0')
  ]

makeHornFormula :: [SynTree BinOp Char] -> Int -> Gen (SynTree BinOp Char)
makeHornFormula spirit extra = do
    permutation <- shuffle spirit
    let withAdded = concatMap addClause $ zip (take extra permutation) ['M'..'Z']
    clauses <- shuffle (withAdded ++ drop extra permutation)
    let lowerCaseClauses = map (fmap toLower) clauses
    let formula = foldr1 (Binary And) lowerCaseClauses
    atomics <- shuffle (getAllAtomics lowerCaseClauses)
    return (foldl (flip (uncurry replace)) formula (zip atomics ['A'..'Z']))
  where
    addClause (Binary Impl a b, x) = [Binary Impl a (Leaf x), Binary Impl (Leaf x) b]
    addClause _ = []


isHornFormulaI :: SynTree BinOp c -> Bool
isHornFormulaI =  all isHornClauseI . getClauses

isHornClauseI :: SynTree BinOp c -> Bool
isHornClauseI (Binary Impl a (Leaf _)) = case a of
    Leaf _ -> True
    (Binary And x y) -> isConj x && isConj y
    _ -> False
  where
    isConj (Leaf _) = True
    isConj (Binary And x y) = isConj x && isConj y
    isConj _ = False
isHornClauseI _ = False

getAllAtomics :: [SynTree BinOp Char] -> [Char]
getAllAtomics clauses = nubOrd $ concatMap (filter (`notElem` ['0', '1']) . collectLeaves) clauses

getClauses :: SynTree BinOp c -> [SynTree BinOp c]
getClauses (Binary And leftPart rightPart) = getClauses leftPart ++ getClauses rightPart
getClauses formula = [formula]

isFact :: SynTree BinOp Char -> Bool
isFact (Binary Impl (Leaf '1') (Leaf _)) = True
isFact _ = False

charFromFact :: SynTree BinOp Char -> Char
charFromFact (Binary Impl (Leaf '1') (Leaf a)) = a
charFromFact _ = error "Cannot get Char from not a fact."

getFacts :: [SynTree BinOp Char] -> [Char]
getFacts = map charFromFact . filter isFact


startAlgorithm :: SynTree BinOp Char -> (Protocol,Bool,Allocation)
startAlgorithm formula = markingAlg modifiedClauses [(1, facts) | notNull facts]
  where
    facts = getFacts clauses
    clauses = getClauses formula
    modifiedClauses = foldl doStep clauses facts

markingAlg :: [SynTree BinOp Char] -> Protocol -> (Protocol,Bool,Allocation)
markingAlg clauses protocol = case nextToMark clauses of
    Nothing   -> (protocol, True, buildModel protocol clauses)
    Just '0'  -> (protocol, False, [])
    Just fact -> markingAlg (doStep clauses fact) (addStep fact protocol)

buildModel :: Protocol -> [SynTree BinOp Char] -> Allocation
buildModel protocol clauses = sort $ trueAtoms ++ falseAtoms
  where
    trueAtoms = [(c, True) | (_, cs) <- protocol, c <- cs]
    allAtoms = getAllAtomics clauses
    falseAtoms = [(c, False) | c <- allAtoms, c `notElem` map fst trueAtoms]


addStep :: Char -> Protocol -> Protocol
addStep marked protocol = protocol ++ [(step,[marked])]
  where
    step = (\(prevStep,_) -> prevStep + 1) $ last protocol

nextToMark :: [SynTree BinOp Char] -> Maybe Char
nextToMark clauses = case getFacts clauses of
    []    -> Nothing
    (c:_) -> Just c

doStep :: [SynTree BinOp Char] -> Char -> [SynTree BinOp Char]
doStep clauses fact = simplify $ map (replace fact '1') clauses

replace :: Eq a => a -> a -> SynTree BinOp a -> SynTree BinOp a
replace x y = fmap (\a -> if a == x then y else a)

simplify :: [SynTree BinOp Char] -> [SynTree BinOp Char]
simplify clauses = if appliedOnce == appliedTwice then appliedOnce else simplify appliedTwice
  where
    appliedOnce = concatMap removeOnes clauses
    appliedTwice = concatMap removeOnes appliedOnce

removeOnes :: SynTree BinOp Char -> [SynTree BinOp Char]
removeOnes tree = case tree of
    Binary Impl (Leaf '1') (Leaf '1')                -> []
    Binary Impl (Binary And (Leaf '1') (Leaf '1')) b -> [Binary Impl (Leaf '1') b]
    Binary Impl (Binary And (Leaf '1') a) b          -> [Binary Impl a b]
    Binary Impl (Binary And a (Leaf '1')) b          -> [Binary Impl a b]
    _                                                -> [tree]

newSolution :: SynTree BinOp Char -> Protocol -> (Protocol, Bool, Allocation)
newSolution formula protocol =
    if case protocol of
       [] -> True
       (_, chars):_ -> sort chars /= sort facts
    then sampleSolution
        else
            case foldl tryStep (Just allClauses) steps of
                Just a -> if '0' `elem` getFacts a then (protocol, False, [])
                    else markingAlg a protocol
                _      -> sampleSolution
  where
    sampleSolution = startAlgorithm formula
    facts = getFacts allClauses
    allClauses = getClauses formula
    steps = concatMap snd protocol
    tryStep :: Maybe [SynTree BinOp Char] -> Char -> Maybe [SynTree BinOp Char]
    tryStep (Just clauses) c = if Binary Impl (Leaf '1') (Leaf c) `elem` clauses
        then Just (doStep clauses c)
        else Nothing
    tryStep Nothing        _ = Nothing
