{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module LogicTasks.Semantics.Resolve where


import Data.Set (fromList, member, toList, unions)
import Control.Monad.State (State)
import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  Language,
  OutputCapable,
  english,
  german,
  translate,
  translations,
  localise,
  yesNo,
  recoverFrom,
  )
import Data.List (intercalate, sort)
import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Tuple.Extra (thd3)
import Test.QuickCheck (Gen)

import Config (ResolutionConfig(..), ResolutionInst(..), BaseConfig(..))
import Formula.Util (isEmptyClause, mkCnf, sat)
import Formula.Resolution (applySteps, genRes, resolvableWith, resolve)
import Formula.Types (Clause, ResStep(..), literals)
import LogicTasks.Helpers (example, extra, keyHeading, negationKey, orKey)
import LogicTasks.Semantics.Step (showClause)
import Util (checkBaseConf, prevent, preventWithHint)
import Control.Monad (unless, when)
import Control.Applicative (Alternative)
import Data.Foldable.Extra (notNull)
import Formula.Parsing.Delayed (Delayed, withDelayed, complainAboutWrongNotation, withDelayedSucceeding)
import Formula.Parsing (resStepsParser, clauseSetParser, clauseFormulaParser)
import Formula.Helpers (showCnfAsSet)



genResInst :: ResolutionConfig -> Gen ResolutionInst
genResInst ResolutionConfig{ baseConf = BaseConfig{..}, ..} = do
  (clauses,solution) <- inst
  pure $ ResolutionInst {
    clauses = clauses,
    solution,
    printFeedbackImmediately = printFeedbackImmediately,
    usesSetNotation = useSetNotation,
    showSolution = printSolution,
    addText = extraText,
    unicodeAllowed = offerUnicodeInput
  }
  where
    inst = genRes (minClauseLength, maxClauseLength) minSteps usedAtoms



baseDescription
  :: OutputCapable m
  => State (Map Language String) ()
  -> State (Map Language String) ()
  -> State (Map Language String) ()
  -> State (Map Language String) ()
  -> ResolutionInst
  -> LangM m
baseDescription howToInput howToHandleNumbers exampleSet exampleFormula ResolutionInst{..} = do
  paragraph $ do
    translate $ do
      german $ "Betrachten Sie die folgende " ++ gerSet ++ "Formel in KNF:"
      english $ "Consider the following " ++ engSet ++ "formula in cnf:"
    indent $ code $ show' clauses
    pure ()
  paragraph $ translate $ do
    german "Führen Sie das Resolutionsverfahren an ihr durch, um die leere Klausel abzuleiten."
    english "Use the resolution technique on it to derive the empty clause."

  paragraph $ translate howToInput
  keyHeading
  negationKey unicodeAllowed
  unless usesSetNotation (orKey unicodeAllowed)

  when usesSetNotation $ paragraph $ indent $ do
    translate $ do
      german "Nicht-leere Klausel:"
      english "Non-empty clause:"
    code "{ ... }"
    pure ()

  paragraph $ indent $ do
    translate $ do
      german "Leere Klausel:"
      english "Empty clause:"
    code "{ }"
    pure ()
  paragraph $ translate $ do
    german "Optional können Sie Klauseln auch durch Nummern ersetzen."
    english "You can optionally replace clauses with numbers."

  paragraph $ translate $ do
    german "Bestehende Klauseln sind bereits ihrer Reihenfolge nach nummeriert. (erste Klausel = 1, zweite Klausel = 2, ...)."
    english "Existing Clauses are already numbered by their order. first clause = 1, second clause = 2, ...)."

  paragraph $ translate howToHandleNumbers
  when usesSetNotation $ paragraph $ indent $ do
    translate $ do
      german "Nutzen Sie zur Angabe der Klauseln die Mengenschreibweise! Ein Lösungsversuch könnte beispielsweise so aussehen: "
      english "Specify the clauses using set notation! A solution attempt could look like this: "
    translatedCode $ flip localise $ translations exampleSet
    pure ()

  unless usesSetNotation $ paragraph $ indent $ do
    translate $ do
      german "Nutzen Sie zur Angabe der Klauseln die Formelschreibweise! Ein Lösungsversuch könnte beispielsweise so aussehen: "
      english "Specify the clauses using the formula notation! A solution attempt could look like this: "
    translatedCode $ flip localise $ translations exampleFormula
    pure ()

  extra addText
  pure ()
    where
      show' = if usesSetNotation
        then showCnfAsSet . mkCnf
        else show . mkCnf

      (gerSet,engSet)
        | usesSetNotation =
          ( "Mengenschreibweise einer "
          , "set notation of a "
          )
        | otherwise = ( "", "")


descriptionMultipleFields :: OutputCapable m => State (Map Language String) () -> ResolutionInst -> LangM m
descriptionMultipleFields explainEmptySteps resInst@ResolutionInst{..} = baseDescription
    (do
      german "Geben Sie die Lösung als eine Auflistung von Schritten an. "
      german "Ein Schritt besteht aus den zwei verwendeten Klauseln sowie der daraus entstehenden Resolvente. "
      german "Füllen Sie die leeren Eingabefelder für Ihre Schritte aus. "
      english "Provide the solution as a sequence of steps. "
      english "A step consists of the two clauses used and the resulting resolvent. "
      english "Fill in the empty input fields for your steps. "
      explainEmptySteps
    )
    (do
      german "Neu resolvierte Klauseln erhalten automatisch die Nummer rechts neben ihrem Eingabefeld."
      english "Newly resolved clauses are automatically assigned the number directly to the right of their input field."
    )
    (setExample unicodeAllowed False)
    (exampleCode unicodeAllowed False)
    resInst


description :: OutputCapable m => ResolutionInst -> LangM m
description resInst@ResolutionInst{..} = baseDescription
    (do
      german "Geben Sie die Lösung als eine Liste von Tripeln an, wobei diese folgendermaßen aufgebaut sind: (Erste Klausel, Zweite Klausel, Resolvente)"
      english "Provide the solution as a list of triples with this structure: (first clause, second clause, resolvent)."
    )

    (do
      german "Neu resolvierte Klauseln können mit einer Nummer versehen werden, indem Sie '= NUMMER' an diese anfügen."
      english "Newly resolved clauses can be associated with a number by attaching '= NUMBER' behind them."
    )
    (setExample unicodeAllowed True)
    (exampleCode unicodeAllowed True)
    resInst


descriptionFlex :: OutputCapable m => ResolutionInst -> LangM m
descriptionFlex = descriptionMultipleFields $ do
  german "Schritte können nicht partiell ausgefüllt werden. Wenn Sie einen Schritt hinzufügen, muss dieser komplett sein. "
  german "Bei Nichtbeachtung wird Ihre Abgabe aus Syntaxgründen abgelehnt. "
  german "Es ist aber erlaubt, Schritte komplett wegzulassen, z.B. wenn Sie weniger Schritte benötigen als im Eingabeformular angegeben."
  english "Steps cannot be filled in partially. Each added step must be complete. "
  english "Submissions containing partially filled steps will be rejected as syntactically wrong. "
  english "You are allowed to entirely leave out steps, e.g., if your solution needs fewer steps overall than provided in the input form."


exampleCode :: Bool -> Bool -> State (Map Language String) ()
exampleCode unicode oneInput
  | unicode && oneInput = do
      english "[(1, 2, A), (3, 4, ¬A ∨ ¬B = 6), (5, 6, not A), (A, not A, {})]"
      german "[(1, 2, A), (3, 4, ¬A ∨ ¬B = 6), (5, 6, nicht A), (A, nicht A, {})]"
  | not unicode && oneInput = do
      english "[(1, 2, A), (3, 4, -A or -B = 6), (5, 6, not A), (A, not A, {})]"
      german "[(1, 2, A), (3, 4, -A oder -B = 6), (5, 6, nicht A), (A, nicht A, {})]"
  | unicode && not oneInput = do
      english $ unlines
        [ "Step 1: First Clause: 1, Second Clause:     2, Resolvent: A       = 6"
        , "Step 2: First Clause: 3, Second Clause:     4, Resolvent: ¬A ∨ ¬B = 7"
        , "Step 3: First Clause: 5, Second Clause:     7, Resolvent: not A   = 8"
        , "Step 4: First Clause: A, Second Clause: not A, Resolvent: {}      = 9"
        ]
      german $ unlines
        [ "Schritt 1: Erste Klausel: 1, Zweite Klausel:       2, Resolvente: A       = 6"
        , "Schritt 2: Erste Klausel: 3, Zweite Klausel:       4, Resolvente: ¬A ∨ ¬B = 7"
        , "Schritt 3: Erste Klausel: 5, Zweite Klausel:       7, Resolvente: nicht A = 8"
        , "Schritt 4: Erste Klausel: A, Zweite Klausel: nicht A, Resolvente: {}      = 9"
        ]
  | otherwise = do
      english $ unlines
        [ "Step 1: First Clause: 1, Second Clause:     2, Resolvent: A        = 6"
        , "Step 2: First Clause: 3, Second Clause:     4, Resolvent: -A or -B = 7"
        , "Step 3: First Clause: 5, Second Clause:     7, Resolvent: not A    = 8"
        , "Step 4: First Clause: A, Second Clause: not A, Resolvent: {}       = 9"
        ]
      german $ unlines
        [ "Schritt 1: Erste Klausel: 1, Zweite Klausel:       2, Resolvente: A          = 6"
        , "Schritt 2: Erste Klausel: 3, Zweite Klausel:       4, Resolvente: -A oder -B = 7"
        , "Schritt 3: Erste Klausel: 5, Zweite Klausel:       7, Resolvente: nicht A    = 8"
        , "Schritt 4: Erste Klausel: A, Zweite Klausel: nicht A, Resolvente: {}         = 9"
        ]


setExample :: Bool -> Bool -> State (Map Language String) ()
setExample unicode oneInput
  | unicode && oneInput = do
      english "[(1, 2, {A}), (3, 4, {¬A, ¬B} = 6), (5, 6, {not A}), ({A}, {not A}, {})]"
      german "[(1, 2, {A}), (3, 4, {¬A, ¬B} = 6), (5, 6, {nicht A}), ({A}, {nicht A}, {})]"
  | not unicode && oneInput = do
      english "[(1, 2, {A}), (3, 4, {-A, -B} = 6), (5, 6, {not A}), ({A}, {not A}, {})]"
      german "[(1, 2, {A}), (3, 4, {-A, -B} = 6), (5, 6, {nicht A}), ({A}, {nicht A}, {})]"
  | unicode && not oneInput = do
      english $ unlines
        [ "Step 1: First Clause:   1, Second Clause:       2, Resolvent: {A}       = 6"
        , "Step 2: First Clause:   3, Second Clause:       4, Resolvent: {¬A, ¬B}  = 7"
        , "Step 3: First Clause:   5, Second Clause:       7, Resolvent: {not A}   = 8"
        , "Step 4: First Clause: {A}, Second Clause: {not A}, Resolvent: {}        = 9"
        ]
      german $ unlines
        [ "Schritt 1: Erste Klausel:   1, Zweite Klausel:         2, Resolvente: {A}       = 6"
        , "Schritt 2: Erste Klausel:   3, Zweite Klausel:         4, Resolvente: {¬A, ¬B}  = 7"
        , "Schritt 3: Erste Klausel:   5, Zweite Klausel:         7, Resolvente: {nicht A} = 8"
        , "Schritt 4: Erste Klausel: {A}, Zweite Klausel: {nicht A}, Resolvente: {}        = 9"
        ]
  | otherwise = do
      english $ unlines
        [ "Step 1: First Clause:   1, Second Clause:       2, Resolvent: {A}        = 6"
        , "Step 2: First Clause:   3, Second Clause:       4, Resolvent: {-A, -B}   = 7"
        , "Step 3: First Clause:   5, Second Clause:       7, Resolvent: {not A}    = 8"
        , "Step 4: First Clause: {A}, Second Clause: {not A}, Resolvent: {}         = 9"
        ]
      german $ unlines
        [ "Schritt 1: Erste Klausel:   1, Zweite Klausel:         2, Resolvente: {A}          = 6"
        , "Schritt 2: Erste Klausel:   3, Zweite Klausel:         4, Resolvente: {-A, -B}     = 7"
        , "Schritt 3: Erste Klausel:   5, Zweite Klausel:         7, Resolvente: {nicht A}    = 8"
        , "Schritt 4: Erste Klausel: {A}, Zweite Klausel: {nicht A}, Resolvente: {}           = 9"
        ]


verifyStatic :: OutputCapable m => ResolutionInst -> LangM m
verifyStatic ResolutionInst{..}
    | any isEmptyClause clauses =
        refuse $ indent $ translate $ do
          german "Mindestens eine der Klauseln ist leer."
          english "At least one of the clauses is empty."

    | sat $ mkCnf clauses =
        refuse $ indent $ translate $ do
          german "Diese Formel ist erfüllbar."
          english "This formula is satisfiable."

    | otherwise = pure()



verifyQuiz :: OutputCapable m => ResolutionConfig -> LangM m
verifyQuiz ResolutionConfig{..}
    | minSteps < 1 =
        refuse $ indent $ translate $ do
          german "Die Mindestanzahl an Schritten muss größer als 0 sein."
          english "The minimal amount of steps must be greater than 0."

    | maxClauseLength baseConf == 1 && minSteps > 1 =
        refuse $ indent $ translate $ do
          german "Mit Klauseln der Länge 1 kann nicht mehr als ein Schritt durchgeführt werden."
          english "More than one step using only length 1 clauses is not possible."

    | minSteps > 2 * length (usedAtoms baseConf) =
        refuse $ indent $ translate $ do
          german "Diese minimale Anzahl Schritte kann mit den gegebenen atomaren Formeln nicht durchgeführt werden."
          english "This amount of steps is impossible with the given amount of atomic formulas."

    | printFeedbackImmediately && printSolution =
        refuse $ indent $ translate $ do
          german "Wenn sofortiges Feedback eingeschaltet ist, kann nicht abschließend die korrekte Lösung angezeigt werden."
          english "If instant feedback is turned on, then the correct solution cannot be displayed afterwards."

    | otherwise = checkBaseConf baseConf



start :: [ResStep]
start = []

gradeSteps :: OutputCapable m => Bool -> [(Clause,Clause,Clause)] -> Bool -> LangM m
gradeSteps setNotation steps appliedIsNothing = do
    preventWithHint (notNull noResolveSteps)
        (translate $ do
          german "Jeder Schritt ist korrekt?"
          english "Each step is correct?"
        )
        (paragraph $ do
          translate $ do
            german "Mindestens ein Schritt ist kein korrekter Resolutionsschritt. "
            english "At least one step is not a correct resolution step. "
          itemizeM $ map (text . tripShow setNotation) noResolveSteps
          pure ()
        )

    prevent checkEmptyClause $
      translate $ do
        german "Der letzte Schritt leitet die leere Klausel ab?"
        english "The last step derives the empty clause?"

    preventWithHint appliedIsNothing
      (translate $ do
        german "Jeder Schritt nutzt nur in der Formel vorhandene oder zuvor abgeleitete Klauseln?"
        english "Each step utilizes only clauses existing in the formula or previously derived?"
      )
      (paragraph $ do
        translate $ do
          german "Mindestens ein Schritt beinhaltet eine Klausel, die weder in der Formel vorhanden ist, noch zuvor abgeleitet wurde."
          english "At least one step contains a clause that is neither present in the formula nor was previously derived."
      )

    pure ()
    where
      noResolveSteps = filter (\(c1,c2,r) -> maybe True (\x ->
            fromJust (resolve c1 c2 x) /= r) (resolvableWith c1 c2)) steps
      checkEmptyClause = null steps || not (isEmptyClause $ thd3 $ last steps)

partialGrade :: OutputCapable m => ResolutionInst -> Delayed [ResStep] -> LangM m
partialGrade inst = (partialGrade' inst `withDelayed` resStepsParser clauseParser) (const complainAboutWrongNotation)
  where clauseParser | usesSetNotation inst = clauseSetParser
                     | otherwise      = clauseFormulaParser

partialGrade' :: OutputCapable m => ResolutionInst -> [ResStep] -> LangM m
partialGrade' ResolutionInst{..} sol = do
  checkMapping

  preventWithHint (not $ null wrongLitsSteps)
    (translate $ do
      german "Genutzte Literale kommen in Formel vor?"
      english "Used literals are present in formula?"
    )
    (paragraph $ do
      translate $ do
        german "Mindestens ein Schritt beinhaltet Literale, die in der Formel nicht vorkommen. "
        english "At least one step contains literals not found in the formula. "
      itemizeM $ map (text . tripShow usesSetNotation) wrongLitsSteps
      pure ()
    )

  when printFeedbackImmediately $ do
    stepsGraded

  pure ()
  where
    checkMapping = correctMapping (zip [1..] sol) $ baseMapping clauses
    steps =  replaceAll sol $ baseMapping clauses
    availLits = unions (map (fromList . literals) clauses)
    stepLits (c1,c2,r) = toList $ unions $ map (fromList . literals) [c1,c2,r]
    wrongLitsSteps = filter (not . all (`member` availLits) . stepLits) steps
    applied = applySteps clauses steps
    stepsGraded = gradeSteps usesSetNotation steps (isNothing applied)

completeGrade :: (OutputCapable m, Alternative m) => ResolutionInst -> Delayed [ResStep] -> LangM m
completeGrade inst = completeGrade' inst `withDelayedSucceeding` resStepsParser clauseParser
  where clauseParser | usesSetNotation inst = clauseSetParser
                     | otherwise      = clauseFormulaParser

completeGrade' :: (OutputCapable m, Alternative m) => ResolutionInst -> [ResStep] -> LangM m
completeGrade' ResolutionInst{..} sol = (if isCorrect then id else refuse) $ do
    unless printFeedbackImmediately $ do
      recoverFrom stepsGraded

    yesNo isCorrect $ translate $ do
      german "Lösung ist korrekt und vollständig?"
      english "Solution is correct and complete?"

    when (showSolution && not isCorrect) $
      example solutionDisplay $ do
        english "A possible solution for this task is:"
        german "Eine mögliche Lösung für diese Aufgabe ist:"

    pure ()
  where
    steps = replaceAll sol $ baseMapping clauses
    solClauses = replaceAll solution $ baseMapping clauses
    applied = applySteps clauses steps
    stepsGraded = gradeSteps usesSetNotation steps (isNothing applied)
    isCorrect = any isEmptyClause (fromMaybe [] applied)
    solutionDisplay =
      '[' : intercalate ", " (map (tripShow usesSetNotation) solClauses) ++ "]"


baseMapping :: [Clause] -> [(Int,Clause)]
baseMapping xs = zip [1..] $ sort xs



correctMapping :: OutputCapable m => [(Int,ResStep)] -> [(Int,Clause)] -> LangM m
correctMapping [] _ = pure()
correctMapping ((j, Res (c1,c2,(c3,i))): rest) mapping = do
  prevent checkIndices $
    translate $ do
      german $ show j ++ ". Schritt verwendet nur existierende Indizes?"
      english $ "Step " ++ show j ++ " uses only existing indices?"

  prevent (alreadyUsed i) $
    translate $ do
      german $ show j ++ ". Schritt vergibt keinen Index wiederholt?"
      english $ "Step " ++ show j ++ " does not assign an index repeatedly?"

  correctMapping rest newMapping
  pure ()
  where
    newMapping = case i of Nothing      -> mapping
                           (Just index) -> (index,c3) : mapping

    unknown (Left _) = False
    unknown (Right n) = n `notElem` map fst mapping
    checkIndices = unknown c1 || unknown c2
    alreadyUsed Nothing = False
    alreadyUsed (Just n) = n `elem` map fst mapping



replaceAll :: [ResStep] -> [(Int,Clause)] -> [(Clause,Clause,Clause)]
replaceAll [] _ = []
replaceAll (Res (c1,c2,(c3,i)) : rest) mapping = (replaceNum c1, replaceNum c2, c3) : replaceAll rest newMapping
  where
    newMapping = case i of Nothing      -> mapping
                           (Just index) -> (index,c3) : mapping

    replaceNum (Left c) = c
    replaceNum (Right n) = case lookup n mapping of Nothing  -> error "no mapping"
                                                    (Just c) -> c


tripShow :: Bool -> (Clause,Clause,Clause) -> String
tripShow setNotation (c1,c2,c3) =
    '(' : show' c1 ++ ", " ++ show' c2 ++ ", " ++ show' c3 ++ ")"
  where
    show' = showClause setNotation
