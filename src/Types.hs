{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}
module Types where

import Data.Maybe (isJust,fromJust)


data ClauseConfig = ClauseConfig
    { minClauseLength :: Int
    , maxClauseLength :: Int
    , usedLiterals :: [Char]
    } deriving (Show,Read)



data CnfConfig = CnfConfig
    { clauseConf :: ClauseConfig
    , minClauseAmount :: Int
    , maxClauseAmount :: Int
    } deriving (Show,Read)



data FillConfig = FillConfig
    { cnfConfig :: CnfConfig
    , percentageOfGaps :: Int
    , percentTrueEntries :: Maybe (Int,Int)
    } deriving (Show,Read)



data GiveCnfConfig = GiveCnfConfig
    { cnfConfig :: CnfConfig
    , percentTrueEntries :: Maybe (Int,Int)
    } deriving (Show,Read)



data PickConfig = PickConfig
    { cnfConfig :: CnfConfig
    , amountOfOptions :: Int
    , pickCnf :: Bool
    } deriving (Show,Read)



data DecideConfig = DecideConfig
    { cnfConfig :: CnfConfig
    , amountOfChanges :: Int
    , findMistakes :: Bool
    } deriving (Show,Read)



newtype StepConfig = StepConfig
    { clauseConfig :: ClauseConfig
    } deriving (Show,Read)



data ResolutionConfig = ResolutionConfig
    { clauseConfig :: ClauseConfig
    , steps :: Int
    } deriving (Show,Read)



defaultClauseConfig :: ClauseConfig
defaultClauseConfig = ClauseConfig
    { minClauseLength = 1
    , maxClauseLength = 3
    , usedLiterals = ['A'..'C']
    }



defaultCnfConfig :: CnfConfig
defaultCnfConfig = CnfConfig
    { clauseConf = defaultClauseConfig
    , minClauseAmount = 2
    , maxClauseAmount = 3
    }



defaultFillConfig :: FillConfig
defaultFillConfig = FillConfig
    { cnfConfig = defaultCnfConfig
    , percentageOfGaps = 40
    , percentTrueEntries = Just (20,80)
    }



defaultGiveCnfConfig :: GiveCnfConfig
defaultGiveCnfConfig = GiveCnfConfig
    { cnfConfig = defaultCnfConfig
    , percentTrueEntries = Just (30,70)
    }



defaultPickConfig :: PickConfig
defaultPickConfig = PickConfig
    { cnfConfig = defaultCnfConfig
    , amountOfOptions = 5
    , pickCnf = False
    }



defaultDecideConfig :: DecideConfig
defaultDecideConfig = DecideConfig
    { cnfConfig = defaultCnfConfig
    , amountOfChanges = 2
    , findMistakes = True
    }



defaultResolutionConfig :: ResolutionConfig
defaultResolutionConfig = ResolutionConfig
    { clauseConfig = defaultClauseConfig
    , steps = 5
    }



defaultStepConfig :: StepConfig
defaultStepConfig = StepConfig { clauseConfig = defaultClauseConfig}



checkClauseConfig :: ClauseConfig -> Maybe String
checkClauseConfig ClauseConfig {..}
    | any (<0) [minClauseLength, maxClauseLength] = Just "At least one of your clause length parameters is negative."
    | minClauseLength > maxClauseLength = Just "The minimum clause length is greater than the maximum."
    | length usedLiterals < minClauseLength = Just "There's not enough literals to satisfy your minimum clause length."
    | null usedLiterals = Just "You did not specify which literals should be used."
    | otherwise = Nothing


checkCnfConfig :: CnfConfig -> Maybe String
checkCnfConfig CnfConfig {..}
    | any (<0) [minClauseAmount, maxClauseAmount] = Just "At least one of your clause amount parameters is negative."
    | minClauseAmount > maxClauseAmount = Just "The minimum amount of clauses is greater than the maximum amount."
    | minClauseAmount > maxClauses = Just "There are not enough combinations available to satisfy your amount and length settings."
    | otherwise = checkClauseConfig clauseConf
  where
    maxClauses = minimum [2^maxClauseLength clauseConf, 2^length (usedLiterals clauseConf)]


checkFillConfig :: FillConfig -> Maybe String
checkFillConfig FillConfig {..}
    | percentageOfGaps < 0 || percentageOfGaps > 100 = Just "The percentage of gaps must be between 0 and 100%."
    | otherwise = checkCnfConfig cnfConfig



checkGiveCnfConfig :: GiveCnfConfig -> Maybe String
checkGiveCnfConfig GiveCnfConfig {..}
    | isJust percentTrueEntries = if lower > upper
        then Just "The minimum percentage of true rows is greater than the maximum."
        else if any (<0) [lower,upper]
               then Just "At least one of your percentages is negative."
               else checkCnfConfig cnfConfig
    | otherwise = checkCnfConfig cnfConfig
  where
    (lower,upper) = fromJust percentTrueEntries



checkPickConfig :: PickConfig -> Maybe String
checkPickConfig PickConfig {..}
    | amountOfOptions < 0 = Just "The amount of options is negative."
    | otherwise = checkCnfConfig cnfConfig



checkDecideConfig :: DecideConfig -> Maybe String
checkDecideConfig DecideConfig {..}
    | amountOfChanges < 0 = Just "The amount of changes is negative."
    | amountOfChanges >  2^length (usedLiterals clConfig) = Just "The table does not have enough entries to support this samount of changes."
    | amountOfChanges > 2^(maxClauseAmount cnfConfig * maxClauseLength clConfig) = Just "This amount of changes is not possible with your Clause length and amount settings."
    | otherwise = checkCnfConfig cnfConfig
  where
    clConfig = clauseConf cnfConfig


checkStepConfig :: StepConfig -> Maybe String
checkStepConfig StepConfig {..} = checkClauseConfig clauseConfig



checkResolutionConfig :: ResolutionConfig -> Maybe String
checkResolutionConfig ResolutionConfig {..}
    | steps < 0 = Just "The amount of steps is negative."
    | maxClauseLength clauseConfig  == 1 && steps > 1 = Just "More than one step using only length 1 clauses is not possible."
    | steps > 2 * length (usedLiterals clauseConfig) = Just "This amount of steps is impossible with the given amount of literals."
    | otherwise = checkClauseConfig clauseConfig
