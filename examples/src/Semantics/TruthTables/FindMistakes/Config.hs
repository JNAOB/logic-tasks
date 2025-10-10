module Semantics.TruthTables.FindMistakes.Config where

import LogicTasks.Config (
  BaseConfig(..),
  DecideConfig(..),
  NormalFormConfig(..),
  FormulaConfig(..)
  )
import Test.Hspec
import Tasks.SynTree.Config (
  SynTreeConfig(..)
  )
import Trees.Types (BinOp(..))
import Util.VerifyConfig
import qualified Data.Map as Map (fromList)
import Data.Map (Map)

listToFM :: Ord k => [(k, a)] -> Map k a
listToFM = Map.fromList

-- 2024: Weight 0.3
task10 :: DecideConfig
task10 = DecideConfig
  { formulaConfig =
      FormulaCnf (NormalFormConfig
                   { baseConf = BaseConfig
                     { minClauseLength = 2
                     , maxClauseLength = 2
                     , usedAtoms = "ABCD"
                     }
                   , minClauseAmount = 3
                   , maxClauseAmount = 3
                   })
  , percentageOfChanged = 40
  , percentTrueEntries = (30,70)
  , printSolution = True
  , extraText = Nothing
  }

-- 2024: Weight 0.3
task12 :: DecideConfig
task12 = DecideConfig
  { formulaConfig =
      FormulaArbitrary (SynTreeConfig
                        { minNodes = 10
                        , maxNodes = 14
                        , minDepth = 4
                        , maxDepth = 6
                        , availableAtoms = "ABCD"
                        , minAmountOfUniqueAtoms = 4
                        , binOpFrequencies = listToFM
                          [ (And, 1)
                          , (Or, 1)
                          , (Impl, 1)
                          , (BackImpl, 0)
                          , (Equi, 1)
                          ]
                        , negOpFrequency = 1
                        , maxConsecutiveNegations = 2
                        , minUniqueBinOperators = 4
                        })
  , percentageOfChanged = 30
  , percentTrueEntries = (30,70)
  , printSolution = True
  , extraText = Nothing
  }

spec :: Spec
spec = do
  describe "task10" $ verifyFormulaConfig (formulaConfig task10)
  describe "task12" $ verifyFormulaConfig (formulaConfig task12)
