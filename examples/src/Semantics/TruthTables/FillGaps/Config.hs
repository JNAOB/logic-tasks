module Semantics.TruthTables.FillGaps.Config where

import LogicTasks.Config (
  BaseConfig(..),
  FillConfig (..),
  NormalFormConfig(..),
  FormulaConfig(..),
  )
import Test.Hspec
import Util.VerifyConfig

-- 2024: Weight 0.3
task07 :: FillConfig
task07 = FillConfig
  { formulaConfig =
      FormulaCnf (NormalFormConfig
                  { baseConf = BaseConfig
                    { minClauseLength = 2
                    , maxClauseLength = 2
                    , usedAtoms = "ABC"
                    }
                  , minClauseAmount = 3
                  , maxClauseAmount = 3
                  })
  , percentageOfGaps = 60
  , percentTrueEntries = (30, 70)
  , printSolution = True
  , extraText = Nothing
  }

-- 2023: Weight 0.25
task20 :: FillConfig
task20 = FillConfig
  { formulaConfig =
      FormulaCnf (NormalFormConfig
                   { baseConf = BaseConfig
                     { minClauseLength = 3
                     , maxClauseLength = 3
                     , usedAtoms = "ABCDE"
                     }
                   , minClauseAmount = 4
                   , maxClauseAmount = 4
                   })
  , percentageOfGaps = 40
  , percentTrueEntries = (35, 65)
  , extraText = Nothing
  , printSolution = True
  }

spec :: Spec
spec = do
  describe "task07" $ verifyFormulaConfig (formulaConfig task07)
  describe "task20" $ verifyFormulaConfig (formulaConfig task20)
