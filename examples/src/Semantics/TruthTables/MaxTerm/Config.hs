module Semantics.TruthTables.MaxTerm.Config where

import LogicTasks.Config (
  BaseConfig(..),
  MinMaxConfig (..),
  NormalFormConfig (..),
  )
import Test.Hspec
import LogicTasks.Util (checkNormalFormConfig)
import Util.VerifyConfig
import Control.OutputCapable.Blocks (Language(German))

-- Weight 0.33
-- Type: Max
unused01 :: MinMaxConfig
unused01 = MinMaxConfig
  { normalFormConf = NormalFormConfig
    { baseConf = BaseConfig
      { minClauseLength = 2
      , maxClauseLength = 2
      , usedAtoms = "ABC"
      }
    , minClauseAmount = 3
    , maxClauseAmount = 3
    }
  , percentTrueEntries = (50, 70)
  , extraText = Nothing
  , printSolution = True
  , offerUnicodeInput = False
  }

-- Weight 0.34
-- Type: Max
unused02 :: MinMaxConfig
unused02 = MinMaxConfig
  { normalFormConf = NormalFormConfig
    { baseConf = BaseConfig
      { minClauseLength = 2
      , maxClauseLength = 3
      , usedAtoms = "ABCD"
      }
    , minClauseAmount = 3
    , maxClauseAmount = 4
    }
  , percentTrueEntries = (50, 70)
  , extraText = Nothing
  , printSolution = True
  , offerUnicodeInput = False
  }

spec :: Spec
spec = do
  describe "unused01" $ verifyConfig German (normalFormConf unused01) checkNormalFormConfig
  describe "unused02" $ verifyConfig German (normalFormConf unused02) checkNormalFormConfig
