module Semantics.TruthTables.MinTerm.Config where

import LogicTasks.Config (
  BaseConfig(..),
  MinMaxConfig (..),
  NormalFormConfig (..),
  )
import Test.Hspec
import LogicTasks.Util (checkNormalFormConfig)
import Util.VerifyConfig
import Control.OutputCapable.Blocks (Language(German))

-- Weight 0.2
-- Type: Min
unused03 :: MinMaxConfig
unused03 = MinMaxConfig
  { normalFormConf = NormalFormConfig
    { baseConf = BaseConfig
      { minClauseLength = 2
      , maxClauseLength = 3
      , usedAtoms = "ABCD"
      }
    , minClauseAmount = 2
    , maxClauseAmount = 3
    }
  , percentTrueEntries = (55, 70)
  , extraText = Nothing
  , printSolution = True
  , offerUnicodeInput = False
  }

spec :: Spec
spec = do
  describe "unused03" $ verifyConfig German (normalFormConf unused03) checkNormalFormConfig
