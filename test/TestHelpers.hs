{-# LANGUAGE TypeOperators #-}
module TestHelpers (
  deleteBrackets,
  deleteSpaces,
  doesNotRefuse,
  doesNotRefuseIO,
  genSublistOf
  ) where

import Data.Char (isSpace)
import Control.OutputCapable.Blocks.Generic (evalLangM, RunnableOutputCapable (RunMonad), GenericLangM, GenericReportT, runLangMReport)
import Control.Monad.Identity (Identity(runIdentity))
import Data.Maybe (isJust)
import Control.OutputCapable.Blocks (Language, LangM')
import Test.QuickCheck (Gen, chooseInt, shuffle)
-- import Numeric.SpecFunctions as Math (choose)

deleteBrackets :: String  -> String
deleteBrackets = filter (`notElem` "()")

deleteSpaces :: String  -> String
deleteSpaces = filter (not . isSpace)

doesNotRefuse :: (RunMonad l m ~ Identity,  RunnableOutputCapable l m) => GenericLangM l m a -> Bool
doesNotRefuse langM = isJust (runIdentity (evalLangM langM))

doesNotRefuseIO
  :: (m ~ GenericReportT Language (IO ()) IO)
  => LangM' m a
  -> IO Bool
doesNotRefuseIO thing = do
  (r, _) <- runLangMReport (pure ()) (>>) thing
  pure $ isJust r

genSublistOf :: (Int, Int) -> [a] -> Gen [a]
genSublistOf (minLength, maxLength) xs = do
  lengthAtoms <- genLengthBinomial (minLength, maxLength) (length xs)
  take lengthAtoms <$> shuffle xs

genLengthBinomial :: (Int, Int) -> Int -> Gen Int
genLengthBinomial (minLength, maxLength) n =
  chooseInt (lo, hi)
  where
    hi = min maxLength n
    lo = max minLength (min 10 hi)
  -- let lo = max minLength 0
  --     hi = min maxLength n
  --     ks = [lo .. hi]
  -- in if null ks
  --    then error "genLengthBinomial: no valid lengths"
  --    else frequency
  --           [ (floor (Math.choose n k), pure k)
  --           | k <- ks
  --           ]
