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
import Test.QuickCheck (Gen, shuffle, chooseInt)

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
  lengthAtoms <- chooseInt (minLength, min maxLength (length xs))
  take lengthAtoms <$> shuffle xs
