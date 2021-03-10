
module Table
       (
         readEntries
       , readLiterals
       , flipAt
       , gapsAt
       ) where



import Data.Set (Set)

import Types
import qualified Data.Set as Set (fromList)




readEntries :: Table -> [Maybe Bool]
readEntries = getEntries

readLiterals :: Table -> Set Literal
readLiterals = Set.fromList . getLiterals



gapsAt :: Table -> [Int] -> Table
gapsAt table gaps = Table (getLiterals table) newEntries
  where
    entries = getEntries table
    newEntries = [ if x+1 `elem` gaps then Nothing else entries !! x
                 | x <- [0..length entries-1]
                 ]





flipAt :: Table -> [Int] -> Table
flipAt table indices = Table (getLiterals table) newEntries
  where
    entries = getEntries table
    newEntries = [ let value = entries !! x in
                     if x+1 `elem` indices then fmap not value else value
                 | x <- [0..length entries-1]
                 ]

