
module Table
       (
         Table
       , getTable
       , genTable
       , genGapTable
       , genWrongTable
       , fillGaps
       , readEntries
       , countDiffEntries
       , possibleAllocations
       ) where

import Data.List (transpose)
import Data.Maybe (isNothing)
import Data.Set (toList,unions)
import Test.QuickCheck
import Formula (Allocation,Literal(..),Clause(..),CNF(..),evalCNF)
import qualified Data.Set as Set (map)



data Table = Table
    { getLiterals :: [Literal]
    , getEntries :: [Maybe Bool]}
    deriving Eq



instance Show Table where
 show t = header ++ "\n" ++ rows
  where literals = getLiterals t
        formatLine [] _ = []
        formatLine x y = foldr ((\x y -> x ++ " | " ++ y) . show) (maybe "---" show y) x ++ "\n"
        header = concat [show x ++ " | " | x <- literals] ++ "VALUES"
        rows = concat [formatLine x y | (x,y) <- zip (transpose $ comb (length literals) 1) $ getEntries t]
        comb 0 _ = []
        comb len n = concat (replicate n $ replicate num 0 ++ replicate num 1) : comb (len-1) (n*2)
         where num = 2^(len -1)



instance Arbitrary Table where
  arbitrary = sized table
    where table n = do
            cnf <- resize n arbitrary
            return (getTable cnf)


getTable :: CNF -> Table
getTable cnf = Table literals values
 where literals = toList $ unions $ map (Set.map filterSign . getLs) $ toList (getCs cnf)
       filterSign x = case x of Not y -> Literal y
                                _     -> x
       values = map (`evalCNF` cnf) $ possibleAllocations literals



--getCNF :: Table -> CNF
--getCNF table =


allCombinations :: [Literal] -> Int ->  [Allocation]
allCombinations [] _ = []
allCombinations (x:xs) n = concat (replicate n $ replicate num (x,False) ++ replicate num (x,True)) : allCombinations xs (n*2)
         where num = 2^ length xs


possibleAllocations :: [Literal] -> [Allocation]
possibleAllocations xs = transpose (allCombinations xs 1)


genTable :: [Char] -> (Int,Int) -> Gen Table
genTable lits (lower,upper)
 | lower < 0 || upper < lower || upper > 100 = error "invalid percentage range."
 | otherwise = do
   percentage <- chooseInt (lower,upper)
   let rows = (length lits)^2
   let amountTrue = rows * percentage `div` 100
   let falses = replicate (rows - amountTrue) False
   let trues = replicate amountTrue True
   entries <- shuffle (trues ++ falses)
   return (Table (map Literal lits) (map Just entries))




genGapTable :: Table -> Int -> Gen Table
genGapTable table gaps
 | gaps < 0 = error "The amount of gaps is negative."
 | rowAmount < gaps = genGapTable table rowAmount
 | otherwise = generateGaps [] gaps

 where rowAmount = length (getEntries table)
       generateGaps indices 0 = do
        let gapTable = Table (getLiterals table) [ if x `elem` indices then Nothing else getEntries table !! x | x <- [0..length (getEntries table)-1]]
        return gapTable
       generateGaps indices num = do
        rInt <- suchThat (chooseInt (0, length (getEntries table)-1)) (`notElem` indices)
        generateGaps (rInt: indices) (num-1)




genWrongTable :: Table -> Int -> Gen ([Int],Table)
genWrongTable table changes
 | changes < 0 = error "The amount of changes is negative."
 | rowAmount < changes = genWrongTable table rowAmount
 | otherwise = generateChanges [] changes

 where rowAmount = length (getEntries table)
       generateChanges indices 0 = do
        let newTable = Table (getLiterals table) [ if x `elem` indices then not <$> (getEntries table !! x) else getEntries table !! x | x <- [0..length (getEntries table)-1]]
        return (indices,newTable)
       generateChanges indices num = do
        rInt <- suchThat (chooseInt (0, length (getEntries table)-1)) (`notElem` indices)
        generateChanges (rInt: indices) (num-1)



fillGaps :: [Bool] -> Table -> Table
fillGaps solution table
 | length solution > length (filter isNothing tabEntries) = table
 | otherwise = Table (getLiterals table) (filledIn solution tabEntries)
  where tabEntries = getEntries table
        filledIn [] ys = ys
        filledIn _ [] = []
        filledIn (x:xs) (y:ys) = if isNothing y then Just x : filledIn xs ys else y : filledIn (x:xs) ys


readEntries :: Table -> [Maybe Bool]
readEntries = getEntries



countDiffEntries :: Table -> Table -> Int
countDiffEntries t1 t2 = diffs (getEntries t1) (getEntries t2)
  where diffs [] ys = length ys
        diffs xs [] = length xs
        diffs (x:xs) (y:ys) = (if x == y then 0 else 1) + diffs xs ys
