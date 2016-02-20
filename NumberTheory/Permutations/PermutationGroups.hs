module NumberTheory.Permutations.PermutationGroups
       (order, makeGroup, groupElement)
       where

import Data.List (nub)
import NumberTheory.Permutations

order :: (Permutation a, Eq a) => a -> Int
order a = let (CyclePerm a') = toCyclePerm a
          in  foldl1 lcm $ map length a'

data PermGroup a = PermGroup { getGenerators :: [a] }

makeGroup :: (Permutation a, Eq a) => [a] -> PermGroup a
makeGroup = PermGroup . nub -- This should be improved upon...

groupElement :: (Permutation a, Eq a) => [(Int,Int)] -> PermGroup a -> a
groupElement p (PermGroup l) = foldl1 permCompose $ map f p
  where f (el,po) = (l !! (el-1)) `permPower` po
