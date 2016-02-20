{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module NumberTheory.Permutations.Internals
       ( CyclePerm(..)
       , FunctionalForm(..)
       , InversionVector(..)
       , Permutation(..)
       , transposition
       , cycleperm
       , cycleProduct
       ) where

import Control.Exception(assert)
import Data.Functor
import Data.Function (on)
import Data.Function.Memoize
import Data.Group
import Data.List
import Data.Monoid
import Data.Ord (comparing)
import Data.Tuple (swap)

newtype FunctionalForm = FunctionalForm
                         { functionalForm :: [(Int,Int)]
                         } deriving (Read, Show)

functionalFormReduce = FunctionalForm . sortByFst
                       . nub . (filter isMoved) . functionalForm 

addUnmovedPairs :: [(Int, Int)] -> [(Int, Int)]
addUnmovedPairs l
  = l ++ [ (n,n) | n <- [1..(maxMovedL l)] \\ mergePairs l]

-- Do not change to @map fst@ -- it interferes with the workings of
-- toOldFunctionalForm, and thus of toInversionVector.
mergePairs :: [(Int, Int)] -> [Int]
mergePairs = map snd

maxMovedL :: [(Int, Int)] -> Int
maxMovedL = maximum . mergePairs

newtype InversionVector = InversionVector
                          { inversionVector :: [Int]
                          } deriving (Read, Show)

newtype CyclePerm = CyclePerm
                    { cyclePerm :: [[Int]]
                    } deriving (Read, Show)

type FactorialNum = Integer

fromFactorialRep :: InversionVector -> FactorialNum
fromFactorialRep
  = let fac n = product [1..n]
    in  sum . (zipWith (*) (map fac [0..]))
        . reverse . (map toInteger) . inversionVector

type OldFunctionalForm = [Int]

class Permutation a where
  toFunctionalForm :: a -> FunctionalForm
  toInversionVector :: a -> InversionVector
  toFactorialNum :: a -> FactorialNum
  toFactorialNum = fromFactorialRep . toInversionVector
  toCyclePerm :: a -> CyclePerm
  permCompose :: a -> a -> a
  permInverse :: a -> a
  permPower :: (Integral x) => a -> x -> a
  isProper :: a -> Bool
  maxMoved :: a -> Int
  permId :: a
  permPower x0 n0 = case compare n0 0 of
    LT -> permInverse . f x0 $ negate n0
    EQ -> permId
    GT -> f x0 n0
    where
      f x 1 = x
      f x n = if even n
              then f (x `permCompose` x) (n `quot` 2)
              else x `permCompose` (f x (n-1))

instance Permutation FunctionalForm where
  toFunctionalForm = id
  toInversionVector = functionalFormToInversionVector
  toCyclePerm = functionalFormToCyclePerm
  permCompose = functionalFormCompose
  permInverse = functionalFormInverse
  isProper = is_proper_functional_form
  maxMoved = maximum . mergePairs . functionalForm
  permId = FunctionalForm []

instance Permutation CyclePerm where
  toFunctionalForm = permToFunctionalForm
  toCyclePerm = id
  toInversionVector = permToInversionVector
  permCompose = cyclePermCompose
  permInverse = cyclePermInverse
  isProper = is_proper_cycle_perm
  maxMoved = maximum . concat . cyclePerm
  permId = CyclePerm []

instance Permutation InversionVector where
  toFunctionalForm = inversionVectorToFunctionalForm
  toCyclePerm = inversionVectorToCyclePerm
  toInversionVector = id
  permCompose = inversionVectorCompose
  permInverse = inversionVectorInverse
  isProper = is_proper_inversion_vector
  maxMoved = length . inversionVector
  permId = InversionVector [0]

-- An instance of Eq for permutation representations. This requires
-- the language pragmas at the top of the file.

instance (Permutation a) => Eq a where
   (==) = eq'

eq' :: (Permutation a) => a -> a -> Bool
eq' a b = let (FunctionalForm a') = functionalFormReduce $ toFunctionalForm a
              (FunctionalForm b') = functionalFormReduce $ toFunctionalForm b
          in a' == b'


-- An instance of Monoid for permutation representations. This
-- requires the language pragmas at the top of the file.

instance (Permutation a) => Monoid a where
  mempty = permId
  mappend = permCompose
  -- An instance of mconcat may be explicitly given in the future,
  -- as I have a few ideas as to how to optimize this.

-- An instance of Group for permutation representations. This requires
-- the language pragmas at the top of the file.

instance (Permutation a) => Group a where
  invert = permInverse
  pow = permPower

-- FunctionalForm instance implementations.

-- At this point in time, all instances have toInversionVector
-- defined in terms of this operation.

functionalFormToInversionVector
  = oldFunctionalFormToInversionVector
    . toOldFunctionalForm

toOldFunctionalForm :: FunctionalForm -> OldFunctionalForm
toOldFunctionalForm = mergePairs . sortByFst
                      . addUnmovedPairs . functionalForm

oldFunctionalFormToInversionVector :: OldFunctionalForm -> InversionVector
oldFunctionalFormToInversionVector
  = let acc l = zipWith count l (tails l)
        count x = length . filter (<x)
    in  InversionVector . acc

functionalFormToCyclePerm :: FunctionalForm -> CyclePerm
functionalFormToCyclePerm
  = let removeFixed = filter isMoved -- Removes unmoved elements
        extractLoops [] = []
        extractLoops l = map fst (getLoop l)
                         :(extractLoops (l \\ getLoop l))
        -- Retrieves a single cycle from l.
        getLoop l = (head l) :
                    (takeWhile (/= head l)
                    . tail $ iterate (nextPair l) (head l))
        -- Looks up the next pair in the loop being constructed
        nextPair l = (flip pairlookup l) . snd
    in  CyclePerm . extractLoops . removeFixed . functionalForm
-- Old version:
-- functionalFormToCyclePerm ff = CyclePerm
--                                $ getCycle (functionalForm ff)
--   where getCycle [] = []
--         getCycle l@((x0,y0):ls) =
--           if x0 == y0
--           then getCycle ls
--           else loopBuild [(x0,y0)] l
--           where loopBuild loop []   = [flatten loop]
--                 loopBuild loop list
--                   = if nextPair == head loop
--                     then (flatten loop):(getCycle (list \\ loop))
--                     else loopBuild (loop++[nextPair]) list
--                   where nextPair = pairlookup ((snd . last) loop) list
--         flatten = map fst

functionalFormCompose :: FunctionalForm -> FunctionalForm -> FunctionalForm
functionalFormCompose (FunctionalForm l) (FunctionalForm m)
   =  FunctionalForm $ compose l m
   where compose x y -- creates the list which represents the
                     -- functional form of the composition $x \circ y$
                     -- of the two permutations.
           = map (\(xi_1,xi_2) ->
                   (xi_1, snd (pairlookup xi_2 ex))) ey
           where [ex, ey] = map extend [x,y]
                 extend x0 = x0 ++ [(n,n) | n <- [(maxMovedL x0)+1..maxlm]]
                 maxlm = (max `on` maxMovedL) x y

functionalFormInverse :: FunctionalForm -> FunctionalForm
functionalFormInverse = FunctionalForm . sortByFst
                        . (map swap) . functionalForm

is_proper_functional_form l
  = let x = functionalForm l
    in sort (map fst x) == sort (map snd x)

-- CyclePerm

permToInversionVector = toInversionVector . toFunctionalForm

permToFunctionalForm (CyclePerm []) = FunctionalForm [(1,1)]
permToFunctionalForm l = (FunctionalForm . addUnmovedPairs
                       . concat . (map toTuples) . cyclePerm) l
  where toTuples [] = []
        toTuples l = (last l, head l):[(l !! n, l !! (n + 1))
                                      | n <- [0..(length l)-2]]

cyclePermCompose l m
  = toCyclePerm
    $ (permCompose `on` toFunctionalForm) l m

cyclePermInverse =
  toCyclePerm . permInverse
  . toFunctionalForm

is_proper_cycle_perm perm =
  let cyc = cyclePerm perm
      pairWiseDisjoint [] = True
      pairWiseDisjoint (p0:ps) = and (map (null . intersect p0) ps)
                                 && pairWiseDisjoint ps
      test = (==) =<< nub
  in  pairWiseDisjoint cyc && and (map test cyc)

-- InversionVector

inversionVectorInverse = toInversionVector
                         .functionalFormInverse
                         .toFunctionalForm

inversionVectorCompose l = toInversionVector
                           . ((cyclePermCompose `on` toCyclePerm) l)

inversionVectorToFunctionalForm l
  = let vect = inversionVector l
        go l iv@(i0:is) = if zero iv then l
                          else next:(go nextl is)
          where next = (l !! i0)
                nextl = delete next l
                zero = and . map (==0)
        ran = [1..(length vect)]
    in FunctionalForm $ zip ran (go ran vect)

inversionVectorToCyclePerm = toCyclePerm . toFunctionalForm

is_proper_inversion_vector 
  = let ipiv l = all (uncurry (<=)) $ zip l (reverse [0..ln])
          where ln = (length l) - 1
    in  ipiv . inversionVector

-- Some basic permutations.

transposition a b = assert (a /= b) $ CyclePerm [[a,b]]

cycleperm :: [Int] -> CyclePerm
cycleperm l = assert (isProper (CyclePerm [l])) (CyclePerm [l])

cycleProduct :: [[Int]] -> CyclePerm
cycleProduct l = assert (isProper (CyclePerm l)) (CyclePerm l)


{- Helper functions. -}

-- Sorts pairs based on the first element.

sortByFst = sortBy (comparing fst)

-- Yields True if a pair consists of two identical entries. isMoved is
-- the negation of this.

isUnmoved, isMoved :: (Eq a) => (a,a) -> Bool
isUnmoved = uncurry (==)
isMoved   = not . isUnmoved
-- A helper function for defining instances involving FunctionalForm.

pairlookup n list = case (find (\(x,y) -> x == n) list)
                    of   (Just plu) -> plu
                         Nothing    -> (n,n)


-- Helper functions for defining inversion vectors.

indexedList :: [a] -> [(a,Int)]
indexedList l
  = let ran = [0..(length l)-1]
    in  zip l ran

mapIndexed :: (a -> Int -> b) -> [a] -> [b]
mapIndexed f l = zipWith f l [0..]


-- SW...

toFactorialRep :: Integer -> [Int]
toFactorialRep num
  = let fac = memoFix ff
          where ff f 0 = 1
                ff f n = n * f (n-1)
        nearestFacIndex n = until (\k -> fac (k+1) > n) succ 1
        builder 0 n = [0]
        builder k n = (n `div` (fac k)):(builder (k-1) (n `rem` (fac k)))
    in  map fromInteger $ builder (nearestFacIndex num) num
     

 
