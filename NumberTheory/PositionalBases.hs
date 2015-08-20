{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances #-}

module NumberTheory.PositionalBases
       where

import Data.Char
import Data.Function (on)
import Data.List (foldl1')
import Data.List.Split (chunksOf)

data BaseRepresentation a = BR { getBase :: a
                               , getDigits :: [a]
                               } deriving Eq

instance (Integral a, Show a) => Show (BaseRepresentation a) where
  show (BR b n) = show b ++ "_" ++ show n

instance (Integral a, Read a) => Read (BaseRepresentation a) where
  readsPrec _ r = let (b,x) = span isDigit r
                      y = tail x
                  in [(BR
                       { getBase = read b
                       , getDigits = read x
                       }, "")]
                      

findDigits :: (Integral a) => a -> a -> [a]
findDigits b n = f [n]
  where
    f l@(l0:ls) = if l0 < b then l else f (q:r:ls)
      where (q,r) = l0 `quotRem` b

{-

A partially pointfree version:

findDigits b = until (\(l0:_) -> l0 < b) (\(l0:ls) -> ((q l0):(r l0):ls))
               . (:[])
  where [q,r] = map (($ b) . flip) [quot, rem]

-}


toBaseRepresentation :: (Integral a) => a -> a -> BaseRepresentation a
toBaseRepresentation b n = BR b (findDigits b n)

fromBaseRepresentation :: (Integral a) => BaseRepresentation a -> a
fromBaseRepresentation (BR b l) = fromDigits b l

fromDigits :: Num a => a -> [a] -> a
fromDigits b l = foldl1' (\x y -> b * x + y) l

-- Helper functions.

chunksOfRev :: Int -> [a] -> [[a]]
chunksOfRev k l = reverse $ map reverse $ chunksOf k $ reverse l

isPowerOf :: Integral a => a -> a -> Bool
isPowerOf n b
  = b ^ (round $ (logBase `on` fromIntegral) b n) == n
