module NumberTheory.PrimeRelated
	where

import Data.Tree
import Data.Ratio
import Data.List(nub, elemIndices)
import Math.NumberTheory.Prime(isPrime,
                               isTrialDivisionPrime,
                               isMillerRabinPrime)
import Math.NumberTheory.Factor(pfactors)

	
primeQ :: Integer -> Bool
primeQ = isPrime

--data PrimeTestMethod = Fermat | Miller_Rabin --

data FactorMethod = FermatMethod1
                  | FermatMethod2 Rational
                  | Default

factorList :: FactorMethod -> Integer -> [Integer]

factorList method n = case method of
  FermatMethod1 -> ferm n
  (FermatMethod2 rat) -> ferm2 n rat
  Default -> pfactors n
  _ -> error "Method given not yet implemented!"
  where
    ferm n = feM1 n k
      where k = (ceiling (sqrt (fromInteger  n :: Float )) :: Integer)
            feM1 n k
              | n `mod` 2 == 0     = [(highest_power_2 n), (power_2_Free n)]
              | n == 1             = [1,1]
              | perfsqQ ((k^2)-n)  = [(k+l),(k-l)]
              | otherwise          = feM1 n (k+1)
              where perfsqQ nn = (nn == ((neq nn)^2 :: Integer))
                    l = neq $ (k^2)-n
                    neq n = (round.sqrt $ (fromInteger n :: Float)) :: Integer
                    highest_power_2 n = fst $ until test acc (1, n)
                      where acc (x,y) = (2*x,y`div`2)
                            test x = (snd x) `mod` 2 /= 0
                    power_2_Free n = n `div` (highest_power_2 n)
    ferm2 n rat = [gcd n cv, gcd n du]
      where (cv,du) = tupl $ ferm (n * (numerator rat) * (denominator rat))
            tupl = (\[x,y] -> (x,y))

primeFactorPowerList :: Integer -> [(Integer, Integer)]
primeFactorPowerList n = binCount (factorList Default n)
  where binCount l = map func (nub l)
          where func x = (x, (toInteger.length) (elemIndices x l))

legendreSymbol :: Integer -> Integer -> Integer
legendreSymbol a b
  | not (primeQ b)               = error "b is not prime! "
  | b < a                        = legendreSymbol (a`mod`b) b
  | a == 1                       = 1
  | a == 2                       = (-1)^((b^2-1)`div`8)
  | a == b - 1                   = (-1)^((b-1)`div`2)
  | (primeQ a)&&(primeQ b)       = (-1)^(((a-1)*(b-1))`div`4)*(lsB a)
  | (not (primeQ a))&&(primeQ b) = product $ map lsB (factorTable a)
  where lsB = (`legendreSymbol` b)
        factorTable = factorList Default

jacobiSymbol :: Integer -> Integer -> Integer
jacobiSymbol a b
  | b `mod` 2 == 0   = error "b must be odd"
  | b == 1 || a == 1 = 1
  | gcd a b /= 1     = 0
  | a == b - 1       = (-1)^((b-1)`div`2)
  | a == 2           = (-1)^(((b^2)-1)`div`8)
  | a > b || a < 0   = jacobiSymbol (a`mod`b) (b)
  | a `mod` 2 == 1   = ((-1)^(((a-1)*(b-1))`div`4))*(jacobiSymbol b a)
  | a `mod` 2 == 0   = (jacobiSymbol 2 b)*(jacobiSymbol (a`div`2) b)

kroneckerSymbol :: Integer -> Integer -> Integer
kroneckerSymbol a n
  | n == (-1)         = neg_one_kronecker a
  | signum n == (-1)  = (neg_one_kronecker a)*(kroneckerSymbol a (-n))
  | n == 2            = two_kronecker a
  | n == 0            = zero_kronecker a
  | gcd a n /= 1      = 0
  | odd n             = jacobiSymbol a n
  | otherwise         = product $ map (\(x,y) -> (kroneckerSymbol a x)^y)
                        (factorTable n)
  where
    factorTable n = pairCount (factorList Default n)
        where pairCount l = map (\x -> (x, length $ elemIndices x l)) $ nub l
    two_kronecker a
      | even a = 0
      | mod a 8 == 1 || mod a 8 == 7 = 1
      | mod a 8 == 3 || mod a 8 == 5 = (-1)
    neg_one_kronecker a
      | a < 0  = (-1)
      | a >= 0 = 1
    zero_kronecker a
      | a == 1 || a == (-1) = 0
      | otherwise = 0
