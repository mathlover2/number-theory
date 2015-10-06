module NumberTheory.Binomials (binomial)
       where

import Factory.Math.Implementations.Factorial
import Factory.Data.PrimeFactors
import Data.Ratio ((%))

binomial m n = let pfd = primeFactors (m-n) >*< primeFactors n
               in  product' (1 % 2) 10 $ fst $ (primeFactors m) >/< pfd
