module NumberTheory.TotientFunction
       where

import NumberTheory.PrimeRelated
import Data.Ratio

eulerTotient n = numerator $ (n%1) * (product $ map (\(x,y) -> 1-(1%x)) (primeFactorPowerList n))
