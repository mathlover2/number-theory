module NumberTheory.Binomials
       where

-- | A (relatively) efficient implementation of the factorial
-- function. Is tail recursive.

factorial = f 1
  where f acc 0 = acc
        f acc n = f (acc*n) (n-1)

poch m n = p 1 n
  where p 0 _ = 0
        p acc 0 = acc
        p acc k = p (acc*(m-k+1)) (k-1)

binomial m n = (poch m n) `div` (factorial n)
