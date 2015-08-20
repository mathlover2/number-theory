module NumberTheory.FractionalDigits
       where

import NumberTheory.PositionalBases

toDigits_bounded :: (RealFrac a, Integral b) => Int -> b -> a -> [b]
toDigits_bounded k b x
  | x < 1 = acc k x
  | otherwise = findDigits b n' ++ toDigits_bounded k b f'
  where acc 0 y = []
        acc m y = n : acc (m-1) f
          where (n,f) = properFraction ((fromIntegral b) * y)
        (n', f') = properFraction x
