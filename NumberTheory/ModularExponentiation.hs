module NumberTheory.ModularExponentiation
	where
	
import NumberTheory.PositionalBases

modular_pow a k m = m_P 1 0
	where m_P c e
		| e == k    = c
		| otherwise = m_P (mod (c*a) m) (e+1)
			
modular_pow_bin_method a k m = (product $ zipWith (expv2) (binary_Powers_a) (binary_Digits_k)) `mod` m
	where
		binary_Digits_k = reverse $ findDigits 2 k
		binary_Powers_a = take (length binary_Digits_k) $ iterate (\x -> mp2 x) a
		expv2 a b
			| a == 0 && b == 0 = 1
			| a == 0 && b == 1 = 0
			| otherwise        = a^b
		mp2 x = (mod (x*x) m)
