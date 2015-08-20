
module NumberTheory.PrimeRelated.MillerRabin
	( miller_Rabin_PseudoPrime
	, miller_Rabin_PseudoPrime_Bounded
	) where

import NumberTheory.PositionalBases
import NumberTheory.ModularExponentiation
import System.Random

{- The Miller-Rabin primality test! I don't think there is another Haskell implementation of this, to the best of my knowledge.
The first function takes a positive odd number n other than 3, and returns a Boolean value as to whether it is a probable prime. 
The second does the same for a list of test values. It is not exported here, as it would only be of interest to a theorist, but is still mentioned here.
The third is a deterministic primality test which works when n is less than 3825123056546413051. -}

miller_Rabin_PseudoPrime n k g
--conditions for which test does not work --
	| n < 3 || n `mod` 2 == 0 = error "Insert an odd number other than 3. "
	| otherwise = mR_Test a_List
		where
			mR_Test [] = True
			mR_Test (a0:aa) 
				|x `elem` [1,n-1] = mR_Test aa 
				|otherwise = mR_Test2 (mod (x^2) n, (s-1))
					where
						x = (modular_pow_bin_method a0 d n)
						mR_Test2 (a, t)
							| a == 1 = False
							| a == (n-1) = mR_Test aa
							| t == 0 = False
							| otherwise = mR_Test2 ((mod (a^2) n),(t-1))
			(s,d) = until (\(x,y) -> y `mod` 2 /= 0 ) (\(x,y) ->(x+1, y `div` 2)) (0, n-1)
			a_List = take k $ randomRs (2, n-2) (mkStdGen g)
		
miller_Rabin_PseudoPrime_specif n a_List
	| n < 3 || n `mod` 2 == 0 = error "Insert an odd number other than 3. "
        | otherwise = mR_Test a_List
            where
                    mR_Test [] = True
                    mR_Test (a0:aa) 
                            |x `elem` [1,n-1] = mR_Test aa 
                            |otherwise = mR_Test2  (mod (x^2) n, (s-1))
                                    where
                                            x = (modular_pow_bin_method a0 d n)
                                            mR_Test2 (a, t)
                                                    | a == 1 = False
                                                    | a == (n-1) = mR_Test aa
                                                    | t == 0 = False
                                                    | otherwise = mR_Test2 ((mod (a^2) n),(t-1))
                    (s,d) = until (\(x,y) -> y `mod` 2 /= 0 ) (\(x,y) ->(x+1, y `div` 2)) (0, n-1)
		
miller_Rabin_PseudoPrime_Bounded :: Integer -> Bool
miller_Rabin_PseudoPrime_Bounded n
	| n < 2047 = miller_Rabin_PseudoPrime_specif n [2]
	| n < 1373653 = miller_Rabin_PseudoPrime_specif n [2,3]
	| n < 9080191 = miller_Rabin_PseudoPrime_specif n [31,73]
	| n < 2152302898747 =  miller_Rabin_PseudoPrime_specif n [2,3,5,7,11]
	| n < 3825123056546413051 =  miller_Rabin_PseudoPrime_specif n [2,3,5,7,11,13,17,19,23]
	| otherwise = error "Number too large!"
