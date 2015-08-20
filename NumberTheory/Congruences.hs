module NumberTheory.Congruences
       ( fromCongClass
       , makeCongClass
       , Mod(..)
       , modInv
       , chineseRemainder
       ) where
	
import Data.List

--Implementation of congruence relations and equivalence classes as typeclasses and datatypes, respectively. Wordplay intended. --

data CongClass a = CongClass a a deriving Show
	
instance Mod a => Eq (CongClass a) where
	(CongClass n m) == (CongClass b p) = (modulo n m) == (modulo b p) && m == p

-- Converts a congruence class object to a function generating its members --
fromCongClass :: (Num a) => CongClass a -> (a -> a)
fromCongClass (CongClass n m) = (\x -> (m*x)+n )

-- Makes a CongClass object from 2 initial parameters.
makeCongClass a b = CongClass (a `mod` b) b

-- Modular arithmetic class: provides an interface for testing congruences between numbers
class Eq a => Mod a where
	cong :: a -> a -> a -> Bool
	ncong :: a -> a -> a -> Bool
	modulo :: a -> a -> a
	cong x y z = not (ncong x y z)
	ncong x y z = not (cong x y z)
	
--Instances of Mod are currently limited to Int and Integer. Others could be invented --
	
instance Mod Integer where
	modulo = mod
	cong x y z = (x `mod` z) == (y `mod` z)

instance Mod Int where
	modulo = mod
	cong x y z = (x `mod` z) == (y `mod` z)
	
{- Modular inversion algorithm: given two integral-class values a and n,
 produces the modular inverse of a w.r.t. n. This is done via the extended
 Euclidean algorithm, which is implemented recursively.-}

modInv :: (Integral a) => a -> a -> a
modInv a n
	| gcd a n /= 1  = error "First and second arguments of modInv must be relatively prime!"
	| otherwise     = mod (fst.fst $ extEA a n) n
		where
		-- Extended Euclidean Algorithm --
			extEA m0 n0 = eeA ((1,0),m0) ((0,1),n0)
				where
					eeA aa@((a1,a2),x) bb@((b1,b2),y)
						| y == 0    = aa
						| otherwise = eeA bb ((a1-(b1*q), a2-(b2*q)), x `mod` y)
							where
								q = x `quot` y

{- This implements the Chinese remainder theorem: given a list of pairs (a,n); this solves the corresponding system of congruences,
or returns an error if that cannot be done. -}
chineseRemainder :: (Integral a) => [(a,a)] -> CongClass a
chineseRemainder l 
	| not (pp $ map snd l) = error "The second elements of the pairs must be pairwise-coprime!"
	| otherwise            = CongClass (fst $ cR l) (snd $ cR l)
	where
		cR e = (mod (sum $ map (\(x,y) -> (n `div` y)* x * (modInv (n `div` y) y)) e) n, n)
			where
				n = product $ map snd e
		pp l = foldl1 (&&) $ map (foldl1 (&&) . (\(x0:xs) -> map ((1 ==). gcd x0) xs)) ((init.init) (tails l))

