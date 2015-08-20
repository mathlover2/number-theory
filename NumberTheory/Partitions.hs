module NumberTheory.Partitions
	where

-- The functions below are designed to construct lists corresponding to partitions of n elements.

partitions :: Int -> [[Int]]

partitions n =  (iterate partGen [[1]]) !! n
  where partGen [l] = map (:l) [1..max_of_l_plus_one]
          where max_of_l_plus_one = (succ . maximum) l
        partGen (l0:ls) = partGen [l0] ++ (partGen ls)
