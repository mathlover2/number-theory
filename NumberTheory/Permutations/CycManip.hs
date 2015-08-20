module NumberTheory.Permutations.CycManip
       where

import Data.List

{- Replaces part of a list with a specific element in a list. If the
list has multiple arguments, the elements of the list are substituted. -}

replacePart :: Eq a => Int -> [a] -> [a] -> [a]
replacePart n x l = take (n-1) l ++ x ++ drop n l

{- Replaces multiple consecutive parts of a list specified in the ordered pair of the first argument, substituting the elements 
of another list -}

replaceParts :: (Eq a) => (Int, Int) -> [a] -> [a] -> [a]
replaceParts (start, end) l1 l2
    | start >= end = error "Start index too large."
    | start <= 0 || end <= 0 = error "Non-positive argument."
    | end > length l1  = error "End index too large."
    | otherwise = take (start-1) l1 ++ l2 ++ drop end l1


multiint, multiuni :: Eq a => [[a]] -> [a]
multiint = foldl1 union
multiuni = foldl1 intersect

type Perm = [[Int]]

cycleprodQ :: Perm -> Bool
cycleprodQ xs = nub (concat xs) == concat xs

rotateList l n
  | n >= 0 && ln > n = (drop n l) ++ (take n l)
  | otherwise        = rotateList l (mod n ln)
  where ln = length l

cycleRotate l (n,nn)
  | n > length l = error "Part length excessively large! "
  | otherwise    = replacePart n [rotateList (l !! (n - 1)) nn] l

cycleDecompose l n
  | n > length l = error "Part length excessively large!"
  | otherwise      = replacePart n decomposed l
  where decomposed = decomposeCycle (l !! (n-1))
        decomposeCycle (l0:ls) = map (\x -> [l0,x]) ls

cycleCompose :: Perm -> (Int,Int) -> Perm
cycleCompose l (start,end)
  | start >= end   = error "Start position can't go before end position!"
  | end > length l = error "End position can't exceed length of list!"
  | any (/=2) (map length cycles_to_compose)
                   = error "Composed cycles must be of length 2!"
  | length (multiint cycles_to_compose) /= 1
                   = error "Cycles must pairwise share exactly one element!"
  | otherwise      = replaceParts (start, end) l
                     [(nub (concat cycles_to_compose))]
  where cycles_to_compose = [ l !! n | n <- [(start-1)..(end-1)]]

involuteCancel :: Perm -> Int -> Perm
involuteCancel l n
    | n > (length l) - 1 = error "Position can't exceed length of list!"
    | (l !! (n - 1)) /=  (l !! n)  = error "No involution here to cancel!"
    | otherwise = replaceParts (n, n + 1) l []

cycleCommute :: Perm -> Int -> Perm
cycleCommute l n
  | n > (length l) - 1 = error "Position can't exceed length of list!"
  | intersect (l !! (n-1)) (l !! n) /= [] =
    error "Cycle at position not pairwise disjoint with its neighbour!"
  | otherwise = replaceParts (n, n + 1) l [(l !! n), (l !! (n-1))]

cycleManip :: String -> Perm -> Perm
cycleManip cmds l = foldr cM l (words cmds)
  where cM command perm
          = case command
            of ('k':':':xs) -> cycleDecompose perm (read xs)
               ('r':':':xs) -> cycleRotate perm (read2 xs)
               ('i':':':xs) -> involuteCancel perm (read xs)
               ('p':':':xs) -> cycleCompose perm (read2 xs)
               ('c':':':xs) -> cycleCommute perm (read xs)
               _ -> error "Improper syntax: Unsupported command."

read2 :: String -> (Int,Int)
read2 str = let (n1,s) = (\st -> break (==',') st) str
                (',':n2) = s
            in  (read n1, read n2)
