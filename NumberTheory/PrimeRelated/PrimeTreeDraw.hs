module NumberTheory.PrimeRelated.PrimeTreeDraw
	where
	
import Data.Tree
import NumberTheory.PrimeRelated


primeFacTree :: (FactorMethod, FactorMethod) -> Integer ->  Either (Tree Integer) ([Tree Integer])
primeFacTree (fm1, fm2) n
	| primeQ n        = Left $ unfoldTree fctr n
	| not $ primeQ n  = Right $ unfoldForest fctr (factorList fm1 (n))
		where
			fctr = (\x -> if x == 2 then (x, []) else (x, factorList fm1 (x - 1)))

drawArbTree :: (Show a) => Tree a -> String
drawArbTree tree = drawTree $ fmap show tree

drawArbForest :: (Show a) => Forest a -> String
drawArbForest forest = drawForest $ map (fmap show) forest

drawPFT ::  Either (Tree Integer) ([Tree Integer]) -> String
drawPFT ft = either (drawArbTree) (drawArbForest) ft
