module Set3 where
-- import Set2
-- import Set1
import MCPrelude

allPairs :: [a] -> [b] -> [(a,b)]
--allPairs _ [] = []
--allPairs [] _ = []
--allPairs (x:xs) y = (allPairs' x y) ++ (allPairs xs y)

--allPairs' x [] = []
--allPairs' x (y:ys) = (x,y):(allPairs' x ys)

data Card = Card Int [Char]
instance Show Card where
        show (Card x b) = (show x) ++ b 

allCards :: [Int] -> [String] -> [Card]
--allCards _ [] = []
--allCards [] _ = []
--allCards (x:xs) y = (allCards' x y) ++ (allCards xs y)

--allCards' x [] = []
--allCards' x (y:ys) = (Card x y):(allCards' x ys)

-- allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
--allCombs _ _ [] = []
--allCombs _ [] _ = []
--allCombs f (x:xs) y = (allCombs' f  x y) ++ (allCombs f xs y)
--        where allCombs' f x [] = []
--              allCombs' f x (y:ys) = (f x y):(allCombs' f x ys)

-- allPairs = allCombs (\x y -> (x,y)) 
allPairs = allCombs (,) 
allCards = allCombs Card 


-- allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
--allCombs3 _ _  _ [] = []
--allCombs3 _ _  [] _ = []
--allCombs3 _ [] _  _ = []
--allCombs3 f (x:xs) y z = (allCombs3' f  x y z) ++ (allCombs3 f xs y z)
--        where allCombs3' f x [] z = []
--              allCombs3' f x (y:ys) z = (allCombs3'' f x y z) ++ (allCombs3' f x ys z)
--              allCombs3'' f x y [] = []
--              allCombs3'' f x y (z:zs) = (f x y z):(allCombs3'' f x y zs)
--

--combStep :: [a -> b] -> [a] -> [b]
--combStep _ [] = []
--combStep [] _ = []
--combStep (f:fs) y = (combStep' f y) ++ (combStep fs y)
--        where combStep' f [] = []
--              combStep' f (y:ys) = (f y):(combStep' f ys)

combStep :: [a -> b] -> [a] -> [b]
combStep _ [] = []
combStep [] _ = []
combStep (f:fs) y = (map f y) ++ (combStep fs y)
-- combStep (f:fs) y = map (map f) fs

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f as bs = combStep (map f as) bs 



allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
-- allCombs3 f as bs cs = map (combStep (map f as) bs) cs 
allCombs3 f as bs cs =  combStep (combStep (map f as) bs ) cs

allCombs4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
-- allCombs3 f as bs cs = map (combStep (map f as) bs) cs 
allCombs4 f as bs cs ds =  combStep (combStep (combStep (map f as) bs ) cs) ds
