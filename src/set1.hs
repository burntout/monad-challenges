import MCPrelude

type Gen a = Seed -> (a, Seed)

generalA :: (a -> b) -> Gen a -> Gen b
generalA f prf s = (f $ fst $ prf s, snd $ prf s)

rand' :: Gen Integer
rand' = rand

fiveRands :: [Integer]
-- fiveRands = map fst $ take 5 $ iterate ( rand . snd ) $ rand $ mkSeed 1
fiveRands = take 5 $ map fst $ iterate ( rand' . snd ) $ rand' $ mkSeed 1
-- 
randLetter :: Gen Char
-- randLetter s = (toLetter $ fst $ rand' s, snd $ rand' s)
randLetter = generalA toLetter rand'
-- 
randString3 :: String
-- randString3 = map (toLetter . fst ) $ take 3 $ iterate ( rand . snd ) $ rand $ mkSeed 1
randString3 = take 3 $ map fst $ iterate ( randLetter . snd ) $ randLetter $ mkSeed 1

randEven :: Gen Integer -- the output of rand * 2
-- randEven s = ((*2) $ fst $ rand' s, snd $ rand' s)
randEven = generalA (*2) rand'

randOdd :: Gen Integer -- the output of rand * 2 + 1
-- randOdd s = (((+1) . (*2)) $ fst $ rand' s, snd $ rand' s)
-- randOdd s = ((+1)  $ fst $ randEven s, snd $ randEven s)
randOdd = generalA (+1) randEven

randTen :: Gen Integer -- the output of rand * 10
-- randTen s = ((*10) $ fst $ rand' s, snd $ rand' s)
randTen = generalA (*10) rand'

randPair :: Gen (Char, Integer)
-- randPair s = ((chr, int ),s'')
--     where 
--         (chr,s') = randLetter s 
--         (int, s'') = rand' s'
randPair = generalPair randLetter rand'

generalPair :: Gen a -> Gen b -> Gen (a,b)
-- generalPair x y s = ((a, b), s'')
--     where
--         (a, s') = x s 
--         (b, s'') = y s'

generalPair = generalB (,)

generalB :: (a -> b -> c ) -> Gen a -> Gen b -> Gen c
generalB f x y s = (f a b, s'')
    where 
        (a, s') = x s
        (b, s'') = y s'

repRandom :: [Gen a] -> Gen [a]
-- repRandom [] = \s -> ([],s)
repRandom [] = \s -> ([], s)
repRandom (x:xs) = generalB (:) x $ repRandom xs
   
