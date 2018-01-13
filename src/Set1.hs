module Set1 where
import MCPrelude

type Gen a = Seed -> (a, Seed)

-- fiveRands :: [Integer]
-- fiveRands = [r,r',r'',r''',r'''']
--     where 
--         (r,s') = rand $ mkSeed 1 
--         (r',s'') = rand s'
--         (r'',s''') = rand s''
--         (r''',s'''') = rand s'''
--         (r'''',s''''') = rand s''''
-- fiveRands = map fst $ take 5  $ iterate (\x -> (rand (snd x))) $ rand $ mkSeed 1 
fiveRands = take 5 $ f $ mkSeed 1
f seed = r : (f seed')
    where (r, seed') = rand seed

randLetter :: Seed -> (Char, Seed)
randLetter s = (l, s')
    where 
        (r,s') = rand s
        l = toLetter r

randString3 = take 3 $ randLetters $ mkSeed 1 
randLetters seed = l : (randLetters seed')
    where (l, seed') = randLetter seed

randEven :: Seed -> (Integer, Seed)
randEven s = (l, s')
    where 
        (r,s') = rand s
        l = r * 2

randOdd :: Seed -> (Integer, Seed)
randOdd s = (l, s')
    where 
        (r,s') = rand s
        l = r * 2 +1

randTen :: Seed -> (Integer, Seed)
randTen s = (l, s')
    where 
        (r,s') = rand s
        l = r * 10

randPair :: Gen (Char, Integer)
randPair s = ((a, b), s'')
    where  
        (a, s') = randLetter s
        (b, s'') = rand s'

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb s = ((a, b), s'')
    where  
        (a, s') = ga s
        (b, s'') = gb s'

generalB :: (a -> b -> c) ->  Gen a -> Gen b -> Gen c
generalB f ga gb s = (f a b, s'')
    where  
        (a, s') = ga s
        (b, s'') = gb s'

generalPair2 = generalB (,)

generalCons :: Gen a -> Gen [a] -> Gen [a]
generalCons ga gla s = (a:as, s'')
    where
        (a, s') = ga s
        (as, s'') = gla s'


notrepRandom :: [Gen a] -> Seed -> [a]
notrepRandom [] seed  = []
notrepRandom (g:gs) seed = r : (notrepRandom gs seed')
        where (r, seed') = g seed

generalA :: (a -> b) -> Gen a -> Gen b
generalA f ga s = (f r, s')
    where 
        (r,s') = ga s

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo ga f s = (f a) s'
    where 
        (a, s') = ga s

mkGen :: a -> Gen a
mkGen a = (\s -> (a ,s))

repRandom :: [Gen a] -> Gen [a]
repRandom [] = \s -> ([], s)
--repRandom (g:gs) = generalCons g $ repRandom gs
repRandom (g:gs) = generalB (:) g $ repRandom gs

generalB2 :: (a -> b -> c ) -> Gen a -> Gen b -> Gen c
generalB2 f ga gb = genTwo ga (\x -> (genTwo gb (\y -> mkGen (f x y))))
