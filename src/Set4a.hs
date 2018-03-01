module Set4a where
import MCPrelude
import Set2

class Monad m where
    (=<<) :: (a -> m b) -> m a -> m b
    (=<<) = flip bind
    bind :: m a -> (a -> m b) -> m b
--    (>>=) = bind
    return :: a -> m a 
    liftM2 :: (a -> b -> c) -> m a -> m b -> m c
    liftM2 f a b = bind a (\x -> (bind b (\y -> return (f x y))))
    liftM3 :: (a -> b -> c -> d) -> m a -> m b  -> m c  -> m d 
    liftM3 f a b c = bind a (\x -> (bind b (\y -> (bind c ( \z -> return (f x y z))))))
    join :: m (m a) -> m a
    join = (=<<) id 
    ap :: m(a -> b) -> m a -> m b
    ap mf ma = bind mf (\f -> (bind ma (\a -> return (f a))))
    fail :: String -> m a
    fail = undefined


instance Monad Maybe where 
    bind Nothing f = Nothing
    bind (Just y) f = f y
   
    return  = Just  
 
instance Monad [] where 
    bind  [] _ = []
    bind (a:as) f = f a ++ (bind as f)
    return a = [a]

newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }
instance Monad Gen where
    return a = Gen $ \s -> (a, s) -- I don't understand this
    bind ga f = Gen $ \s ->  
        let (a, s') = runGen ga s in runGen (f a) s'
    
evalGen :: Gen a -> Seed -> a
evalGen ga  = fst . runGen ga

sequence :: [Gen a] -> Gen [a]
-- sequence [] = Gen $ \s -> ([], s)
sequence [] = return []
sequence (g:gs) = bind g (\x -> (bind (sequence gs) (\y -> return (x:y))))

rand' = Gen rand
randLetter' = bind rand' $ return . toLetter 
    -- where toGenChar n = Gen $ \s -> (toLetter n, s)
    -- where toGenChar = return . toLetter

-- evalGen (sequence (replicate 3 rand'))  (mkSeed 1) == [16807,282475249,1622650073]
--
addSalaries2' salaryData p1 p2 = liftM2 (+) s1 s2
    where
        s1 = lookupMay p1 salaryData
        s2 = lookupMay p2 salaryData

