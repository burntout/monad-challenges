{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set5 where
import MCPrelude
import Set2a

class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a

    fail :: String -> m a
    fail = undefined

instance Monad Maybe where 
    (>>=) Nothing f = Nothing
    (>>=) (Just y) f = f y
   
    return  = Just  

newtype Gen a = Gen {runGen :: Seed -> (a, Seed)}

instance Monad Gen where
    return a = Gen $ \s -> (a, s)
    ga >>= f = Gen $ \s ->
        let (a, s') = runGen ga s in runGen (f a) s'

evalGen :: Gen a -> Seed -> a 
evalGen ga = fst . runGen ga

makeRandom :: Gen Integer
makeRandom = Gen rand

sequence [] = return []
sequence (g:gs) =  g >>= (\x -> ((sequence gs) >>= (\y -> return (x:y))))

doseq [] = return []
doseq (g:gs) =
    do {x <- g;
        do {y <- (doseq gs);
            return (x:y)}}

randLetter = do
    r <- makeRandom
    return $ toLetter r

fiveRands :: Gen [Integer]
--fiveRands = do
-- let gs = replicate 5 makeRandom in
--  <- replicate 5 makeRandom 
fiveRands = doseq (replicate 5 makeRandom)

randString3 :: Gen [Char]
randString3 = doseq (replicate 3 randLetter)

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb =
    do { a <- ga ;
        do {b <- gb ;
            return (a, b) }}

greekDiv ma mb = 
    do { a <- ma ; 
        do { b <- mb ;
            divMay (fromIntegral a) (fromIntegral b) }}

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd key = greekDiv num denom
    where 
    xs = lookupMay key gd
    num = xs >>= tailMay >>= maximumMay
    denom = xs >>= headMay

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries salaryData p1 p2 =
    do { s1 <- lookupMay p1 salaryData;
        do { s2 <- lookupMay p2 salaryData;
            return (s1 + s2) }}
 
tailProd :: Num a => [a] -> Maybe a
tailProd ml = do
    l <- tailMay ml
    return $ product l
    
tailSum :: Num a => [a] -> Maybe a
tailSum ml = do
    l <- tailMay ml
    return $ sum l

tailMax :: Ord a => [a] -> Maybe a
tailMax ml = do
    l <- tailMay ml
    maximumMay l
    
