module Set2 where
import Set1
import MCPrelude

data Maybe a  = Nothing | Just a
instance Show a => Show (Maybe a) where 
    show (Just a) = "Just " ++ (show a)
    show Nothing = "Nothing"
    
instance Eq a => Eq (Maybe a) where
    (==) Nothing Nothing = True
    (==) (Just a) (Just b) = (==) a b

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (a:as) = Just a


tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (a:as) = Just as

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay a (x:xs) 
        | (fst x) == a = Just (snd x)
        | otherwise = lookupMay a xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay x y 
    | y == 0 = Nothing
    | otherwise = Just (x/y)


maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay p@(x:xs) = Just (foldl max x p)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay p@(x:xs) = Just (foldl min x p)

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd key = maybeDivOf (maybeMaxOf $ maybeTailOf xs) (maybeHeadOf xs) 
        where xs = lookupMay key gd
              maybeTailOf maybeList = 
                  case maybeList of Nothing -> Nothing
                                    Just ys -> tailMay ys 
              maybeHeadOf maybeList = 
                  case maybeList of Nothing -> Nothing
                                    Just ys -> headMay ys 
              maybeMaxOf maybeList = 
                  case maybeList of Nothing -> Nothing
                                    Just ys -> maximumMay ys 
              maybeDivOf maybeNum maybeDen =
                  case maybeNum of Nothing -> Nothing
                                   Just aNum -> maybeDivOf' aNum maybeDen
              maybeDivOf' aNum maybeDen =
                  case maybeDen of Nothing -> Nothing
                                   Just aDen -> divMay (fromIntegral aNum)  (fromIntegral aDen)

              
chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f Nothing = Nothing
chain f (Just y) = f y

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link Nothing f = Nothing
link (Just y) f = f y
            
queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gd key =  link (link (link xs tailMay) maximumMay) (\m -> (link (link xs headMay) (\h -> divMay (fromIntegral m) (fromIntegral h))))
        where xs = lookupMay key gd

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries salaryData p1 p2 = link s1 (\x -> (link s2 (\y -> Just (x + y))))
    where
        s1 = lookupMay p1 salaryData
        s2 = lookupMay p2 salaryData


--ylink :: Maybe a-> Maybe b -> (a -> b -> Maybe c) -> Maybe c
--ylink m n f = link m (\x -> (link n (\y -> f x y)))

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
-- yLink f _ Nothing = Nothing
-- yLink f Nothing _ = Nothing
-- yLink f (Just x) (Just y) = Just (f x y)
yLink f a b = link a (\x -> (link b (\y -> Just (f x y))))

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 salaryData p1 p2 = yLink (+) s1 s2
    where
        s1 = lookupMay p1 salaryData
        s2 = lookupMay p2 salaryData

mkMaybe :: a -> Maybe a
mkMaybe = Just

tailProd :: Num a => [a] -> Maybe a
tailProd xs = link (tailMay xs) (Just . product) 

tailSum :: Num a => [a] -> Maybe a
tailSum xs = link (tailMay xs) (Just . sum) 

--transMaybe :: a -> ( a -> Maybe b) -> ( b -> c) -> Maybe c
--transMaybe a f g = link (f a) (Just . g)  

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f m = link m (Just . f)

tailProd' :: Num a => [a] -> Maybe a
tailProd' xs = transMaybe product $ tailMay xs

tailSum' :: Num a => [a] -> Maybe a
tailSum' xs = transMaybe sum $ tailMay xs

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax xs = transMaybe maximumMay $ tailMay xs

combine :: Maybe (Maybe a) -> Maybe a
combine m = link m id
