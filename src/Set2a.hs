module Set2a where
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
