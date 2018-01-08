module Set2 where
import Set1
import MCPrelude

data Maybe a  = Nothing | Just a
instance Show a => Show (Maybe a) where 
    show (Just a) = "Just " ++ (show a)
    show Nothing = "Nothing"


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
            

queryGreek2 gd key =  link (link (link xs tailMay) maximumMay) (\m -> (link (link xs headMay) (\h -> divMay (fromIntegral m) (fromIntegral h))))
        where xs = lookupMay key gd
