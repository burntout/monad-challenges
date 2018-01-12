module Set4 where
import MCPrelude

type Gen a = Seed -> (a, Seed)

data Maybe a  = Nothing | Just a
instance Show a => Show (Maybe a) where 
    show (Just a) = "Just " ++ (show a)
    show Nothing = "Nothing"
    
instance Eq a => Eq (Maybe a) where
    (==) Nothing Nothing = True
    (==) (Just a) (Just b) = (==) a b

generalA :: (a -> b) -> Gen a -> Gen b
generalPair :: Gen a -> Gen b -> Gen (a,b)
mkGen :: a -> Gen a

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
mkMaybe :: a -> Maybe a
combine :: Maybe (Maybe a) -> Maybe a

genTwo :: Gen a -> (a -> Gen b) -> Gen b
link :: Maybe a -> (a -> Maybe b) -> Maybe b

generalB :: (a -> b -> c ) -> Gen a -> Gen b -> Gen c
yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c


mLink :: m a -> (a -> m b) -> m b
mYLink :: (a -> b -> c) -> m a -> m b -> m c
