import MCPrelude

fiveRands :: [Integer]
fiveRands = map fst $ take 5 $ iterate ( rand . snd ) $ rand $ mkSeed 1

randString3 :: String
randString3 = map (toLetter . fst )  $ take 3 $ iterate ( rand . snd ) $ rand $ mkSeed 1

-- newtype Gen a Seed = Gen a Seed
data Gen a = Gen a Seed deriving(Eq, Show)
gen (a,s) = Gen a s
