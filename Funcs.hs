module Funcs where

firstLetter :: String -> String
firstLetter "" = "FUCK OFF WHAT THE EMPTY STRING"
firstLetter a@(x : xs) =
  "The first letter of "
    ++ a
    ++ " is "
    ++ [x]
    ++ ". and the rest part of it is "
    ++ xs

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height | bmi <= skinny   = "underweight"
                      | bmi <= standard = "normal"
                      | bmi <= fat      = "fat"
                      | otherwise       = "whale"
 where
  (skinny, standard, fat) = (18.5, 25.0, 30.0)
  bmi                     = weight / height ^ (2 :: Integer)

initials :: String -> String -> String
initials firstName lastName = [f] ++ "." ++ [l] ++ "."
 where
  (f : _) = firstName
  (l : _) = lastName

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [ bmi w h | (w, h) <- xs ]
  where bmi w h = w / h ^ (2 :: Integer)

calcAndFilterBmis :: (RealFloat a) => [(a, a)] -> [a]
calcAndFilterBmis xs =
  [ bmi | (w, h) <- xs, let bmi = w / h ^ (2 :: Integer), bmi > 25 ]

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

qs :: (Ord a) => [a] -> [a]
qs [] = []
qs (x : xs) =
  let smallOrEq = [ a | a <- xs, a <= x ]
      larger    = [ a | a <- xs, a > x ]
  in  qs smallOrEq ++ [x] ++ qs larger

chain :: Integer -> [Integer]
chain 1 = [1]
chain n | even n = n : chain (n `div` 2)
        | odd n  = n : chain (n * 3 + 1)

nChainsItHasMoreThan15Elem :: Int
nChainsItHasMoreThan15Elem = length (filter isLong (map chain [1 .. 100]))
  where isLong xs = length xs > 15

nChainsItHasMoreThan15ElemWithLambda :: Int
nChainsItHasMoreThan15ElemWithLambda =
  length (filter (\xs -> length xs > 15) (map chain [1 .. 100]))

-- Faster than mapWithFoldl, cuz '++' is slower than ':'
mapWithFoldr :: (a -> b) -> [a] -> [b]
mapWithFoldr f xs = foldr (\x acc -> f x : acc) [] xs

mapWithFoldl :: (a -> b) -> [a] -> [b]
mapWithFoldl f xs = foldl (\acc x -> acc ++ [f x]) [] xs

maximumWithFoldl1 :: (Ord a) => [a] -> a
maximumWithFoldl1 = foldl1 max

-- Record syntax
data Person = Person { 
  firstName :: String,
  lastName :: String,
  age :: Int,
  height :: Float,
  phoneNumber :: String,
  flavor :: String
} deriving (Show)