module Solution
    ( unique
    , pythagoreanTriples
    , primitivePythagoreanTriples
    , perfectNumbers
    , cantorPairs
    , minimalDistance
    ) where


import Data.List (nub)
import Data.List (minimumBy)
import Data.Function (on)
import Data.List
unique :: Eq a => [a] -> Bool
unique [] = True
unique (x:xs) = x `notElem` xs && unique xs

pythagoreanTriples :: [(Integer, Integer, Integer)]
pythagoreanTriples = [(a, b, c) | c <- [1..], b <- [1..c-1], a <- [1..b-1], a^2 + b^2 == c^2]

primitivePythagoreanTriples :: Integral a => [(a, a, a)]
primitivePythagoreanTriples = nub [(a, b, c) | c <- [1..], b <- [1..c-1], a <- [1..b-1], a^2 + b^2 == c^2, gcd a b == 1, gcd a c == 1, gcd b c == 1, odd(a - b)]

factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n `div` 2], n `mod` x == 0]

isPerfect :: Integral a => a -> Bool
isPerfect n = n == sum (factors n)

perfectNumbers :: Integral a => [a]
perfectNumbers = [n | n <- [1..], isPerfect n]

cantorPairs :: Integral a => [(a, a)]
cantorPairs = [(x, y) | i <- [0..], let (x, y) = cantorPairing i]

cantorPairing :: Integral a => a -> (a, a)
cantorPairing n = (x, y)
    where
    t = truncate ((sqrt (8 * fromIntegral n + 1) - 1) / 2)
    x = t * (t + 3) `div` 2 - n
    y = n - t * (t + 1) `div` 2

minimalDistance :: RealFloat a => [(a, a)] -> a
minimalDistance [] = 1/0
minimalDistance [_] = 1/0
minimalDistance l = undefined