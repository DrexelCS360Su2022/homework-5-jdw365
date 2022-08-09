{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

import Set

--
--Q1
--

--1.1
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

--1.2
toDigits :: Integer -> [Integer]
toDigits n | (n <= 0)  = reverse []
           | otherwise = (toDigits (n `div` 10)) ++ [(n `mod` 10)]

--1.3
everyOtherTrue :: Int -> [Bool]
everyOtherTrue n | (n == 0) = []
                 | ((n `mod` 2) == 0) = True : everyOtherTrue (n - 1)
                 | ((n `mod` 2) == 1) = False : everyOtherTrue (n - 1)
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (xs) | (head (everyOtherTrue (length xs))) = (2 * (head xs)) : doubleEveryOther (tail xs)
                      | otherwise                           = (head xs) : doubleEveryOther (tail xs)

--1.4
sumDigitsHelper :: Integer -> Integer
sumDigitsHelper n | (n == 0)  = 0
                  | otherwise = (n `mod` 10) + sumDigitsHelper (n `div` 10)
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (xs) = (sumDigitsHelper (head xs)) + sumDigits (tail xs)

--1.5
validate :: Integer -> Bool
validate n | (((sumDigits (doubleEveryOther (toDigits n))) `mod` 10) == 0) = True
           | otherwise                                                     = False

--
--Q2
--

--2.1
square :: Integer -> Integer
square x = x * x
pow :: (a -> a) -> Int -> a -> a
pow f 0 = id
pow f n = f . (pow f (n - 1))

--2.2
g :: Integer -> Integer
g 0 = 0
g n = n - (pow g 2 (n-1))
h :: Integer -> Integer
h 0 = 0
h n = n - (pow h 3 (n-1))

--2.3
d :: Int -> Integer -> Integer
d i 0 = 0
d i n = n - (pow (d i) i (n-1))


--Q3
powerSet :: Ord a => Set a -> Set (Set a)
powerSet s | isEmpty s = singleton empty
           | otherwise = union (mapSet (insert x) (powerSet xs)) (powerSet xs) where (x, xs) = split s