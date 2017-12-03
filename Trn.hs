module Trn where

import Data.Function
import Data.Char

fibonacci 0 = 0
fibonacci n = fibacc 1 0 n
fibacc acc prev 0 = acc
fibacc acc prev 1 = acc
fibacc acc prev n | n > 0 = fibacc (acc + prev) acc (n - 1)
                  | n < 0 = (-1) ^ (-n + 1) * fibacc acc prev (-n)

{-
    seqA 0 = 1
    seqA 1 = 2
    seqA 2 = 3
    seqA n = seqA (n - 1) + seqA (n - 2) - 2 * seqA (n - 3)
-}
seqA n  | n < 0 = error "n must be positive"
        | n < 3 = n + 1
        | otherwise =
            let
                helper k3 _ _ 2 = k3
                helper k3 k2 k1 n = helper
                                        (k3 + k2 - 2 * k1)
                                        k3
                                        k2
                                        (n - 1)
            in helper 3 2 1 n

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0, 1)
sum'n'count x | x < 0 = sum'n'count (-x)
              | otherwise = helper x 0 0
    where
        helper 0 sum cnt = (sum, cnt)
        helper x sum cnt = helper (div x 10) (sum + mod x 10) (cnt + 1)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * (get1st f a b + getrest f (a + h))
    where
        h = (b - a) / n
        n = 1000
        get1st :: (Double -> Double) -> Double -> Double -> Double
        get1st f x0 xn = (f x0 + f xn) / 2.0
        getrest :: (Double -> Double) -> Double -> Double
        getrest f x = getrest_acc f x h 0 (n - 1) 0.0
            where
                getrest_acc :: (Double -> Double) 
                               -> Double -> Double 
                               -> Double -> Double
                               -> Double -> Double
                getrest_acc f x incr step steps acc
                    | step >= steps = acc
                    | otherwise = getrest_acc f (x + incr) incr 
                                                (step + 1) steps 
                                                (acc + f x)

multSecond = g' `on` h'
g' = (*)
h' x = snd x

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

doItYourself = f . g . h
f = logBase 2
g = (^3)
h = max 42

class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString True = "true"
    toString False = "false"

instance Printable () where
    toString _ = "unit type"

instance (Printable a, Printable b) => Printable (a,b) where
    toString (x, y) = "(" ++ toString x ++ "," ++ toString y ++ ")"

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab x | doesEnrageMork x && doesEnrageGork x = stomp (stab x)
                  | doesEnrageMork x = stab x
                  | doesEnrageGork x = stomp x
                  | otherwise = x

class (Enum a, Bounded a, Eq a) => SafeEnum a where
    ssucc :: a -> a
    ssucc x | x == maxBound = minBound
            | otherwise = succ x

    spred :: a -> a
    spred x | x == minBound = maxBound
            | otherwise = pred x

instance SafeEnum Bool where
instance SafeEnum Int where

avg :: Int -> Int -> Int -> Double
avg x y z =  sum lst / fromIntegral (length lst)
    where lst = [fromIntegral x, fromIntegral y, fromIntegral z]

addTwoElements a b c = a : b : c

nTimes:: a -> Int -> [a]
nTimes val cnt = nTimes_acc val cnt []
    where
        nTimes_acc :: a -> Int -> [a] -> [a]
        nTimes_acc _ 0 a = a
        nTimes_acc v c a = nTimes_acc v (c - 1) (v : a)

oddsOnly xs = oddsOnly_acc xs []
    where
        oddsOnly_acc [] acc = reverse acc
        oddsOnly_acc (x:xs) acc | odd x = oddsOnly_acc xs (x : acc)
                                | otherwise = oddsOnly_acc xs acc


oddsOnly' [] = []
oddsOnly' (x:xs) | odd x = x : oddsOnly' xs
                 | otherwise = oddsOnly' xs

product' [] = 1
product' (0:_) = 0
product' (x:xs) = x * product xs

sum' [] = 0
sum' (x:xs) = x + sum' xs


errEmpty = error "Empty list"

maximum' [] = errEmpty
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

minimum' [] = errEmpty
minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == reverse l

sum3 [] [] [] = []
sum3 x y z = sumheads x y z : sum3 (smarttail x) (smarttail y) (smarttail z)
    where
        sumheads x y z = smarthead x + smarthead y + smarthead z
            where
                smarthead [] = 0
                smarthead (x:_) = x
        smarttail [] = []
        smarttail (_:xs) = xs

groupElems [] = []
groupElems (x:xs) = fst (spanned (x:xs)) : groupElems (snd $ spanned (x:xs))
    where
        spanned (x:xs) = span (==x)(x:xs)

readDigits = span isDigit

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj pr1 pr2 (x:xs) | pr1 x || pr2 x = x : filterDisj pr1 pr2 xs
                          |otherwise = filterDisj pr1 pr2 xs

qsort [] = []
qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++ qsort (filter (>= x) xs) 

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x ^ 2, x ^ 3])

getPermsCnt x = product [1..(length x)]

getPermPairsViaShift :: Integral b => [[a]] -> b -> [a] -> [[a]]
getPermPairsViaShift acc 0 _ = acc
getPermPairsViaShift acc n xx@(x:xs) =
    getPermPairsViaShift (xx : reverse xx : acc)
                         (n - 1)
                         (xs ++ [x])

perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms xx =
    let
        getShiftsCnt x = div (product [1..(length x)]) 2

        getPermPairsViaShift :: Integral b => [[a]] -> b -> [a] -> [[a]]
        getPermPairsViaShift acc 0 _ = acc
        getPermPairsViaShift acc n xx@(x:xs) =
            getPermPairsViaShift (xx : reverse xx : acc)
                                 (n - 1)
                                 (xs ++ [x])
    in
        getPermPairsViaShift [] (getShiftsCnt xx) xx
