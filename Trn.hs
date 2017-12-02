module Trn where

import Data.Function

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

multSecond = g `on` h
g = (*)
h x = snd x

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)
