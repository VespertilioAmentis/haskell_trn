module LstTrn where

import Data.List

testAll :: [Bool] -> [(Int, Bool)]
testAll = zip [1..]

meanList :: [Double] -> Double
meanList x = (/ fromIntegral (length x)) $ foldr (+) 0 x

evenOnly :: [a] -> [a]
evenOnly x = foldr appendIfEven [] (zip x [1..])
    where
        appendIfEven (x,n) xs | mod n 2 == 0 = (x:xs)
                              | otherwise = xs

evTest1 = evenOnly [1 .. 10] == [2,4,6,8,10]
evTest2 = evenOnly ['a'..'z'] == "bdfhjlnprtvxz"
evTest3 = evenOnly [2, 4, 6, 8, 10] == [4, 8]

allEvenTests = testAll [evTest1, evTest2, evTest3]
