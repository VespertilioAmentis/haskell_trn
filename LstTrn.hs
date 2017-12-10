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

lastElem :: [a] -> a
lastElem = foldl1 (flip const)

lastTest1 = lastElem [1] == 1
lastTest2 = lastElem [1,2,3] == 3
lastTest3 = lastElem "qwertyuiop" == 'p'
lastTest4 = lastElem ['a' .. 'z'] == 'z'
lastTest5 = lastElem [10, 9, (-3)] == (-3)

allLastTests = testAll [lastTest1, lastTest2, lastTest3, lastTest4, lastTest5]
