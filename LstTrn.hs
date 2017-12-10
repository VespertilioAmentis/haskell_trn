module LstTrn where

testAll :: [Bool] -> [(Int, Bool)]
testAll = zip [1..]

meanList :: [Double] -> Double
meanList x = (/ fromIntegral (length x)) $ foldr (+) 0 x

evenOnly :: [a] -> [a]
evenOnly = id

evTest1 = evenOnly [1 .. 10] == [2,4,6,8,10]
evTest2 = evenOnly ['a'..'z'] == "bdfhjlnprtvxz"
evTest3 = evenOnly [2, 4, 6, 8, 10]

allEvenTests = testAll [evTest1, evTest2]
