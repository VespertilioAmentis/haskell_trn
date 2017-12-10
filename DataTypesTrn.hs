module DataTypesTrn where

import LstTrn

data Color = Red | Green | Blue

instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"

data LogLevel = Info | Warning | Error deriving (Show, Enum)

cmp :: LogLevel -> LogLevel -> Ordering
cmp x y = compare (fromEnum x) $ fromEnum y

cmpTests = [cmp Error Error == EQ,
            cmp Info Info == EQ,
            cmp Warning Warning == EQ,
            cmp Info Error == LT,
            cmp Info Warning == LT,
            cmp Error Info == GT,
            cmp Error Warning == GT,
            cmp Warning Error == LT,
            cmp Warning Info == GT]

allCmpTests = testAll cmpTests

data Shape = Circle Double | Rectangle Double Double deriving Show

area :: Shape -> Double
area (Circle r) | r < 0 = error "Negative radius"
                | otherwise = pi * r ^ 2
area (Rectangle a b) | a < 0 || b < 0 = error "Negative sides"
                     | otherwise = a * b


