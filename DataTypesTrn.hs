module DataTypesTrn where

import LstTrn
import Data.List

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


data Bit = Zero | One deriving (Eq, Enum)
data Sign = Minus | Plus deriving Eq
data Z = Z Sign [Bit]

instance Enum Sign where
    succ Minus = Plus
    pred Plus = Minus
    fromEnum Minus = (-1)
    fromEnum Plus = 1
    toEnum (-1) = Minus
    toEnum 1 = Plus
    toEnum _ = error "This number can't be a sign"

instance Show Sign where
    show Minus = "-"
    show Plus = ""

instance Show Bit where
    show Zero = "0"
    show One = "1"

instance Show Z where
    show (Z a [Zero]) = show a ++ show Zero
    show (Z a b) = show a ++ (toChars $ lstToDirectOrder b)
        where
            lstToDirectOrder :: [Bit] -> [Bit]
            lstToDirectOrder = dropWhile (== Zero) . reverse
            toChars :: [Bit] -> String
            toChars = concatMap show

instance Eq Z where
    (==) (Z a b) (Z c d) = (b == d) && (a == c)

toDecimal :: [Bit] -> Int
toDecimal = foldr getPow 0 . zip [0..]
    where
        getPow (x, y) z = z + fromEnum y * 2 ^ x

binToDec :: Z -> Int
binToDec (Z a b) = fromEnum a * toDecimal b

decTests = [toDecimal [Zero] == 0,
            toDecimal [Zero, One] == 2,
            toDecimal [One, Zero, One] == 5,
            toDecimal [Zero, Zero, Zero, Zero, One] == 16,
            toDecimal [One, Zero, Zero, One, One] == 25]

allDecTests = testAll decTests

btdTests = [binToDec (Z Minus [One]) == (-1),
            binToDec (Z Plus [One, Zero, Zero, One, One]) == 25,
            binToDec (Z Minus [Zero, Zero, Zero, Zero, Zero, Zero, One]) == (-64)]

allBtdTests = testAll btdTests

fromDecimal :: Int -> [Bit]
fromDecimal = reverse . (unfoldr decompositeInt)
    where
        decompositeInt :: Int -> Maybe (Bit, Int)
        decompositeInt n | n >=0 = Just (One,
                                         n - closestPow n)
                         | otherwise = Nothing
            where
                closestPow x = ceiling (log (fromIntegral x) / log 2)


add :: Z -> Z -> Z
add = undefined

mul :: Z -> Z -> Z
mul = undefined
