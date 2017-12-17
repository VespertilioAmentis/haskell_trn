module DataTypesTrn where

import LstTrn
import Data.List
import Data.Char(isDigit)

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
fromDecimal = reverse . dropWhile (==Zero) . toBin
    where
        toBin :: Int -> [Bit]
        toBin 0 = [Zero]
        toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [One]
                | n `mod` 2 == 0 = toBin (n `div` 2) ++ [Zero]


decToBin :: Int -> Z
decToBin x = Z (toEnum $ signum x) $ fromDecimal (abs x)

add :: Z -> Z -> Z
add x y = decToBin $ binToDec x + binToDec y

mul :: Z -> Z -> Z
mul x y = decToBin $ binToDec x * binToDec y

data Coord a = Coord a a
    deriving Show

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt ( (x2 - x1) ^ 2 + (y2 - y1) ^ 2 )

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x2 - x1) + abs (y2 - y1)

getCenter :: Double -> Coord Int -> Coord Double
getCenter width (Coord x y) =
    Coord (widthproj x) (widthproj y)
        where
            widthproj = projection width
            projection :: Double -> Int -> Double
            projection width x = width / 2 + fromIntegral x * width
            
getCell :: Double -> Coord Double -> Coord Int
getCell width (Coord x y) = 
    Coord (getNum x) (getNum y)
        where
            getNum = floor . (/width)

findDigit :: [Char] -> Maybe Char
findDigit = Just . head . (dropWhile (not . isDigit))
