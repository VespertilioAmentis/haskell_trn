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


data Bit = Zero | One deriving Eq
data Sign = Minus | Plus deriving Eq
data Z = Z Sign [Bit]

instance Show Sign where
    show Minus = "-"
    show Plus = "+"

instance Show Bit where
    show Zero = "0"
    show One = "1"

instance Show Z where
    show (Z a b) = show a ++ (show $ lstToDirectOrder b)
        where
            lstToDirectOrder :: [Bit] -> [Bit]
            lstToDirectOrder = dropWhile (== Zero) . reverse

instance Eq Z where
    (==) (Z a b) (Z c d) = (b == d) && (a == c)

add :: Z -> Z -> Z
add = undefined

mul :: Z -> Z -> Z
mul = undefined

{-
-- Thanks to Eugene Kalashnikov for these tests
test001 = (add (Z Plus []) (Z Plus [])) == Z Plus []
test002 = (add (Z Plus []) (Z Plus [One])) == Z Plus [One]
test003 = (add (Z Plus []) (Z Minus [One])) == Z Minus [One]

test011 = (add (Z Plus [Zero, One, One]) (Z Plus [One])) == Z Plus [One, One, One]
test012 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One])) == Z Plus [Zero, Zero, Zero, One]
test013 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus [Zero, Zero, One, One]

test021 = (add (Z Minus [Zero, One, One]) (Z Minus [One])) == Z Minus [One, One, One]
test022 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One])) == Z Minus [Zero, Zero, Zero, One]
test023 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One, One]

test031 = (add (Z Minus [Zero, One, One]) (Z Plus [One])) == Z Minus [One, Zero, One]
test032 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One])) == Z Minus [Zero, Zero, One]
test033 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus []

test041 = (add (Z Plus [Zero, One, One]) (Z Minus [One])) == Z Plus [One, Zero, One]
test042 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One])) == Z Plus [Zero, Zero, One]
test043 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Plus []

test051 = (add (Z Plus [One]) (Z Minus [One])) == Z Plus []
test052 = (add (Z Plus [One]) (Z Minus [One, One])) == Z Minus [Zero, One]
test053 = (add (Z Plus [One]) (Z Minus [Zero, One])) == Z Minus [One]
test054 = (add (Z Plus [One]) (Z Minus [Zero, Zero, Zero, One])) == Z Minus [One, One, One]
test055 = (add (Z Plus [One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, Zero, One]
test056 = (add (Z Plus [Zero, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One]
test057 = (add (Z Plus [Zero, One]) (Z Minus [Zero, Zero, One])) == Z Minus [Zero, One]
test058 = (add (Z Plus [One, Zero, One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, One]


test101 = (mul (Z Plus []) (Z Plus [])) == emptyZ
test102 = (mul (Z Plus []) (Z Plus [One])) == emptyZ
test103 = (mul (Z Plus []) (Z Minus [One])) == emptyZ
test104 = (mul (Z Plus [One]) (Z Plus [])) == emptyZ
test105 = (mul (Z Minus [One]) (Z Plus [])) == emptyZ

test111 = (mul (Z Plus [One]) (Z Plus [One])) == Z Plus [One]
test112 = (mul (Z Minus [One]) (Z Plus [One])) == Z Minus [One]
test113 = (mul (Z Plus [One]) (Z Minus [One])) == Z Minus [One]
test114 = (mul (Z Minus [One]) (Z Minus [One])) == Z Plus [One]

test121 = (mul (Z Plus [One]) (Z Plus [Zero, One])) == Z Plus [Zero, One]
test122 = (mul (Z Plus [Zero, Zero, One]) (Z Plus [Zero, Zero, One])) == Z Plus [Zero, Zero, Zero, Zero, One]

test131 = (mul (Z Plus [One, Zero, One, Zero, One]) (Z Plus [One, One, One])) == Z Plus [One, One, Zero, Zero, One, Zero, Zero, One]


testAdd = [test001, test002, test003, test011, test012, test013, test021, test022, test023, test031, test032, test033, test041, test042, test043, test051, test052, test053, test054, test055, test056, test057, test058]
testMul = [test101, test102, test103, test104, test105, test111, test112, test113, test114, test121, test122, test131]

testAddMul = testAdd ++ testMul

zippedTests = testAll testAddMul
-}

