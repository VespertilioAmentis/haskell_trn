module DataTypesTrn where

import LstTrn
import Data.List
import Data.Char(isDigit)
import Data.Maybe (fromMaybe)

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
findDigit x | length (trimmedDigits x) == 0 = Nothing
            | otherwise = Just $ head $ trimmedDigits x
    where
        trimmedDigits = dropWhile (not . isDigit)

findDigitOrX :: [Char] -> Char
findDigitOrX x =
    case findDigit x of
        Nothing -> 'X'
        Just x -> x
                   
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe x = Just $ head x

data Error = ParsingError | IncompleteDataError | IncorrectDataError String
    deriving (Show, Eq)

data Person = Person { firstName :: String, lastName :: String, age :: Int }
    deriving (Show, Eq)

ageName = "age"
fname = "firstName"
lname = "lastName"

spacedEq = " = "

hasSpacedEqSign :: String -> Bool
hasSpacedEqSign =  isInfixOf spacedEq

filterPerson :: String -> [(String, String)]
filterPerson =  toTupleList . filter (isFLA) . filter hasSpacedEqSign . lines
    where
        toTupleList :: [String] -> [(String, String)]
        toTupleList = map trimSnd . map (span (/= ' '))
            where
                trimSnd x = (fst x, drop (length spacedEq) $ snd x)
        isFLA str = or $ map (`isInfixOf` str) [fname, lname, ageName]

checkFmt :: String -> Bool
checkFmt x = 
    (length eqList == (length $ splitted)) && (all (==3) $ map length $ map words $ splitted)
        where
            splitted = lines x
            eqList = filter hasSpacedEqSign $ splitted
        
        

parsePerson :: String -> Either Error Person
parsePerson x | not $ checkFmt x = Left ParsingError
              | (length $ filterPerson x) < 3 = Left IncompleteDataError
              | otherwise = makePerson $ orderVals $ filterPerson x
    where
        makePerson :: [String] -> Either Error Person
        makePerson x | not $ all isDigit $ x !! 2 = Left $ IncorrectDataError $ x !! 2
                     | otherwise = Right $ Person {firstName = x !! 0,
                                                   lastName = x !! 1,
                                                   age = read (x !! 2) :: Int}
        orderVals :: [(String, String)] -> [String]
        orderVals x = [findByStr fname x, findByStr lname x, findByStr ageName x]
            where
                findByStr :: String -> [(String, String)] -> String
                findByStr str = snd . fromMaybe (("Can't find " ++ str, "")) . find ((==str) . fst)
              

-- wrong Parse | empty string
t0 = parsePerson ""
-- correct
t1 = parsePerson "firstName = John\nlastName = Connor\nage = 30"
-- correct | shiffled fields
t18 = parsePerson "lastName = Connor\nfirstName = John\nage = 30"
-- wrong Parse | no spaces around = in minor fields
t2 = parsePerson "firstName = John Smith\nlastName = Connor\nage = 30\nasde=as11"
 -- wrong Parse | no spaces around = on the left in minor fields
t5 = parsePerson "firstName = John Smith\nlastName = Connor\nage = 30\nasde= "
-- wrong Parse | no spaces around = in major fields
t3 = parsePerson "firstName=Barbarian\nlastName=Conn On\nage=30"
 -- wrong Incorrect | age is non-numeric
t4 = parsePerson "firstName = John\nlastName = Connor\nage = as30"
-- wrong Parse | no spaces around = in major fields, missing major field
t6 = parsePerson "firstName=Barbarian\nlastName=Conn Or"
-- wrong Parse | no spaces around = in major fields, typo in major field
t7 = parsePerson "firstNameee = John Smith\nlastName = Connor\nage = 30\nasde=as11"
-- correct | excessive fields
t8 = parsePerson "firstName = John\nlastName = Connor\nfoo = bar\nage = 30"
-- wrong Incomplete | missing major field
t9 = parsePerson "firstName = Barbarian\nlastName = Conn Or"
-- wrong Parse | empty major value
t10 = parsePerson "firstName = John\nlastName = Connor\nage = "
-- wrong Parse | no spaces around = on the right in major field
t11 = parsePerson "firstName = John\nlastName = Connor\nage ="
-- wrong Parse | empty key, missing major field
t12 = parsePerson "firstName = John\nlastName = Connor\n = 30"
-- correct | spaces in major field value
t13 = parsePerson "firstName = Barbarian\nlastName = Conn On\nage = 30"
-- correct | = in major field value
t14 = parsePerson "firstName = John\nlastName = Con=nor\nage = 30"
-- wrong Parse | no spaces around =, missing value in minor field
t15 = parsePerson "firstName=Barbarian\nlastName=Conn On\nage=30\ng dsfsd"
-- wrong Incomplete | major field key with whitespace, age is non-numeric
t17 = parsePerson " firstName = John\nlastName = Connor\nage = 2f8 "
