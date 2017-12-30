module Monades where

import Data.Char
import Control.Monad

data Log a = Log [String] a
    deriving (Eq,Show)

returnLog :: a -> Log a
returnLog = Log []

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger fn msg = Log ([msg]) . fn

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers parm fn1 fn2 =
    case fn1 parm of
        Log x y ->
                case fn2 y of
                    Log z w -> Log (x ++ z) w

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log msgs val) fn = 
    case fn val of Log msg val -> Log (msgs ++ msg) val

instance Applicative Log where
    pure = returnLog
    (<*>) = undefined

instance Functor Log where
    fmap = undefined

instance Monad Log where
    return = Log ["Log start"]
    (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList initval = foldl (>>=) (return initval)

exLgTestFn = execLoggersList (3 :: Int) [toLogger (+1) "added one",
                                         toLogger (*2) "multiplied by 2",
                                         toLogger (*100) "multiplied by 100"]

sampleVal = Log ["added one","multiplied by 2","multiplied by 100"] 800

exLgTest = exLgTestFn == sampleVal

-------------------------------

data SomeType a = SomeType a
    deriving Show

instance Applicative SomeType where
    pure = SomeType
    (<*>) = undefined

instance Monad SomeType where
    (>>=) (SomeType x) fn = fn x

instance Functor SomeType where
    fmap f = ( >>= return . f )

-------------------------------

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace     
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken x =
    case x of
        "(" -> Just LeftBrace
        ")" -> Just RightBrace
        "+" -> Just Plus
        "-" -> Just Minus
        x -> if all isDigit x then Just $ Number $ (read x :: Int) else Nothing

tokenize :: String -> Maybe [Token]
tokenize = sequence . map (asToken) . words

-------------------------------

data Board = Board

nextPositions :: Board -> [Board]
nextPositions = undefined

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred = do undefined

