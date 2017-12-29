module Monades where

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
    return = returnLog
    (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList initval lst = foldl (>>=) (return initval) lst

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
