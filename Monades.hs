module Monades where

data Log a = Log [String] a
    deriving Show

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

data SomeType a = SomeType a
    deriving Show

instance Applicative SomeType where
    pure = SomeType
    (<*>) = undefined

instance Monad SomeType where
    (>>=) (SomeType x) fn = fn x

instance Functor SomeType where
    fmap f = ( >>= return . f )
