module Monades where

data Log a = Log [String] a
    deriving Show

decompose (Log x y) = (x,y)

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger fn msg = Log ([msg]) . fn

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers parm fn1 fn2 = undefined

data SomeType a = SomeType a
    deriving Show

instance Applicative SomeType where
    pure = SomeType
    (<*>) = undefined

instance Monad SomeType where
    (>>=) (SomeType x) fn = fn x

instance Functor SomeType where
    fmap f = ( >>= return . f )
