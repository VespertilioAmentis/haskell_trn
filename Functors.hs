module Functors where

data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
    fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)
    deriving Show

instance Functor GeomPrimitive where
    fmap f (Point x) = Point $ fmap f x
    fmap f (LineSegment x y) = LineSegment (fmap f x) $ fmap f y

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

instance Functor Tree where
    fmap f (Leaf x) = Leaf $ fmap f x
    fmap f (Branch x y z) = Branch (fmap f x) (fmap f y) (fmap f z)
