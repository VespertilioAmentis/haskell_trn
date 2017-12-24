module MapLikeStuff where

import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
    empty = ListMap []
    lookup str lst | length droppedList == 0 = Nothing
                   | otherwise = Just $ snd $ head $ droppedList
        where
            droppedList =  dropWhile ((/=str) . fst) $ getListMap lst
    delete x lst = ListMap $ fst spanned ++ safetail (snd spanned)
        where
            spanned = span ( (/=x) . fst ) $ getListMap lst
            safetail [] = []
            safetail a = tail a
    insert x y = ListMap . ((x,y):) . getListMap . delete x

fn k = if k < 5 then Nothing else Just "qwerqwre"
arrmap = ArrowMap fn

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    empty = ArrowMap $ \_ -> Nothing
    lookup k m = getArrowMap m $ k
    insert k v a = ArrowMap $ \x -> if x == k then Just v else (getArrowMap a) x
    delete ky whr = undefined

-----------------------------------------------

infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
expand (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = expand e1 :*: expand e2
expand e = e

    

