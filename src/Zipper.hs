module Zipper where

import Control.Comonad
import Data.Maybe (fromMaybe)

data Zipper a = Zipper [a] a [a] deriving (Show, Eq)

instance Functor Zipper where
    fmap f (Zipper ls x rs) = Zipper (fmap f ls) (f x) (fmap f rs)

instance Comonad Zipper where
    extract (Zipper _ x _) = x
    duplicate z = Zipper (lefts z) z (rights z)

moveLeft :: Zipper a -> Maybe (Zipper a)
moveLeft (Zipper [] _ _) = Nothing
moveLeft (Zipper ls x rs) = Just $ Zipper (tail ls) (head ls) (x : rs)

moveRight :: Zipper a -> Maybe (Zipper a)
moveRight (Zipper _ _ []) = Nothing
moveRight (Zipper ls x rs) = Just $ Zipper (x : ls) (head rs) (tail rs)

lefts :: Zipper a -> [Zipper a]
lefts z = fromMaybe [] $ do
    l <- moveLeft z
    return (l : lefts l)

rights :: Zipper a -> [Zipper a]
rights z = fromMaybe [] $ do
    r <- moveRight z
    return (r : rights r) 

toList :: Zipper a -> [a]
toList (Zipper l x r) = reverse l ++ x : r