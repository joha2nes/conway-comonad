module StreamZipper where

import Control.Comonad
import Data.Stream as S

import Zipper (Zipper(..))

data StreamZipper a = StreamZipper (Stream a) a (Stream a) deriving (Show, Eq)

instance Functor StreamZipper where
    fmap f (StreamZipper ls x rs) = StreamZipper (fmap f ls) (f x) (fmap f rs)

instance Applicative StreamZipper where
    pure x = StreamZipper (pure x) x (pure x)
    (StreamZipper lfs f rfs) <*> (StreamZipper ls x rs) = StreamZipper (lfs <*> ls) (f x) (rfs <*> rs)

instance Comonad StreamZipper where
    extract (StreamZipper _ x _) = x
    duplicate z = StreamZipper (lefts z) z (rights z)

moveLeft :: StreamZipper a -> StreamZipper a
moveLeft (StreamZipper ls x rs) = StreamZipper (S.tail ls) (S.head ls) (x <:> rs)

moveRight :: StreamZipper a -> StreamZipper a
moveRight (StreamZipper ls x rs) = StreamZipper (x <:> ls) (S.head rs) (S.tail rs)

move :: Int -> StreamZipper a -> StreamZipper a
move i sz
    | i < 0 = S.iterate moveLeft sz S.!! (-i)
    | i > 0 = S.iterate moveRight sz S.!! i
    | i == 0 = sz

left :: StreamZipper a -> Stream a
left (StreamZipper l _ _) = l

right :: StreamZipper a -> Stream a
right (StreamZipper _ _ r) = r

lefts :: StreamZipper a -> Stream (StreamZipper a)
lefts sz = let l = moveLeft sz
           in l <:> lefts l

rights :: StreamZipper a -> Stream (StreamZipper a)
rights sz = let r = moveRight sz
            in r <:> rights r

take :: (Int, Int) -> StreamZipper a -> Zipper a
take (l, r) (StreamZipper ls x rs) = Zipper (S.take l ls) x (S.take r rs)

range :: Int -> StreamZipper Int
range i = StreamZipper (S.iterate pred (i-1)) i (S.iterate succ (i+1))
