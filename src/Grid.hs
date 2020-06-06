module Grid where

import Control.Comonad
import Control.Applicative (liftA2)

import qualified StreamZipper as SZ
import qualified Data.Stream as S

newtype Grid a = Grid { unGrid :: SZ.StreamZipper (SZ.StreamZipper a) } deriving (Show, Eq)
-- y
-- |  o o  o  o o
-- |  o o  o  o o
-- | [x x] X [x x]
-- |  o o  o  o o
-- |  o o  o  o o
-- |______________ x

instance Functor Grid where
    fmap f = Grid . fmap (fmap f) . unGrid

instance Applicative Grid where
    pure x = Grid (pure . pure $ x)
    (Grid f) <*> (Grid x) = Grid $ liftA2 (<*>) f x

instance Comonad Grid where
    extract = extract . extract . unGrid
    duplicate = fmap Grid . Grid . layer . layer . unGrid
        where
            layer :: SZ.StreamZipper (SZ.StreamZipper a) -> SZ.StreamZipper (SZ.StreamZipper (SZ.StreamZipper a))
            layer sz =
                let lefts = fmap snd . S.zip (SZ.left sz) . S.tail . S.iterate (fmap SZ.moveLeft) $ sz
                    rights = fmap snd . S.zip (SZ.right sz) . S.tail . S.iterate (fmap SZ.moveRight) $ sz
                in SZ.StreamZipper lefts sz rights

moveLeft :: Grid a -> Grid a
moveLeft = Grid . fmap SZ.moveLeft . unGrid

moveRight :: Grid a -> Grid a
moveRight = Grid . fmap SZ.moveRight . unGrid

moveDown :: Grid a -> Grid a
moveDown = Grid . SZ.moveLeft . unGrid

moveUp :: Grid a -> Grid a
moveUp = Grid . SZ.moveRight . unGrid

move :: (Int, Int) -> Grid a -> Grid a
move (x, y) = Grid . fmap (SZ.move x) . SZ.move y . unGrid

lefts :: Grid a -> S.Stream (Grid a)
lefts = S.iterate moveLeft

rights :: Grid a -> S.Stream (Grid a)
rights = S.iterate moveRight

ups :: Grid a -> S.Stream (Grid a)
ups = S.iterate moveUp

downs :: Grid a -> S.Stream (Grid a)
downs = S.iterate moveDown

neighbors :: Grid a -> [a]
neighbors g = extract . ($ g) <$>
    [ moveLeft . moveDown
    , moveLeft
    , moveUp . moveLeft
    , moveUp
    , moveUp . moveRight
    , moveRight
    , moveDown . moveRight
    , moveDown
    ]

horizontal :: Grid a -> SZ.StreamZipper a
horizontal = extract . unGrid

vertical :: Grid a -> SZ.StreamZipper a
vertical = fmap extract . unGrid

coords :: (Int, Int) -> Grid (Int, Int)
coords (x0, y0) = Grid $ SZ.StreamZipper down focus up
    where down = row x0 <$> S.iterate pred (y0-1)
          focus = row x0 y0
          up = row x0 <$> S.iterate succ (y0+1)
          row x y = flip (,) y <$> SZ.range x
