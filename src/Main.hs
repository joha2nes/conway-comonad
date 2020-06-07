module Main where

import Control.Applicative (liftA2)
import Control.Comonad
import Control.Monad (when)
import qualified Data.Stream as S
import Data.Char (toLower)
import Data.Bool (bool)
import Data.List (intersperse)
import System.IO (hFlush, stdout)
import System.Process (system)

import Conway
import Grid

renderString :: (a -> Char) -> (Int, Int) -> Grid a -> String
renderString toChar (width, height) = unlines . surround (take (2 * width) (' ' : repeat '-')) . S.take height . fmap (surround '|' . intersperse ' ' . S.take width . fmap (toChar . extract) . rights) . ups
    where
        surround :: a -> [a] -> [a]
        surround x xs = x : xs ++ [x]

main :: IO ()
main =
    let
        continue = do
            putStr "Continue (y/n)? [default: y] "
            hFlush stdout
            (/=) 'n' . toLower <$> getChar

        loop g = do
            system "cls"
            putStrLn $ renderString (bool ' ' '@') (40, 20) g
            b <- continue
            when b $ loop $ conway g

        start = move (3, -2) gosperGliderGun
           <||> move (-5, -15) beacon
           <||> move (-35, -3) blinker
    in
        loop start

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
