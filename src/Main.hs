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
renderString toChar (width, height) = unlines . surround (take (2 * width) $ repeat '-') . S.take height . fmap (surround '|' . intersperse ' ' . S.take width . fmap (toChar . extract) . rights) . ups
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
            putStrLn $ renderString (bool ' ' '@') (20, 20) g
            b <- continue
            when b $ loop $ conway g

        start = move (-10, -7) blinker
           <||> conway (move (-3, -3) blinker)
           <||> move (-5, -10) beacon
           <||> move (-11, -11) glider
           <||> move (-16, -14) blinker
           <||> move (-2, -14) toad
    in
        loop start

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
