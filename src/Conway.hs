module Conway where

import Control.Comonad
import Control.Applicative (liftA2)

import Grid

conway :: Grid Bool -> Grid Bool
conway = extend conwayRules

conwayRules :: Grid Bool -> Bool
conwayRules grid =
    let isLive = extract grid
        liveNeighbors = length . filter id . neighbors $ grid
    in if isLive
        then 2 <= liveNeighbors && liveNeighbors <= 3
        else liveNeighbors == 3

blinker :: Grid Bool
blinker = trueCoords [(0,0), (1,0), (2,0)]

toad :: Grid Bool
toad = (||) <$> move (1, 0) blinker <*> move (0, -1) blinker

boat :: Grid Bool
boat = trueCoords [(1,0), (0,1), (2,1), (1,2), (2,2), (0,0)]

beacon :: Grid Bool
beacon = trueCoords [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]

glider :: Grid Bool
glider = trueCoords [(0,0), (1,1), (2,1), (0,2), (1,2)]

block :: Grid Bool
block = trueCoords [(0,0), (1, 0), (0, 1), (1, 1)]

gosperGliderGun :: Grid Bool
gosperGliderGun = trueCoords $ pairs $
    [ 24,0
    , 22,1, 24,1
    , 12,2, 13,2, 20,2, 21,2, 34,2, 35,2
    , 11,3, 15,3, 20,3, 21,3, 34,3, 35,3
    , 0,4,  1,4,  10,4, 16,4, 20,4, 21,4
    , 0,5,  1,5,  10,5, 14,5, 16,5, 17,5, 22,5, 24,5
    , 10,6, 16,6, 24,6
    , 11,7, 15,7
    , 12,8, 13,8
    ]

trueCoords :: [(Int, Int)] -> Grid Bool
trueCoords liveCoords = fmap (\c -> any (==c) liveCoords) . coords $ (0, 0)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (_ : []) = []
pairs (x : y : xs) = (x, y) : pairs xs
