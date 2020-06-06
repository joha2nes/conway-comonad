module Conway where

import Control.Comonad

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

trueCoords :: [(Int, Int)] -> Grid Bool
trueCoords liveCoords = fmap (\c -> any (==c) liveCoords) . coords $ (0, 0)