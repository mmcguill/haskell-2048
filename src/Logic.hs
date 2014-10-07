{-# LANGUAGE NoImplicitPrelude #-}
module Logic where

import Prelude (Eq, (==), Int, ($) , filter, (/=), take, map, sum, (.), repeat, (++), concat, length, fst, snd, (>), id, (+), Bool, all, and, Float, (<), (!!), floor, (*), fromIntegral)
import Data.Maybe
import GameModel

groupedByTwo :: Eq a => [a] -> [[a]] -- takes a list of values and 'slides' them to 
                                -- the left, joining in lists pairs of adjacent
                                -- identical values.
groupedByTwo l = case l of
    [x] -> [[x]]
    [x,y] -> if (x == y) then [[x,y]] else [[x],[y]]
    (x:y:xs) -> if (x == y) then ([x,y] : (groupedByTwo xs)) 
                    else ([x] : (groupedByTwo (y:xs)))
    otherwise -> []

slideRow :: [Tile] -> ([Tile], Int) -- slides list of tiles to left,
                                   -- merging tiles where necessary,
                                   -- and returning a full list of 
                                   -- four tiles, and the number of
                                   -- points gained
slideRow r = let grouped = groupedByTwo $ filter (\t -> t /= Empty) r
    in (
        take gridSize 
        $ (map ( intToTile . sum . (map tileToInt)) grouped) 
            ++ take gridSize (repeat Empty)
      , sum . (map tileToInt)
        $ concat 
        $ filter (\x -> length x > 1) grouped
    )

slideGrid :: Direction -> Grid -> (Grid, Int) -- slide all of the rows of a grid
                                             -- in a certain direction
slideGrid dir grid = if (dir == None) then (grid,0) else let 
                rotatedGrid = (case dir of
                    Down  -> rotateGrid 
                    Right -> rotateGrid . rotateGrid 
                    Up    -> rotateGrid . rotateGrid . rotateGrid
                    otherwise -> id)
                    $ grid

                rowsWithScores = map slideRow
                              $ (\(Grid h) -> h)
                              $ rotatedGrid

                slidRotatedGrid = Grid $ map fst rowsWithScores
                scoreGained = sum $ map snd rowsWithScores

                slidGrid = (case dir of
                    Up  -> rotateGrid 
                    Right -> rotateGrid . rotateGrid 
                    Down    -> rotateGrid . rotateGrid . rotateGrid
                    otherwise -> id)
                    $ slidRotatedGrid

            in (slidGrid, scoreGained) 

slideGameState :: Direction -> GameState -> GameState -- push the tiles in the grid 
                                            -- according to the direction in 
                                            -- the input
slideGameState direction gameState = 
   let newGridScore = slideGrid direction (grid gameState)
       in if (fst newGridScore == (grid gameState)) then gameState else
        GameState {  
            grid = fst newGridScore
          , score = (score gameState) + snd newGridScore
          , gameProgress = InProgress   -- the game is in progress
        } 

{------------------------------------------------------------------------------
                            Win and loss conditions
------------------------------------------------------------------------------}

gameLost :: Grid -> Bool -- check if none of the rows or columns of a grid
                        -- can be slid in any direction
gameLost g = let
        up = fst $ slideGrid Up g
        down = fst $ slideGrid Down g
        left = fst $ slideGrid Left g
        right = fst $ slideGrid Right g
    in and
        [ g /= emptyGrid
        , up == down
        , down == left
        , left == right
        , right == g
        ]

gameWon :: Grid -> Bool -- checks if a 2048 tile present in the grid
gameWon (Grid g) = 0 /= (length $ filter (\t -> t == Number 2048) $ concat g)

lose :: GameState -> GameState -- set a game to be at game over
lose gameState = gameState { gameProgress = GameOver }

win :: GameState -> GameState -- set a game to be won
win gameState = gameState { gameProgress = Won }

{------------------------------------------------------------------------------
                             Random tile placement
------------------------------------------------------------------------------}

tile2Probability :: Float -- the probability that a new tile is a 2.
                         -- equivalently, the probability that a new 
                         -- tile is a 4
tile2Probability = 0.9

newTile :: Float -> Tile -- based on a float that will be random, 
                        -- return a new tile
newTile x = if (x < tile2Probability) then (Number 2) else (Number 4)

emptyTiles :: Grid -> [(Int, Int)] -- a list of the coordinates of the empty 
                                  -- tiles in a grid
emptyTiles g = map (\(_,i,j) -> (i,j)) 
                   $ filter (\(t,_,_) -> t == Empty) 
                   $ tilesWithCoordinates g

newTileIndex :: Float -> Grid -> Maybe (Int, Int) -- based on a float that will
                                        -- be random, return Just the 
                                        -- coordinates of an empty tile in a 
                                        -- grid if one exists, or Nothing if 
                                        -- there are none
newTileIndex x g = let emptyTileIndices = emptyTiles g
    in case emptyTileIndices of 
        [] -> Nothing
        otherwise -> Just 
                    $ emptyTileIndices !! 
                     (floor $ (fromIntegral $ length emptyTileIndices) * x) -- TODO: are we sure this will select the last tile ever?

placeRandomTile :: Float -> Float -> GameState -> GameState -- place a random 
                                                -- tile into a random position
                                                -- in the grid
placeRandomTile float1 float2 gameState = 
    let tileIndex = newTileIndex float1 (grid gameState)
    in if (tileIndex == Nothing) then gameState else
         gameState {
            grid = setTile (case tileIndex of 
            	Just (i,j)  -> (i, j) 
            	Nothing -> (0,0)) 
            	(grid gameState) 
            	$ newTile float2
        }

newGame :: [Float] -> GameState -- generate a new game with two random 
                             -- starting tiles
newGame input = 
    placeRandomTile (input !! 0) (input !! 1)
 	$ placeRandomTile (input !! 2) (input !! 3)
	$ defaultGame

{------------------------------------------------------------------------------
                          Game stepping function
------------------------------------------------------------------------------}

startNewGame :: [Float] -> GameState
startNewGame r = newGame r

stepGame :: Direction -> [Float] -> GameState -> GameState -- game stepper that is called every
                                           -- time the input changes
stepGame direction r gameState =
	if(gameProgress gameState) /= InProgress then gameState else
		if (gameWon (grid gameState)) then win gameState else 
        	if(gameLost (grid gameState)) then lose gameState else 
        		if(direction /= None) then let pushedState = slideGameState direction gameState -- then try to push 
                in if (pushedState == gameState) then gameState 
                    else placeRandomTile 
                        (r !! 0) -- and if it has, place a new
                        (r !! 1) -- random tile
                        pushedState
        		else gameState