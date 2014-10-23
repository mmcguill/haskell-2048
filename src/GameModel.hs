
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module GameModel where

import Data.List
import GHC.Generics
import Data.Aeson hiding (Number)
import Data.Text (Text, pack)
import qualified Data.Text as Text

data Tile = Num Int | Empty deriving (Eq, Show, Generic) -- a tile can either contain an int, or be empty

instance ToJSON Tile where
 toJSON x = object [ "tile" .= (show $ tileToInt x)]
instance FromJSON Tile

data Grid = Grid [[Tile]]  deriving (Eq, Show, Generic) -- a grid is a list of lists of tiles
instance FromJSON Grid
instance ToJSON Grid 

data Progress = InProgress | GameOver | Won deriving (Eq, Show, Generic) -- a game can be in progress, 
instance FromJSON Progress
instance ToJSON Progress                                          -- at game over, or won

data GameState = GameState { -- defines the various properties of a game state:
    gameid :: Text                -- the app wide game id
  , grid :: Grid              -- the grid of tiles
  , score :: Int              -- the score
  , gameProgress :: Progress  -- the progress of the game (in progress, 
                            -- game over etc.)
} deriving (Eq, Show, Generic)

instance FromJSON GameState
instance ToJSON GameState

data Direction = Up | Down | Left | Right | None deriving (Eq, Show, Generic) -- the direction to shift 
                                                 -- the grid

gridSize :: Int -- the length of the sides of the grid
gridSize = 4

{------------------------------------------------------------------------------
                             Grid manipulation
------------------------------------------------------------------------------}

readTile :: (Int, Int) -> Grid -> Tile -- the tile at (i,j) in a grid
readTile (i, j) (Grid g) = (g !! j) !! i

setTile :: (Int, Int) -> Grid -> Tile -> Grid -- change the tile at (i,j) in 
                                             -- a grid
setTile (i, j) (Grid g) t = let 
        r = g !! j -- jth row
        nr = (take i r) ++ [t] ++ (drop (i+1) r) -- ith element changed in
                                                 -- jth row
    in Grid $ (take j g) ++ [nr] ++ (drop (j+1) g) -- new grid with modified 
                                                    -- jth row


tileToInt :: Tile -> Int -- convert a tile to the int it represents
tileToInt t = case t of
    Num n -> n
    otherwise -> 0

intToTile :: Int -> Tile -- convert an int to a tile representing it
intToTile n = case n of
    0 -> Empty
    otherwise -> Num n

tilesWithCoordinates :: Grid -> [(Tile,Int,Int)] -- a list of the tiles in a grid 
                                                -- with their coordinates
tilesWithCoordinates (Grid g) = concat
                   $ zipWith (\j r -> map (\(t,i) -> (t,i,j)) r) 
                        [0..(gridSize-1)] 
                   $ map (\r -> zip r [0..(gridSize-1)]) 
                   $ g

rotateGrid :: Grid -> Grid -- rotate a grid clockwise by 90 degrees 
rotateGrid (Grid g) = Grid $ map reverse $ transpose g

{------------------------------------------------------------------------------
                             Initial gamestate
------------------------------------------------------------------------------}

emptyGrid :: Grid -- a grid of empty tiles
emptyGrid = Grid 
	$ take gridSize
	$ repeat 
	$ take gridSize 
	$ repeat Empty

defaultGame :: Text -> GameState -- the default starting game state:
defaultGame gid = GameState { 
    gameid = gid
  , grid = emptyGrid            -- an empty grid
  , score = 0                   -- initial score is zero
  , gameProgress = InProgress   -- the game is in progress
    }

