{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ViewPatterns #-}

-- TODO: Mobile friendly arrow keys - touch swipe
-- TODO: AI competitor
-- TODO: Performance test with some bots

module Main where

import Prelude hiding (Up, Down, Left, Right)   -- GameModel.Direction uses these
import System.Random
import System.Environment
import Yesod
import Control.Concurrent.STM
import Data.Maybe
import Data.Text
import qualified Data.Text as Text
import Data.Map
import qualified Data.Map as Map

import Paths_Haskell2048 -- For Cabal Data file location stuff
import GameModel
import Logic

----------------------------------------------------------------------------------

data App = App (TVar Int) (TVar (Map Text GameState))

mkYesod "App" [parseRoutes|
/                 HomeR GET 
/move/#Text       MoveR POST 
/newGame          NewGameR POST
/favicon.ico      FaviconR GET 
/stylesheet.css   StylesheetR GET 
/gameState        GameStateR GET 
|] 

instance Yesod App where
  makeSessionBackend _ = do
    backend <- defaultClientSessionBackend 1440 "2048-sessions.aes"
    return $ Just backend

------------------------------------------------------------------------------------

getHomeR :: Handler Html
getHomeR = do
  foo <- liftIO $ getDataFileName "src/static/index.html" 
  sendFile "text/html; charset=utf-8" foo

getFaviconR :: Handler Html
getFaviconR = do
  foo <- liftIO $ getDataFileName "src/static/favicon.ico" 
  sendFile "image/x-icon" foo

getStylesheetR :: Handler Html
getStylesheetR = do
  foo <- liftIO $ getDataFileName "src/static/stylesheet.css" 
  sendFile "text/css" foo

------------------------------------------------------------------------------------

createNewGame :: Handler (Text, GameState)
createNewGame = do
  app@(App tIdCounter tGamesMap) <- getYesod
  gameId <- liftIO $ getNextIdAsText tIdCounter
  g <- liftIO newStdGen
  let newGame = startNewGame gameId (randomFloats g)
  liftIO $ setGameStateForGameId tGamesMap gameId newGame
  setSession "gameId" gameId
  return (gameId, newGame)

loadGame :: Handler (Text, GameState)
loadGame = do
  app@(App tIdCounter tGamesMap) <- getYesod
  gameId <- lookupSession "gameId"
  case gameId of
    Just gid -> do
      existing <- liftIO $ getById tGamesMap gid
      case existing of
        Just game -> return (gid, game)
        Nothing -> createNewGame
    Nothing  -> createNewGame
  
getGameStateR :: Handler Value
getGameStateR = do
  (_, game) <- loadGame
  returnJson game

postMoveR :: Text -> Handler Value
postMoveR "Up"    = doMove Up
postMoveR "Down"  = doMove Down
postMoveR "Left"  = doMove Left
postMoveR "Right" = doMove Right
postMoveR _ = notFound 

doMove :: Direction -> Handler Value
doMove direction = do
  app@(App tIdCounter tGamesMap) <- getYesod
  (gameId, game) <- loadGame
  rndGen <- liftIO newStdGen
  let delta = stepGame direction (randomFloats rndGen) $ game
  liftIO $ setGameStateForGameId tGamesMap gameId delta
  returnJson delta

postNewGameR :: Handler Value
postNewGameR = do
  (gameId, gameState) <- createNewGame
  returnJson gameState

----------------------------------------------------------------------------------

getNextId :: TVar Int -> IO Int
getNextId tIdCounter = atomically $ do
  modifyTVar tIdCounter (+1)
  readTVar tIdCounter

getNextIdAsText :: TVar Int -> IO Text
getNextIdAsText tIdCounter = do
  gameId <- getNextId tIdCounter
  return $ pack $ show gameId

getById :: TVar (Map Text GameState) -> Text -> IO (Maybe GameState)
getById tGamesMap gameId = do
    games <- readTVarIO tGamesMap
    return $ Map.lookup gameId games

setGameStateForGameId :: TVar (Map Text GameState) -> Text -> GameState -> IO (Map Text GameState)
setGameStateForGameId tGamesMap gameId gameState = atomically $ do
  games <- readTVar tGamesMap
  let newMap = Map.insert gameId gameState games
  swapTVar tGamesMap newMap

randomFloats :: RandomGen g => g -> [Float]
randomFloats g = randoms (g) :: [Float]

----------------------------------------------------------------------------------

main :: IO ()
main = do 
  maybePort <- lookupEnv "PORT"
  let port = fromMaybe "3000" maybePort

  putStrLn $ "Haskell2048 started on port " ++ port

  tident <- atomically $ newTVar 0
  tgames <- atomically $ newTVar Map.empty
  warp (read(port)) (App tident tgames)

----------------------------------------------------------------------------------
