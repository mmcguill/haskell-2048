{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ViewPatterns #-}

-- TODO: Mobile friendly arrow keys - touch swipe
-- TODO: AI competitor
-- TODO: can we make the static routes just work? with Heroku?
-- TODO: Performance test with some bots - lets see how many clients we can have!

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
/jgestures.min.js JGesturesMinJsR GET 
|] 


instance Yesod App where
  makeSessionBackend _ = do
    backend <- defaultClientSessionBackend 1440 "session.aes"
    return $ Just backend

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

getJGesturesMinJsR :: Handler Html
getJGesturesMinJsR = do
  foo <- liftIO $ getDataFileName "src/static/jgestures.min.js" 
  sendFile "application/javascript" foo

---------------------------------------------------------------

getNextId :: App -> IO Int
getNextId (App tnextId _) = atomically $ do
  modifyTVar tnextId (+1)
  readTVar tnextId

getNextIdAsText :: App -> Handler Text
getNextIdAsText app = do
  ident <- liftIO $ getNextId app
  return $ pack $ show ident

getById :: Text -> Handler GameState
getById ident = do
    App _ tstore <- getYesod
    games <- liftIO $ readTVarIO tstore
    case Map.lookup ident games of
      Nothing -> notFound
      Just game -> return game

loadGame :: Handler (Text, GameState)
loadGame = do
  gameId <- lookupSession "gameId"
  case gameId of
    Just gid -> do
      game <- getById gid
      return (gid, game)
    Nothing  -> do
      (key, game) <- createNewGame
      setSession "gameId" key
      return (key, game)
  
getGameStateR :: Handler Value
getGameStateR = do
  (_, game) <- loadGame
  returnJson (game :: GameState)

postMoveR :: Text -> Handler Value
postMoveR "Up"    = doMove Up
postMoveR "Down"  = doMove Down
postMoveR "Left"  = doMove Left
postMoveR "Right" = doMove Right
postMoveR _ = notFound 

doMove :: Direction -> Handler Value
doMove direction = do
  (gameId, game) <- loadGame
  rndGen <- liftIO newStdGen
  let delta = stepGame direction (randomFloats rndGen) $ game
  setGameStateForGameId gameId delta
  returnJson delta

setGameStateForGameId :: Text -> GameState -> Handler ()
setGameStateForGameId gameId gameState = do
  App _ tgames <- getYesod
  liftIO $ atomically $ do
    games <- readTVar tgames
    let newMap = Map.insert gameId gameState games
    swapTVar tgames newMap
  return ()

createNewGame :: Handler (Text, GameState)
createNewGame = do
  app <- getYesod
  gameId <- getNextIdAsText app
  g <- liftIO newStdGen
  let newGame = startNewGame gameId (randomFloats g)
  setGameStateForGameId gameId newGame
  return (gameId, newGame)

postNewGameR :: Handler Value
postNewGameR = do
  (gameId, gameState) <- createNewGame
  returnJson gameState

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
