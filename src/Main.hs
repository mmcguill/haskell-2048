{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ViewPatterns #-}

-- TODO: Mobile friendly arrow keys - touch swipe
-- TODO: Combine newGame and moveX handlers usage of modify/swap?
-- TODO: defaultGame doesn't really make any sense, since we overwrite it with newGame?
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

import Paths_Haskell2048 -- For Cabal Data file location stuff
import GameModel
import Logic

----------------------------------------------------------------------------------

type GameEntry = (Text, TVar GameState)
data App = App (TVar Int) (TVar [GameEntry])

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

getNextId :: App -> STM Int
getNextId (App tnextId _) = do
  nextId <- readTVar tnextId
  writeTVar tnextId $ nextId + 1
  return nextId

getNextIdAsText :: App -> Handler Text
getNextIdAsText app = do
  ident <- liftIO $ atomically $ getNextId app
  let key = pack $ show ident
  return key 

addGame :: App -> Handler Text
addGame app@(App _ tstore) = do
    key <- getNextIdAsText app
    tgame <- liftIO $ atomically $ newTVar (defaultGame key)
    let entry = (key, tgame)
    liftIO . atomically $ do
        modifyTVar tstore $ \ ops -> entry : ops
    return $ fst entry

getById :: Text -> Handler (TVar GameState)
getById ident = do
    App _ tstore <- getYesod
    operations <- liftIO $ readTVarIO tstore
    case lookup ident operations of
      Nothing -> notFound
      Just game -> return game

loadTGame :: Handler (Text, TVar GameState)
loadTGame = do
  gameId <- lookupSession "gameId"
  case gameId of
    Just gid -> do
      tgame <- getById gid
      return (gid, tgame)
    Nothing  -> do
      app <- getYesod
      key <- addGame app
      setSession "gameId" key
      tgame <- getById key
      return (key, tgame)

getGameState :: Handler GameState
getGameState = do
  (_, tgame) <- loadTGame
  game <- liftIO $ readTVarIO $ tgame 
  return game
  
getGameStateR :: Handler Value
getGameStateR = do
  game <- getGameState
  returnJson (game :: GameState)

postMoveR :: Text -> Handler Value
postMoveR "Up"    = doMove Up
postMoveR "Down"  = doMove Down
postMoveR "Left"  = doMove Left
postMoveR "Right" = doMove Right
postMoveR _ = notFound 

doMove :: Direction -> Handler Value
doMove dir = do
  (gameId, tgame) <- loadTGame
  g <- liftIO newStdGen
  let left = move dir tgame g
  newGame <- liftIO left
  liftIO $ atomically $ swapTVar tgame newGame
  getGameStateR

move :: RandomGen g => Direction -> TVar GameState -> g -> IO GameState 
move d s g = do 
  current <- readTVarIO s
  let step = stepGame d (randomFloats g) $ current
  delta <- atomically $ swapTVar s step
  return step

postNewGameR :: Handler Value
postNewGameR = do
  (gameId, tgame) <- loadTGame
  g <- liftIO newStdGen
  let newGame = startNewGame gameId (randomFloats g)
  liftIO $ atomically $ swapTVar tgame newGame
  getGameStateR

randomFloats :: RandomGen g => g -> [Float]
randomFloats g = randoms (g) :: [Float]

----------------------------------------------------------------------------------

main :: IO ()
main = do 
  maybePort <- lookupEnv "PORT"
  let port = fromMaybe "3000" maybePort

  putStrLn $ "Haskell2048 started on port " ++ port

  tident <- atomically $ newTVar 0
  tgames <- atomically $ newTVar []
  warp (read(port)) (App tident tgames)
