{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

-- TODO: Combine newGame and moveX handlers usage of modify/swap?
-- TODO: Parameterise moveX instead of having 4 different methods
-- TODO: defaultGame doesn't really make any sense, since we overwrite it with newGame?
-- TODO: Move/New really should be POST?
-- TODO: AI competitor
-- TODO: can we make the static routes just work? with Heroku?
-- TODO: Mobile friendly arrow keys - touch swipe
-- TODO: Performance test with some bots - lets see how many clients we can have!

import System.Random
import GameModel
import GameModel as GM
import Logic
import Paths_Haskell2048
import System.Environment
import Yesod
import Control.Concurrent
import Control.Concurrent.STM
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text, pack)
import qualified Data.Text as Text

data App = App (TVar Int) (TVar [(Text, TVar GameState)])

mkYesod "App" [parseRoutes|
/               HomeR GET 
/moveLeft       MoveLeftR GET 
/moveRight      MoveRightR GET 
/moveUp         MoveUpR GET 
/moveDown       MoveDownR GET 
/newGame        NewGameR GET 
/favicon.ico    FaviconR GET 
/stylesheet.css StylesheetR GET 
/gameState      GameStateR GET 
|] 

instance Yesod App where
  -- Make the session timeout 1 minute so that it's easier to play with or a day...
  makeSessionBackend _ = do
    backend <- defaultClientSessionBackend 1440 "session.aes"
    return $ Just backend

getHomeR :: Handler Html
getHomeR = do
           foo <- liftIO $ getDataFileName "src/static/index.html" 
           dummy <- liftIO $ putStrLn foo
           sendFile "text/html; charset=utf-8" foo

getFaviconR :: Handler Html
getFaviconR = do
  foo <- liftIO $ getDataFileName "src/static/favicon.ico" 
  sendFile "image/x-icon" foo

getStylesheetR :: Handler Html
getStylesheetR = do
  foo <- liftIO $ getDataFileName "src/static/stylesheet.css" 
  sendFile "text/css" foo

---------------------------------------------------------------
--  defaultLayout [whamlet||]
--getHomeR  = defaultLayout [whamlet|<a href=@{Page1R}>Go to page 1!|] 

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
  app <- getYesod
  gameId <- lookupSession "gameId"
  case gameId of
    Just gid -> do
      tgame <- (getById gid)
      return (gid, tgame)
    Nothing  -> do
      key <- addGame app
      setSession "gameId" key
      tgame <- (getById key)
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

getMoveLeftR :: Handler Value
getMoveLeftR = doMoveR GM.Left

getMoveRightR :: Handler Value
getMoveRightR = doMoveR GM.Right

getMoveUpR :: Handler Value
getMoveUpR = doMoveR GM.Up

getMoveDownR :: Handler Value
getMoveDownR = doMoveR GM.Down

doMoveR :: GM.Direction -> Handler Value
doMoveR dir = do
  (gameId, tgame) <- loadTGame
  g <- liftIO newStdGen
  let left = move dir tgame g
  newGame <- liftIO left
  liftIO $ atomically $ swapTVar tgame newGame
  getGameStateR

move :: RandomGen g => GM.Direction -> TVar GameState -> g -> IO GameState 
move d s g = do 
  current <- readTVarIO s
  let step = stepGame d (randomFloats g) $ current
  delta <- atomically $ swapTVar s step
  return step

getNewGameR :: Handler Value
getNewGameR = do
  (gameId, tgame) <- loadTGame
  g <- liftIO newStdGen
  let newGame = startNewGame gameId (randomFloats g)
  liftIO $ atomically $ swapTVar tgame newGame
  getGameStateR

randomFloats :: RandomGen g => g -> [Float]
randomFloats g = randoms (g) :: [Float]

main :: IO ()
main = do 
  maybePort <- lookupEnv "PORT"
  let port = fromMaybe "3000" maybePort

  putStrn $ "Haskell2048 started on port " ++ port

  tident <- atomically $ newTVar 0
  tgames <- atomically $ newTVar []
  warp (read(port)) (App tident tgames)

   