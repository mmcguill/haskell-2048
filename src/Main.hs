{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import System.Random
import GameModel
import GameModel as GM
import Logic
import Paths_Haskell2048
import System.Environment
import Yesod
import Control.Concurrent.STM
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text, pack)
import qualified Data.Text as Text

randomFloats :: RandomGen g => g -> [Float]
randomFloats g = randoms (g) :: [Float]

move :: RandomGen g => GM.Direction -> MVar GameState -> g -> IO GameState 
move d s g = do
      gameState <- liftIO $ takeMVar s
      let delta = stepGame d (randomFloats g) gameState  
      liftIO $ putMVar s delta
      return delta

-- TODO can we make the static routes just work? with Heroku?
mkYesod "App" [parseRoutes|
/               HomeR GET 
/favicon.ico    FaviconR GET
/stylesheet.css StylesheetR GET
/gameState      GameStateR GET
|]

instance Yesod App where
    -- Make the session timeout 1 minute so that it's easier to play with
    makeSessionBackend _ = do
    backend <- defaultClientSessionBackend 1 "session.aes"
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
    let entry = (key, (defaultGame key))
    liftIO . atomically $ do
        modifyTVar tstore $ \ ops -> entry : ops
    return $ fst entry

getById :: Text -> Handler GameState
getById ident = do
    App _ tstore <- getYesod
    operations <- liftIO $ readTVarIO tstore
    case lookup ident operations of
      Nothing -> notFound
      Just game -> return game

getGameStateR :: Handler Value
getGameStateR = do
  app <- getYesod
  gameId <- lookupSession "gameId"
  case gameId of
    Just gid -> do
      game <- (getById gid)
      returnJson (game :: GameState)
    Nothing  -> do
      key <- addGame app
      setSession "gameId" key
      game <- (getById key)
      returnJson (game :: GameState)

data App = App (TVar Int) (TVar [(Text, GameState)])

main :: IO ()
main = do 
  maybePort <- lookupEnv "PORT"
  let port = fromMaybe "3032" maybePort

  tident <- atomically $ newTVar 0
  tgames <- atomically $ newTVar []
  warp (read(port)) (App tident tgames)

  --  get "/newGame" $ do
  --    g <- liftIO newStdGen
  --    let x = startNewGame $ randomFloats g
  --    ignored <- liftIO $ takeMVar s
  --    liftIO $ putMVar s x
  
  --    json $ (x :: GameState)

  --  get "/moveLeft" $ do
  --    g <- liftIO newStdGen
  --    delta <- liftIO $ move GM.Left s g
  --    json $ (delta :: GameState)

  --  get "/moveRight" $ do
  --    g <- liftIO newStdGen
  --    delta <- liftIO $ move GM.Right s g
  --    json $ (delta :: GameState)

  --  get "/moveUp" $ do
  --    g <- liftIO newStdGen
  --    delta <- liftIO $ move GM.Up s g
  --    json $ (delta :: GameState)

  --  get "/moveDown" $ do
  --    g <- liftIO newStdGen
  --    delta <- liftIO $ move GM.Down s g
  --    json $ (delta :: GameState)
   