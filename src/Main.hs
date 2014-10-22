{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Web.Scotty
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import System.Random
import Data.Monoid (mconcat)
import Control.Monad.Trans
import System.Directory
import GameModel
import GameModel as GM
import Logic
import Data.Text.Lazy
import Paths_Haskell2048
import System.Environment
import Data.Maybe (fromMaybe)
import Yesod
import Control.Concurrent
import Control.Concurrent.STM
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T

randomFloats :: RandomGen g => g -> [Float]
randomFloats g = randoms (g) :: [Float]

move :: RandomGen g => GM.Direction -> MVar GameState -> g -> IO GameState 
move d s g = do
      gameState <- liftIO $ takeMVar s
      let delta = stepGame d (randomFloats g) gameState  
      liftIO $ putMVar s delta
      return delta

data App = App (TVar (Map T.Text GameState))

mkYesod "App" [parseRoutes|
/             HomeR GET 
/favicon.ico  FaviconR GET
/gameState    GameStateR GET
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


getGameForUser = do
  gameId <- lookupSession "gameId"
  case gameId of
    Just gid -> return gid
    Nothing  -> do
      App tstate <- getYesod
      let x = "the_key"
      liftIO . atomically $ do
        modifyTVar tstate $ \ map -> Map.insert x defaultGame map
      setSession "gameId" x
      return x

getGameStateR :: Handler Value
getGameStateR = do
  gameKey <- getGameForUser
  App tstate <- getYesod
  gamesMap <- liftIO $ atomically $ readTVar $ tstate
  let game = (Map.lookup gameKey gamesMap)
  returnJson (fromJust(game) :: GameState)


--  defaultLayout [whamlet||]

--getHomeR  = defaultLayout [whamlet|<a href=@{Page1R}>Go to page 1!|] 

main :: IO ()
main = do 
  games <- atomically $ newTVar Map.empty
  x <- lookupEnv "PORT"
  let port = fromMaybe "3020" x

  putStrLn ("Starting Haskell2048 on Port: " ++ port)
  warp (read(port)) (App games)

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
    
  --  -- Debug...

  --  get "/:word" $ do
  --    beam <- param "word"
  --    html $ mconcat ["<h1>Last Resort: [", beam , "]</h1>"]

    -- Scrap

    --get "/pwd" $ do
    --  g <- liftIO $ System.Directory.getCurrentDirectory
    --  html $ pack $ g

        --g <- liftIO newStdGen
        --let beam = L.pack $ show $ head $ randomFloats g
