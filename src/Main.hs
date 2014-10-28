{-# LANGUAGE OverloadedStrings #-}

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
import Control.Concurrent
import System.Environment
import Data.Maybe (fromMaybe)

randomFloats :: RandomGen g => g -> [Float]
randomFloats g = randoms (g) :: [Float]

move :: RandomGen g => GM.Direction -> MVar GameState -> g -> IO GameState 
move d s g = do
      gameState <- liftIO $ takeMVar s
      let delta = stepGame d (randomFloats g) gameState  
      liftIO $ putMVar s delta
      return delta

main :: IO ()
main = do 
  s <- newMVar defaultGame
  x <- lookupEnv "PORT"
  let port = fromMaybe "3000" x

  putStrLn "Starting Haskell2048!!"

  scotty (read port) $ do
    middleware logStdoutDev

    get "/gameState" $ do 
      x <- liftIO $ takeMVar s
      liftIO $ putMVar s x
      json $ (x :: GameState)

    get "/newGame" $ do
      g <- liftIO newStdGen
      let x = startNewGame $ randomFloats g
      ignored <- liftIO $ takeMVar s
      liftIO $ putMVar s x
  
      json $ (x :: GameState)

    get "/moveLeft" $ do
      g <- liftIO newStdGen
      delta <- liftIO $ move GM.Left s g
      json $ (delta :: GameState)

    get "/moveRight" $ do
      g <- liftIO newStdGen
      delta <- liftIO $ move GM.Right s g
      json $ (delta :: GameState)

    get "/moveUp" $ do
      g <- liftIO newStdGen
      delta <- liftIO $ move GM.Up s g
      json $ (delta :: GameState)

    get "/moveDown" $ do
      g <- liftIO newStdGen
      delta <- liftIO $ move GM.Down s g
      json $ (delta :: GameState)

    -- Static File Serving

    get "/" $ do 
      let foo = "static/index.html"  
      setHeader "Content-Type" "text/html; charset=utf-8"
      file foo
    
    get "/favicon.ico" $ do
      let foo = "static/favicon.ico"  
      setHeader "Content-Type" "image/x-icon"
      file foo
    
    -- Debug...

    get "/:word" $ do
      beam <- param "word"
      html $ mconcat ["<h1>Last Resort: [", beam , "]</h1>"]

    -- Scrap

    --get "/pwd" $ do
    --  g <- liftIO $ System.Directory.getCurrentDirectory
    --  html $ pack $ g

        --g <- liftIO newStdGen
        --let beam = L.pack $ show $ head $ randomFloats g
