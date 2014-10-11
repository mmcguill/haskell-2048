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
      
  scotty 3000 $ do
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

    get "/" $ file "static/index.html"
    get "/favicon.ico" $ file "static/favicon.ico"
    get "/knockout.mapping.js" $ file "static/js/knockout.mapping.js"

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