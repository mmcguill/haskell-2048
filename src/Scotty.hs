{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import System.Random
import Data.Monoid (mconcat)
import Control.Monad.Trans
import System.Directory
import GameModel
import Logic
import Data.Text.Lazy

randomFloats :: RandomGen g => g -> [Float]
randomFloats g = randoms (g) :: [Float]

main = scotty 3000 $ do
  middleware logStdoutDev

  get "/cwd" $ do
    g <- liftIO System.Directory.getCurrentDirectory
    html $ pack $ g

  get "/gameState" $ do 
    g <- liftIO newStdGen
    let r = randomFloats g
    let x = startNewGame r
    json $ (x :: GameState)

  get "/" $ file "static/index.html"
  
  get "/favicon.ico" $ file "static/favicon.ico"

  get "/knockout.mapping.js" $ file "static/js/knockout.mapping.js"

  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Last Resort: [", beam , "]</h1>"]

      --g <- liftIO newStdGen
      --let beam = L.pack $ show $ head $ randomFloats g
