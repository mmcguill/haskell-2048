module Paths_Haskell2048 where

import System.FilePath

-- This module will be replaced by cabal automatically and will be return the correct paths. THis is for development only

getDataFileName :: FilePath -> IO FilePath
getDataFileName a = do 
	putStrLn "Yoyo!!"
	return $ a
	--return $ "../" ++ a -- Paths will be relative to root dir though we will be in src dir for development...