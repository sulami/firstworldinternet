module Main where

import           System.Environment (getArgs)

import           Lib

main :: IO ()
main = do
  filename <- fmap head getArgs
  filedata <- readFile filename
  mapM_ (print . chunkToDataPoint) $ chunks filedata

