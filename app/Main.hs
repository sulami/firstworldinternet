module Main where

import           System.Environment (getArgs)

import           Lib

main :: IO ()
main = do
  filename <- fmap head getArgs
  filedata <- lines <$> readFile filename
  putStrLn . unlines $ take 15 filedata

