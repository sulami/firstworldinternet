module Lib where

import           Data.Char (isNumber)

-- | Divide the blob of data into chunks that contain a single traceroute each.
chunks :: String -> [String]
chunks []   = []
chunks blob = let l = lines blob
                  (h,r) = splitAt 2 l
                  (d,n) = span isData r
              in (unlines h ++ unlines d) : chunks (unlines n)

-- | Decide if a line is part of the header or data.
isData :: String -> Bool
isData = any isNumber . take 2

