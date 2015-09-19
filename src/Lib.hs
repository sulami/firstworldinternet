module Lib where

import           Data.Char (isNumber)

import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, parseTimeM)

-- | This structure associates a number of fails with a time, which makes
-- analysis easier later on.
data DataPoint = DP {
  time :: UTCTime,
  fails :: Int
}

-- | Parse the inital time line of each chunk to an actual time. Because time
-- does not know about CEST, use sed to replace all occurences of it with "B",
-- which is the military designation.
readTime :: String -> Maybe UTCTime
readTime = parseTimeM True defaultTimeLocale "%e. %b %H:%M:%S %Z %Y" . prep
  where
    -- We need this to drop the German weekday prefix.
    prep :: String -> String
    prep = unwords . drop 1 . words

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

