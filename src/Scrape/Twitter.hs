{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrape.Twitter (convertStoredData) where

import System.Environment
import Data.Aeson
import Data.String.Conversions
import Data.Time
import Data.Text (Text)
import GHC.Generics

import Model.Schema

type Entry = (UTCTime, [TwitterTrend])

data TwitterTrend = TwitterTrend
  {
     tweet_volume :: Int
  ,  name :: Text
  } deriving (Show, Generic, FromJSON)

toTrendData :: (UTCTime, TwitterTrend) -> TrendData
toTrendData (time, trend) = TrendData "Twitter"
                                       time
                                       (name trend)
                                       0.5 --no sentiments stored so far
                                       (tweet_volume trend)

getEntries :: [String] -> [Entry]
getEntries (time : user : trendJson : xs) = 
  let localTime = parseTimeOrError False defaultTimeLocale "%a %b %e %T UTC %Y"
                    time
      date = localTimeToUTC utc localTime
      Just trends = decode (cs trendJson) :: Maybe [TwitterTrend]
 in (date, trends) : getEntries xs
getEntries _ = []  

convertStoredData :: IO [TrendData]
convertStoredData = do
  --TODO: replace file path with the accurate path
  storedData <- readFile "twitter_trends.json" >>= pure . lines
  let trends :: [Entry]
      trends = getEntries storedData
  pure . map toTrendData $ concatMap sequenceA trends

