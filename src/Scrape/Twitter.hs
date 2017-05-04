{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrape.Twitter (scrapeTwitterTrends) where

import Data.Aeson
import Data.Maybe
import Data.Time
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Simple
import Web.Authenticate.OAuth

import Model.Schema

type Entry = (UTCTime, [TwitterTrend])

data TwitterTrend = TwitterTrend
  {
     tweet_volume :: Maybe Int
  ,  name :: Text
  } deriving (Show, Generic, FromJSON)

newtype TwitterResponse = TwitterResponse
  {
    getTwitterResponse :: [TwitterTrend]
  } deriving (Show)

instance FromJSON TwitterResponse where
  parseJSON = withObject "response" $ \o -> TwitterResponse <$> o .: "trends"

twitterTrendData :: (UTCTime, TwitterTrend) -> TrendData
twitterTrendData (time, trend) = TrendData "Twitter"
                                           time
                                           (name trend)
                                           0.5 -- no sentiments stored so far
                                           (fromMaybe 0 $ tweet_volume trend)

getTwitterJSON :: OAuth -> Credential -> IO Entry
getTwitterJSON tokens credential = parseRequest
  "GET https://api.twitter.com/1.1/trends/place.json?id=1"
  >>= signOAuth tokens credential
  >>= httpJSON
  >>= (\ts -> getCurrentTime >>= pure . flip (,) ts)
        . getTwitterResponse . head . getResponseBody

scrapeTwitterTrends :: OAuth -> Credential -> IO [TrendData]
scrapeTwitterTrends tokens credential = getTwitterJSON tokens credential
  >>= pure . map twitterTrendData . sequenceA

-- auxiliary functions to insert pre-serialised data

-- getEntries :: [String] -> [Entry]
-- getEntries (time : _ : trendJson : xs) =
--   let localTime = parseTimeOrError False defaultTimeLocale "%a %b %e %T UTC %Y"
--                     time
--       date = localTimeToUTC utc localTime
--       Just trends = decode (cs trendJson) :: Maybe [TwitterTrend]
--  in (date, trends) : getEntries xs
-- getEntries _ = []
--
-- convertStoredData :: IO [TrendData]
-- convertStoredData = do
--   --TODO: replace file path with the accurate path
--   storedData <- readFile "twitter_trends.json" >>= pure . lines
--   let trends :: [Entry]
--       trends = getEntries storedData
--   pure . map twitterTrendData $ concatMap sequenceA trends

