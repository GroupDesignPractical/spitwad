{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Scrape.NewsAPI (scrapeNewsData) where

import Data.Aeson
import Data.Maybe
import Data.String.Conversions
import Data.Text (Text)
import Data.Time
import GHC.Generics
import Network.HTTP.Simple
import Network.URI

import Model.Schema
import Model.URI

newtype Headlines = Headlines { getArticles :: [NewsArticle] }

data NewsArticle = NewsArticle
  {
    title :: Text
  , description :: Text
  , url :: URI
  , publishedAt :: Maybe UTCTime
  } deriving (Show, Generic, FromJSON)

instance FromJSON Headlines where
  parseJSON = withObject "response" $ \o ->
    Headlines <$> o .: "articles"

getNewsJSON :: String -> Maybe Text -> IO [NewsArticle]
getNewsJSON source apiKey = parseRequest
  ("GET https://newsapi.org/v1/articles?"
    <> "source=" <> source
    <> maybe "" (("&apiKey=" <>) . cs) apiKey
  )
  >>= httpJSON
  >>= (pure . getArticles . getResponseBody)

newsAPINewsData :: String -> NewsArticle -> IO NewsData
newsAPINewsData source na = (\t -> NewsData
  (cs source) (fromMaybe t $ publishedAt na) -- default to current time
  (title na) (description na) (url na)
  0 0 0 0 0 0) -- facebook reacts
  <$> getCurrentTime

scrapeNewsData :: String -> Maybe Text -> IO [NewsData]
scrapeNewsData source apiKey = getNewsJSON source apiKey
  >>= mapM (newsAPINewsData source)
