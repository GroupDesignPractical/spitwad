{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Scrape.NewsAPI (scrapeNewsData) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson
import Data.Maybe
import Data.String.Conversions
import Data.Text (Text)
import Data.Time
import GHC.Generics
import Network.HTTP.Simple
import Network.URI

import Model.Schema

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

getNewsJSON :: (MonadCatch m, MonadIO m, MonadLogger m)
            => String -> Maybe Text -> m [NewsArticle]
getNewsJSON source apiKey = catchAny (parseRequest
  ("GET https://newsapi.org/v1/articles?"
    <> "source=" <> source
    <> maybe "" (("&apiKey=" <>) . cs) apiKey
  )
  >>= httpJSON
  >>= pure . getArticles . getResponseBody)
  (\e -> $(logError) (cs $ show e) >> pure [])

newsAPINewsData :: String -> NewsArticle -> IO NewsData
newsAPINewsData source na = (\t -> NewsData
  (cs source) (fromMaybe t $ publishedAt na) -- default to current time
  (title na) (description na) (url na)
  0 0 0 0 0 0) -- facebook reacts
  <$> getCurrentTime

scrapeNewsData :: (MonadCatch m, MonadIO m, MonadLogger m)
               => String -> Maybe Text -> m [NewsData]
scrapeNewsData source apiKey = getNewsJSON source apiKey
  >>= liftIO . mapM (newsAPINewsData source)
