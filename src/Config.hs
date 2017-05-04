{-# LANGUAGE OverloadedStrings #-}
module Config where

import Control.Monad.Logger (runNoLoggingT)
import Data.Maybe
import Data.String.Conversions
import Data.Text
import Database.Persist.Sqlite
import System.Environment

data Config = Config
  {
    connectionString :: Text
  , connectionPool :: ConnectionPool
  , port :: Int
  , nworkers :: Int
  , stockBootstrapFilePath :: FilePath
  , newsSourceBootstrapFilePath :: FilePath
  , trendSourceBootstrapFilePath :: FilePath
  , quandlApiKey :: Maybe Text
  , newsApiKey :: Maybe Text
  , twitterOauthConsumerKey :: Maybe Text
  , twitterOauthConsumerSecret :: Maybe Text
  , twitterOauthToken :: Maybe Text
  , twitterOauthTokenSecret :: Maybe Text
  , facebookAccessToken :: Maybe Text
  }

makeConfig :: IO Config
makeConfig = do
  p <- fromMaybe ":memory:" <$> lookupEnv "SPITWAD_DB_PATH"
  n <- maybe 4 read <$> lookupEnv "SPITWAD_MAX_WORKERS"
  qapi <- lookupEnv "SPITWAD_QUANDL_API_KEY"
  napi <- lookupEnv "SPITWAD_NEWS_API_KEY"
  tock <- lookupEnv "SPITWAD_TWITTER_OAUTH_CONSUMER_KEY"
  tocs <- lookupEnv "SPITWAD_TWITTER_OAUTH_CONSUMER_SECRET"
  tot <- lookupEnv "SPITWAD_TWITTER_OAUTH_TOKEN"
  tots <- lookupEnv "SPITWAD_TWITTER_OAUTH_TOKEN_SECRET"
  fbat <- lookupEnv "SPITWAD_FACEBOOK_ACCESS_TOKEN"
  pool <- runNoLoggingT $ createSqlitePool (cs p) 100
  return Config
    {
      connectionString = cs p
    , connectionPool = pool
    , port = 3000
    , nworkers = n
    , stockBootstrapFilePath = "dump/stock_table_data.sql"
    , newsSourceBootstrapFilePath = "dump/news_source_table_data.sql"
    , trendSourceBootstrapFilePath = "dump/trend_source_table_data.sql"
    , quandlApiKey = cs <$> qapi
    , newsApiKey = cs <$> napi
    , twitterOauthConsumerKey = cs <$> tock
    , twitterOauthConsumerSecret = cs <$> tocs
    , twitterOauthToken = cs <$> tot
    , twitterOauthTokenSecret = cs <$> tots
    , facebookAccessToken = cs <$> fbat
    }

