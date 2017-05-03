{-# LANGUAGE OverloadedStrings #-}
module Config where

import Control.Monad.Logger (runStderrLoggingT)
import Data.Either.Combinators
import Data.String.Conversions
import Data.Text
import Database.Persist.Sqlite
import System.Environment
import qualified System.IO.Error as E

data Config = Config
  {
    connectionString :: Text
  , connectionPool :: ConnectionPool
  , port :: Int
  , stockBootstrapFilePath :: FilePath
  , newsSourceBootstrapFilePath :: FilePath
  , trendSourceBootstrapFilePath :: FilePath
  , quandlApiKey :: Maybe Text
  , newsApiKey :: Maybe Text
  , twitterOauthConsumerKey :: Maybe Text
  , twitterOauthConsumerSecret :: Maybe Text
  , twitterOauthToken :: Maybe Text
  , twitterOauthTokenSecret :: Maybe Text
  }

makeConfig :: IO Config
makeConfig = do
  p <- E.tryIOError $ getEnv "SPITWAD_DB_PATH"
  qapi <- E.tryIOError $ getEnv "SPITWAD_QUANDL_API_KEY"
  napi <- E.tryIOError $ getEnv "SPITWAD_NEWS_API_KEY"
  tock <- E.tryIOError $ getEnv "SPITWAD_TWITTER_OAUTH_CONSUMER_KEY"
  tocs <- E.tryIOError $ getEnv "SPITWAD_TWITTER_OAUTH_CONSUMER_SECRET"
  tot <- E.tryIOError $ getEnv "SPITWAD_TWITTER_OAUTH_TOKEN"
  tots <- E.tryIOError $ getEnv "SPITWAD_TWITTER_OAUTH_TOKEN_SECRET"
  let path = either (const ":memory:") cs p
  pool <- runStderrLoggingT $ createSqlitePool path 100
  return Config
    {
      connectionString = path
    , connectionPool = pool
    , port = 3000
    , stockBootstrapFilePath = "dump/stock_table_data.sql"
    , newsSourceBootstrapFilePath = "dump/news_source_table_data.sql"
    , trendSourceBootstrapFilePath = "dump/trend_source_table_data.sql"
    , quandlApiKey = cs <$> rightToMaybe qapi
    , newsApiKey = cs <$> rightToMaybe napi
    , twitterOauthConsumerKey = cs <$> rightToMaybe tock
    , twitterOauthConsumerSecret = cs <$> rightToMaybe tocs
    , twitterOauthToken = cs <$> rightToMaybe tot
    , twitterOauthTokenSecret = cs <$> rightToMaybe tots
    }

