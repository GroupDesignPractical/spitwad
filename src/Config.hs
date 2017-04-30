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
  , bootstrapFilePath :: FilePath
  , quandlApiKey :: Maybe Text
  }

makeConfig :: IO Config
makeConfig = do
  p <- E.tryIOError $ getEnv "SPITWAD_DB_PATH"
  qapi <- E.tryIOError $ getEnv "SPITWAD_QUANDL_API_KEY"
  let path = either (const ":memory:") cs p
  pool <- runStderrLoggingT $ createSqlitePool path 100
  return Config
    {
      connectionString = path
    , connectionPool = pool
    , port = 3000
    , bootstrapFilePath = "dump/stock_table_data.sql"
    , quandlApiKey = cs <$> rightToMaybe qapi
    }

