{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Text

data Config = Config
  {
    connectionString :: Text
  , port :: Int
  , bootstrapFilePath :: FilePath
  }

defaultConfig :: Config
defaultConfig = Config ":memory:" 3000 "dump/stock_table_data.sql"

