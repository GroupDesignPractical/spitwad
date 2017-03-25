{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Text

data Config = Config
  {
    connectionString :: Text
  , port :: Int
  }

defaultConfig :: Config
defaultConfig = Config ":memory:" 3000

