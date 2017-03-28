module Main where

import Data.String.Conversions
import System.Environment
import qualified System.IO.Error as E

import App
import Config

main :: IO ()
main = do
  path <- E.tryIOError $ getEnv "SPITWAD_DB_PATH"
  let config = either
        (const defaultConfig)
        (\str -> defaultConfig { connectionString = cs str })
        path
  run config

