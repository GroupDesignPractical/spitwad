module Main where

import Data.String.Conversions
import System.Environment

import App
import Config

main :: IO ()
main = do
  args <- getArgs
  let config = if length args == 1
               then defaultConfig { connectionString = cs $ head args }
               else defaultConfig
  run config

