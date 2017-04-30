module Main where

import App
import Config

main :: IO ()
main = do
  config <- makeConfig
  run config

