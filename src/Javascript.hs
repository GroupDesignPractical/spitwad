{-# LANGUAGE OverloadedStrings #-}
module Javascript where

import Data.Text (Text)
import qualified Data.Text.IO as T
import Servant.JS

import Api

apiJS :: Text
apiJS = jsForAPI api $ angularService defAngularOptions
  { serviceName = "spitwad" }

writeJSFiles :: IO ()
writeJSFiles =
  T.writeFile "static/api.js" apiJS

main :: IO ()
main = writeJSFiles
