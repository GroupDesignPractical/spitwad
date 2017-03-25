module Javascript where

import Data.Text (Text)
import qualified Data.Text.IO as T
import Servant.JS

import Api

apiJS :: Text
apiJS = jsForAPI api $ angular defAngularOptions

writeJSFiles :: IO ()
writeJSFiles =
  T.writeFile "static/api.js" apiJS
