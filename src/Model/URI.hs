{-# LANGUAGE OverloadedStrings #-}

module Model.URI where

import Data.Aeson.Types
import Data.Maybe
import Data.String.Conversions
import Database.Persist.Sql
import Network.URI

instance ToJSON URI where
  toJSON = String . cs . show

instance FromJSON URI where
  parseJSON = withText "url" $ pure . fromJust . parseURI . cs

instance PersistField URI where
  fromPersistValue (PersistText t) = maybe (Left "failed to parse uri") Right
                                   . parseURI $ cs t
  fromPersistValue _ = error "malformed uri"
  toPersistValue = PersistText . cs . show

instance PersistFieldSql URI where
  sqlType _ = SqlString

