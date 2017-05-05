{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Scrape.Facebook (getReacts) where

import Control.Concurrent.Async.Lifted
import Control.Concurrent.MVar
import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Loops
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Aeson.Types
import qualified Control.Concurrent.Map as M
import Data.Maybe
import Data.String.Conversions
import Data.Text (Text)
import Data.Time
import GHC.Generics
import Network.HTTP.Simple
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Client.TLS as C.TLS
import Network.URI

import Model

data Posts = Posts
  {
    _data :: [Post]
  , _paging :: Paging
  } deriving (Show, Generic)

data Post = Post
  {
    _link :: URI
  , _id :: Text
  } deriving (Show, Generic)

data Paging = Paging
  {
    _previous :: Maybe Text
  , _next :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON Posts where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON Post where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON Paging where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON FacebookReacts where
  parseJSON = withObject "reacts" $ \o -> do
    likes <- o .: "like" >>= \x -> x .: "summary"
    loves <- o .: "love" >>= \x -> x .: "summary"
    hahas <- o .: "haha" >>= \x -> x .: "summary"
    wows <- o .: "wow" >>= \x -> x .: "summary"
    sads <- o .: "sad" >>= \x -> x .: "summary"
    angrys <- o .: "angry" >>= \x -> x .: "summary"
    FacebookReacts <$> (likes .: "total_count") <*> (loves .: "total_count")
                   <*> (hahas .: "total_count") <*> (wows .: "total_count")
                   <*> (sads .: "total_count")  <*> (angrys .: "total_count")

getPosts :: (MonadIO m, MonadLogger m, MonadThrow m)
         => Maybe Text -> Text -> UTCTime -> m Posts
getPosts apiToken newspage date = parseRequest
  ("GET https://graph.facebook.com/v2.9/" <> cs newspage
    <> "/posts?fields=link&format=json&limit=100"
    <> maybe "" (("&access_token=" <>) . cs) apiToken
    <> "&since=" <> addTime (-2*24*60*60)
    <> "&until=" <> addTime (2*24*60*60)
  )
  >>= httpJSON
  >>= pure . getResponseBody
    where
      addTime :: Integer -> String
      addTime = formatTime defaultTimeLocale "%F" . flip addUTCTime date
                                                  . fromIntegral

unshortenUrl :: (MonadIO m, MonadLogger m, MonadThrow m)
             => URI -> m Text
unshortenUrl url = do
  manager <- liftIO $ C.newManager C.TLS.tlsManagerSettings
  req <- liftIO . C.parseRequest $ show url
  hres <- liftIO $ C.responseOpenHistory req manager
  let freq = C.hrFinalRequest hres
  pure . cs $ C.path freq

crawlPosts :: (MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadLogger m,
               MonadThrow m)
           => M.Map Text Text -> Maybe Text -> Text -> UTCTime -> m ()
crawlPosts m apiToken newspage date = do
  posts <- getPosts apiToken newspage date
  $(logInfo) $ "\x1b[32mCrawling page 0 of " <> newspage <> "\x1b[0m"
  mapConcurrently_ (\x -> catchAny (do
    p <- unshortenUrl $ _link x
    liftIO $ M.insert p (_id x) m)
    (\e -> $(logError) (cs $ show e))
    ) $ _data posts
  _i <- liftIO $ newMVar (1 :: Int)
  _n <- liftIO $ newMVar $! _next $ _paging posts
  whileM_
    (do
    i <- liftIO $ readMVar _i
    n <- liftIO $ readMVar _n
    pure $ i <= 15 && isJust n
    ) (do
      i <- liftIO $ takeMVar _i
      $(logInfo) $ "\x1b[32mCrawling page " <> cs (show i)
                    <> " of " <> newspage <> "\x1b[0m"
      Just n <- liftIO $ takeMVar _n
      nposts <- liftIO $ parseRequest (cs n) >>= httpJSON
                                             >>= pure . getResponseBody
      mapConcurrently_ (\x -> catchAny (do
        p <- unshortenUrl $ _link x
        liftIO $ M.insert p (_id x) m)
        (\e -> $(logError) (cs $ show e))
        ) $ _data nposts
      liftIO . putMVar _i $! i + 1
      liftIO . putMVar _n $! _next $ _paging nposts
     )

getReacts :: (MonadBaseControl IO m, MonadCatch m, MonadIO m, MonadLogger m)
          => Maybe Text -> Text -> [URI] -> m [Maybe FacebookReacts]
getReacts apiToken newspage links = do
  m <- liftIO M.empty
  date <- liftIO getCurrentTime
  catchAny (crawlPosts m apiToken newspage date)
           (\e -> $(logError) (cs $ show e))
  mapConcurrently (runMaybeT . getReact m apiToken) links

getReact :: (MonadBaseControl IO m, MonadIO m, MonadLogger m)
         => M.Map Text Text -> Maybe Text -> URI -> MaybeT m FacebookReacts
getReact m apiToken url = do
  let p = uriPath url
  postId <- MaybeT . liftIO $ M.lookup (cs p) m
  $(logInfo) $ "\x1b[32mGetting reacts for " <> cs p <> "\x1b[0m"
  req <- liftIO $ parseRequest ("GET https://graph.facebook.com/v2.9/"
             <> cs postId
             <> "?fields=reactions.type(LIKE).summary(true).as(like),"
             <> "reactions.type(LOVE).summary(true).as(love),"
             <> "reactions.type(HAHA).summary(true).as(haha),"
             <> "reactions.type(WOW).summary(true).as(wow),"
             <> "reactions.type(SAD).summary(true).as(sad),"
             <> "reactions.type(ANGRY).summary(true).as(angry)"
             <> "&format=json"
             <> maybe "" (("&access_token=" <>) . cs) apiToken
         )
  res <- liftIO $ httpJSON req
  pure $ getResponseBody res
