{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module App (app, run) where

import Control.Lens hiding ((<.))
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Time

import qualified Network.Wai.Handler.Warp as Warp
import Database.Persist.Sqlite
import Servant

import Api
import Config
import Model

newtype App a = App
  {
    runApp :: ReaderT Config Handler a
  } deriving (Functor, Applicative, Monad,
              MonadReader Config, MonadError ServantErr, MonadIO)

app :: Config -> Application
app cfg = serve api $ withConfig cfg

withConfig :: Config -> Server API
withConfig cfg = enter (convertApp cfg) server

convertApp :: Config -> App :~> Handler
convertApp cfg = Nat (flip runReaderT cfg . runApp)

run :: Config -> IO ()
run cfg = flip runReaderT cfg $ do
  p <- asks port
  fp <- asks connectionString
  runSqlite fp $ runMigration migrateAll
  -- runReaderT initialiseDb cfg
  liftIO . Warp.run p $ app cfg

runDb :: (MonadReader Config m, MonadIO m)
      => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runDb q = do
  fp <- asks connectionString
  liftIO $ runSqlite fp q

server :: ServerT API App
server = getMarkets :<|> getTrendSources :<|> getNewsSources
    :<|> getMarketDataInterval :<|> getTrendDataInterval :<|> getNewsInterval
  where getMarkets :: App [Market]
        getMarkets = do
          markets <- runDb $ selectList [] []
          pure $ entityVal <$> markets
        getTrendSources :: App [TrendSource]
        getTrendSources = do
          trendSources <- runDb $ selectList [] []
          pure $ entityVal <$> trendSources
        getNewsSources :: App [NewsSource]
        getNewsSources = do
          newsSources <- runDb $ selectList [] []
          pure $ entityVal <$> newsSources
        getMarketDataInterval :: Maybe MarketId -- market_name
                              -> Maybe UTCTime -> Maybe UTCTime -- start, end
                              -> App MarketDataInterval
        getMarketDataInterval (Just mname) (Just s) (Just e) = do
          selected <- runDb $ selectList
            [MarketDataMarketName ==. mname
           , MarketDataDate >. s, MarketDataDate <. e]
            [Asc MarketDataDate]
          let points = entityVal <$> selected
          pure MarketDataInterval
            {
              marketDataIntervalTimePeriod = TimePeriod
                {
                  start = points ^? _head . marketDataDate
                , end = points ^? _last . marketDataDate
                , splits = length selected
                }
            , marketData = (^. marketDataDatum) <$> points
            }
        getMarketDataInterval (Just _) _ _ = throwError
          err400 { errBody = "Bad Request, invalid dates" }
        getMarketDataInterval _ _ _ = throwError
          err400 { errBody = "Bad Request, invalid market name" }
        getTrendDataInterval :: Maybe TrendSourceId -- trend_source_name
                             -> Maybe UTCTime -> Maybe UTCTime -- start, end
                             -> App TrendDataInterval
        getTrendDataInterval (Just tsname) (Just s) (Just e) = do
          selected <- runDb $ selectList
            [TrendDataTrendSourceName ==. tsname
           , TrendDataDate >. s, TrendDataDate <. e]
            [Asc TrendDataDate]
          let points = entityVal <$> selected
          pure TrendDataInterval
            {
              trendDataIntervalTimePeriod = TimePeriod
                {
                  start = points ^? _head . trendDataDate
                , end = points ^? _last . trendDataDate
                , splits = length selected
                }
            , trendData = (^. trendDataDatum) <$> points
            }
        getTrendDataInterval (Just _) _ _ = throwError
          err400 { errBody = "Bad Request, invalid dates" }
        getTrendDataInterval _ _ _ = throwError
          err400 { errBody = "Bad Request, invalid trend source name" }
        getNewsInterval :: Maybe NewsSourceId -- news_source_name
                        -> Maybe UTCTime -> Maybe UTCTime -- start, end
                        -> App [NewsData]
        getNewsInterval (Just nsname) (Just s) (Just e) = do
          selected <- runDb $ selectList
            [NewsDataNewsSourceName ==. nsname
           , NewsDataDate >. s, NewsDataDate <. e]
            [Asc NewsDataDate]
          pure $ entityVal <$> selected
        getNewsInterval (Just _) _ _ = throwError
          err400 { errBody = "Bad Request, invalid dates" }
        getNewsInterval _ _ _ = throwError
          err400 { errBody = "Bad Request, invalid news source name" }

-- initialiseDb :: (MonadReader Config m, MonadIO m) => m ()
-- initialiseDb = do
--   return ()

