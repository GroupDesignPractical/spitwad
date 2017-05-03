{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module App (app, run) where

import Control.Lens hiding ((<.))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Maybe
import Data.String.Conversions
import Data.Time
import Data.Text (Text)
import qualified Data.Text.IO as T.IO

import Network.Wai.Middleware.Cors
import qualified Network.Wai.Handler.Warp as Warp
import Web.Authenticate.OAuth
import Database.Persist.Sql
import Servant

import Api
import Config
import Model
import Scrape.NewsAPI
import Scrape.Quandl
import Scrape.Twitter

newtype App a = App
  {
    runApp :: ReaderT Config Handler a
  } deriving (Functor, Applicative, Monad,
              MonadReader Config, MonadError ServantErr, MonadIO)

app :: Config -> Application
app cfg = simpleCors (serve api $ withConfig cfg)

withConfig :: Config -> Server API
withConfig cfg = enter (convertApp cfg) server

convertApp :: Config -> App :~> Handler
convertApp cfg = Nat (flip runReaderT cfg . runApp)

run :: Config -> IO ()
run cfg = flip runReaderT cfg $ do
  p <- asks port
  pool <- asks connectionPool
  liftIO $ putStrLn "Initialising"
  runSqlPool (runMigration migrateAll) pool
  liftIO $ putStrLn "Migrations done"
  sc <- runDb $ count ([] :: [Filter Stock])
  when (sc == 0) $ do
    fp <- asks stockBootstrapFilePath
    runReaderT (initialiseDb fp) cfg
  nsc <- runDb $ count ([] :: [Filter NewsSource])
  when (nsc == 0) $ do
    fp <- asks newsSourceBootstrapFilePath
    runReaderT (initialiseDb fp) cfg
  tsc <- runDb $ count ([] :: [Filter TrendSource])
  when (tsc == 0) $ do
    fp <- asks trendSourceBootstrapFilePath
    runReaderT (initialiseDb fp) cfg
  liftIO $ putStrLn "Loaded"
  napiKey <- asks newsApiKey
  when (isJust napiKey)
    . liftIO . putStrLn $ "Using News API key " <> cs (fromJust napiKey)
  qapiKey <- asks quandlApiKey
  when (isJust qapiKey)
    . liftIO . putStrLn $ "Using Quandl API key " <> cs (fromJust qapiKey)
  tock <- asks twitterOauthConsumerKey
  when (isJust tock)
    . liftIO . putStrLn $ "Using Twitter OAuth consumer key "
      <> cs (fromJust tock)
  tocs <- asks twitterOauthConsumerSecret
  when (isJust tocs)
    . liftIO . putStrLn $ "Using Twitter OAuth consumer secret "
      <> cs (fromJust tocs)
  tot <- asks twitterOauthToken
  when (isJust tot)
    . liftIO . putStrLn $ "Using Twitter OAuth token " <> cs (fromJust tot)
  tots <- asks twitterOauthTokenSecret
  when (isJust tots)
    . liftIO . putStrLn $ "Using Twitter OAuth token secret "
      <> cs (fromJust tots)
  liftIO . Warp.run p $ app cfg

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO a -> m a
runDb q = do
  pool <- asks connectionPool
  liftIO $ runSqlPool q pool

server :: ServerT API App
server = getStocks :<|> getTrendSources :<|> getNewsSources
    :<|> getStockDataInterval :<|> getTrendDataInterval :<|> getNewsInterval
    :<|> updateStockData :<|> updateNewsData :<|> updateTrends
  where getStocks :: App [Stock]
        getStocks = do
          stocks <- runDb $ selectList [] []
          pure $ entityVal <$> stocks
        getTrendSources :: App [TrendSource]
        getTrendSources = do
          trendSources <- runDb $ selectList [] []
          pure $ entityVal <$> trendSources
        getNewsSources :: App [NewsSource]
        getNewsSources = do
          newsSources <- runDb $ selectList [] []
          pure $ entityVal <$> newsSources
        getStockDataInterval :: Maybe Text -- stock_symbol
                              -> Maybe UTCTime -> Maybe UTCTime -- start, end
                              -> App StockDataInterval
        getStockDataInterval (Just ssym) (Just s) (Just e) = do
          selected <- runDb $ selectList
            [StockDataStock_symbol ==. ssym
           , StockDataDate >. s, StockDataDate <. e]
            [Asc StockDataDate]
          let points = entityVal <$> selected
          pure StockDataInterval
            {
              stockDataIntervalTimePeriod = TimePeriod
                {
                  start = points ^? _head . stockDataDate
                , end = points ^? _last . stockDataDate
                , splits = length selected
                }
            , stockData = points
            }
        getStockDataInterval (Just _) _ _ = throwError
          err400 { errBody = "Bad Request, invalid dates" }
        getStockDataInterval _ _ _ = throwError
          err400 { errBody = "Bad Request, invalid stock name" }
        getTrendDataInterval :: Maybe Text -- trend_source_name
                             -> Maybe UTCTime -> Maybe UTCTime -- start, end
                             -> App TrendDataInterval
        getTrendDataInterval (Just tsname) (Just s) (Just e) = do
          selected <- runDb $ selectList
            [TrendDataTrend_source_name ==. tsname
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
            , trendData = points
            }
        getTrendDataInterval (Just _) _ _ = throwError
          err400 { errBody = "Bad Request, invalid dates" }
        getTrendDataInterval _ _ _ = throwError
          err400 { errBody = "Bad Request, invalid trend source name" }
        getNewsInterval :: Maybe Text -- news_source_name
                        -> Maybe UTCTime -> Maybe UTCTime -- start, end
                        -> App [NewsData]
        getNewsInterval (Just nsaname) (Just s) (Just e) = do
          selected <- runDb $ selectList
            [NewsDataNews_source_api_name ==. nsaname
           , NewsDataDate >. s, NewsDataDate <. e]
            [Asc NewsDataDate]
          pure $ entityVal <$> selected
        getNewsInterval (Just _) _ _ = throwError
          err400 { errBody = "Bad Request, invalid dates" }
        getNewsInterval _ _ _ = throwError
          err400 { errBody = "Bad Request, invalid news source name" }
        updateStockData :: Maybe Text -> Maybe UTCTime -> App Bool
        updateStockData (Just sym) since = do
          -- TODO: don't retrieve stock data for values already in db
          apiKey <- asks quandlApiKey
          rows <- liftIO $ scrapeStockData (cs sym) since apiKey
          -- TODO: use insertBy and collect failures
          res <- mapM (runDb . insertUnique) rows
          pure . or $ map isJust res
        updateStockData Nothing since = do
          selected <- runDb $ selectList [] []
          let stocks = entityVal <$> selected
              symbols = map (^. stockSymbol) stocks
          res <- mapM (flip updateStockData since . Just) symbols
          pure $ or res
        updateNewsData :: Maybe Text -> App Bool
        updateNewsData (Just aname) = do
          apiKey <- asks newsApiKey
          rows <- liftIO $ scrapeNewsData (cs aname) apiKey
          -- TODO: use insertBy and collect failures
          res <- mapM (runDb . insertUnique) rows
          pure . or $ map isJust res
        updateNewsData Nothing = do
          selected <- runDb $ selectList [] []
          let newsSources = entityVal <$> selected
              apiNames = map (^. newsSourceApi_name) newsSources
          res <- mapM (updateNewsData . Just) apiNames
          pure $ or res
        updateTrends :: App Bool
        updateTrends = do
          tock <- asks twitterOauthConsumerKey
          tocs <- asks twitterOauthConsumerSecret
          tot <- asks twitterOauthToken
          tots <- asks twitterOauthTokenSecret
          let tokens = newOAuth
                {
                  oauthConsumerKey = cs $ fromMaybe "" tock
                , oauthConsumerSecret = cs $ fromMaybe "" tocs
                }
              credential = Credential
                [
                  ("oauth_token", cs $ fromMaybe "" tot)
                , ("oauth_token_secret", cs $ fromMaybe "" tots)
                ]
          rows <- liftIO $ scrapeTwitterTrends tokens credential
          _ <- runDb $ insertMany rows
          pure True

initialiseDb :: (MonadReader Config m, MonadIO m, MonadBaseControl IO m) =>
                FilePath -> m ()
initialiseDb sqlfp = do
  sql <- liftIO $ T.IO.readFile sqlfp
  liftIO . print $ "Bootstrapping..." <> sqlfp
  -- liftIO . putStrLn $ cs bootstrap
  pool <- asks connectionPool
  runSqlPool (rawExecute sql []) pool
  pure ()

