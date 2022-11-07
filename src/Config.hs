{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Exception.Safe (throwIO)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader (ReaderT, asks, MonadReader)
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Database.Persist.Postgresql 
  (ConnectionPool, ConnectionString, createPostgresqlPool)
import Safe (readMay)
import Servant (ServerError)
import System.Environment (lookupEnv)

import qualified Data.ByteString.Char8 as BS



data Environment = Development | Test | Production
  deriving (Eq, Show, Read)


data DatabaseConfig =
  DatabaseConfig 
    { host :: String
    , port :: String
    , user :: String
    , password :: String
    , dbname :: String
    , poolConnectionsNumber :: Int
    }


data Config = 
  Config 
    { databaseConnectionPool :: ConnectionPool
    , configEnv :: Environment
    }


newtype AppT m a =
  AppT { runApp :: ReaderT Config (ExceptT ServerError m)  a }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServerError, MonadIO)


type App a = AppT IO a


lookupStrSetting :: String -> IO String
lookupStrSetting env = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing ->  handleFailedRead env
        Just str -> return str
  where
    handleFailedRead str = 
        error $ mconcat ["Environment variable [[" , env , "]] not found"]


lookupSetting :: Read a => String -> IO a
lookupSetting env = do
    strSetting <- lookupStrSetting env
    maybe (handleFailedRead strSetting) return (readMay strSetting)
  where
    handleFailedRead str = 
        error $ mconcat
            ["Failed to parse value \"", str, "\" for environment variable [[", env, "]]"]


getDatabaseConfig :: IO DatabaseConfig
getDatabaseConfig = do
  DatabaseConfig 
    <$> lookupStrSetting "PG_HOST"
    <*> lookupStrSetting "PG_PORT"
    <*> lookupStrSetting "PG_USER"
    <*> lookupStrSetting "PG_PASSWORD"
    <*> lookupStrSetting "PG_DATABASE"
    <*> lookupSetting "PG_CONNECTIONS_NUM"
