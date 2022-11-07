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
import Servant (ServerError)
import System.Environment (lookupEnv)

import qualified Data.ByteString.Char8 as BS


data Environment = Development | Test | Production deriving (Eq, Show, Read)

data Config = 
  Config 
    { configPool :: ConnectionPool
    , configEnv :: Environment
    }

newtype AppT m a =
  AppT { runApp :: ReaderT Config (ExceptT ServerError m)  a }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServerError, MonadIO)

type App a = AppT IO a

makePool :: Environment -> IO ConnectionPool
makePool Test = 
  runNoLoggingT $ createPostgresqlPool localDbConnectionStr (poolConnectionsAmount Test)
makePool Development = 
  runNoLoggingT $ createPostgresqlPool localDbConnectionStr (poolConnectionsAmount Development)
makePool Production = do
  pool <- runMaybeT $ do
    let keys = ["host=", "port=", "user=", "password=", "dbname="]
        envs = ["PGHOST", "PGPORT", "PGUSER", "PGPASSWORD", "PGDATABASE"]

    envVars <- traverse (MaybeT . lookupEnv) envs
    let prodStr = BS.intercalate " " . zipWith (<>) keys $ BS.pack <$> envVars
    lift $ runStdoutLoggingT $ createPostgresqlPool prodStr (poolConnectionsAmount Production)
  case pool of
    Nothing -> throwIO (userError "Database connections pool creation failed.")
    Just a -> return a

poolConnectionsAmount :: Environment -> Int
poolConnectionsAmount Production  = 8
poolConnectionsAmount _           = 1

localDbConnectionStr :: ConnectionString
localDbConnectionStr = "host=localhost dbname=testdb user=postgres password=postgres port=5438"
