{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database where

import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (asks, liftIO, MonadIO, MonadReader, runReaderT)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.Postgresql
import Database.Persist.TH
import Database.Persist.Quasi
import Say

import Config (Config, databaseConnectionPool, DatabaseConfig(..))

import qualified Data.ByteString.Char8 as BS


share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistManyFileWith lowerCaseSettings [
      "config/models/User.persistentmodels"
    , "config/models/Account.persistentmodels"
    ]
  )


doMigrations :: SqlPersistT IO ()
doMigrations = do
  say "Database - Starting migrations"
  runMigration migrateAll
  say "Database - Migrations ran successfuly"


runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks databaseConnectionPool
  liftIO $ runSqlPool query pool


makeDatabasePool :: DatabaseConfig -> IO ConnectionPool
makeDatabasePool DatabaseConfig{..} = do
  let keys = ["host=", "port=", "user=", "password=", "dbname="]
      values = [host, port, user, password, dbname]
      connStr = BS.intercalate " " . zipWith (<>) keys $ BS.pack <$> values

  runStdoutLoggingT $ createPostgresqlPool connStr poolConnectionsNumber
