{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Database where

import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (asks, liftIO, MonadIO, MonadReader, runReaderT)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.Postgresql
import Database.Persist.TH
import Database.Persist.Quasi
import Say

import Config (Config, configPool)


share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistManyFileWith lowerCaseSettings [
      "config/models/User.persistentmodels"
    , "config/models/Account.persistentmodels"
    ]
  )

doMigrations :: SqlPersistT IO ()
doMigrations = do
  liftIO $ say "Database - Starting migrations"
  runMigration migrateAll
  liftIO $ say "Database - Migrations ran successfuly"

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks configPool
  liftIO $ runSqlPool query pool
