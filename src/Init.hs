{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Init where

import Control.Exception.Safe
import Data.Typeable (typeOf)
import Database.Persist.Postgresql (runSqlPool)
import Network.Wai.Handler.Warp (run)
import Say
import Servant.Server (Application)

import qualified Data.Text as Text

import App (mkApp)
import Config
import Database (doMigrations, makeDatabasePool)

withConfig :: (Config -> IO a) -> IO a
withConfig runAppFromConfig = do
  say "Creating configuration"
  let env = Development

  dbConfig <- getDatabaseConfig

  !pool <- makeDatabasePool dbConfig 

  runAppFromConfig $ Config
    { configEnv = env,
      databaseConnectionPool = pool
    }

initialize :: Config -> IO Application
initialize cfg = do
  bracket
    (say "Satarting to run migrations")
    (\_ -> say "Finished running migrations")
    $ \_ -> do
      say "Running migrations..."
      runSqlPool doMigrations (databaseConnectionPool cfg) `catch` \(SomeException e) -> do
        say $ mconcat
          [ "exception in doMigrations fo type:"
          , Text.pack . show $ (typeOf e)
          , ", shown: "
          , Text.pack . show $ e
          ]
        throwIO e
      say "Completed runSqlPool"
  return $ mkApp cfg

runApp :: IO ()
runApp = do
  say "Initializing app"
  withConfig $ \config -> do
    say "Config acquired"
    app <- initialize config
    run 8081 app
