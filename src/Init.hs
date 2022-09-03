{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Init where

import Control.Exception.Safe
import qualified Data.Text as Text
import Data.Typeable (typeOf)
import Database.Persist.Postgresql (runSqlPool)
import Network.Wai.Handler.Warp (run)
import Say
import Servant.Server (Application)

import App (mkApp)
import Config
import Database (doMigrations)

withConfig :: (Config -> IO a) -> IO a
withConfig runAppFromConfig = do
  say "Creating configuration"
  let env = Development

  !pool <- makePool env

  runAppFromConfig $ Config
    { configEnv = env,
      configPool = pool
    }

initialize :: Config -> IO Application
initialize cfg = do
  -- bracket :: IO a -> (a -> IO b) -> (b -> IO c)
  bracket
    (say "Satarting to run migrations") -- IO ()
    (\_ -> say "Finished running migrations") -- a -> IO ()
    $ \_ -> do -- a -> IO ()
      say "Running migrations..." -- IO ()
      -- doMigrations :: SqlPersistT IO ()
      -- runSqlPool doMigrations pool :: IO ()
      runSqlPool doMigrations (configPool cfg) `catch` \(SomeException e) -> do
        say $ mconcat
          [ "exception in doMigrations fo type:"
          , Text.pack . show $ (typeOf e)
          , ", shown: "
          , Text.pack . show $ e
          ]
        throwIO e
      say "Completed runSqlPool" -- IO ()
  return $ mkApp cfg

runApp :: IO ()
runApp = do
  say "Initializing app"
  withConfig $ \config -> do
    say "Config acquired"
    app <- initialize config
    run 8081 app
