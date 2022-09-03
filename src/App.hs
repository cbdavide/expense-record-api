{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module App where

import Control.Monad.Reader (runReaderT, MonadIO)
import Servant
import Servant.Server

import Account.API
import Config

type API = AccountAPI

api :: Proxy API
api = Proxy

appToHandler :: Config -> App a -> Handler a
appToHandler cfg app = Handler $ runReaderT (runApp app) cfg

serverT :: MonadIO m => ServerT API (AppT m)
serverT = healthCheck

mkServer :: Config -> Server API
mkServer cfg = hoistServer api (appToHandler cfg) serverT

mkApp :: Config -> Application
mkApp cfg = serve api (mkServer cfg)
