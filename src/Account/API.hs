{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Account.API where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text
import GHC.Generics
import Servant

import Config


data HealthCheck = 
  HealthCheck {status :: Text} deriving (Eq, Show, Generic)

instance ToJSON HealthCheck


type AccountAPI = "health" :> Get '[JSON] HealthCheck

healthCheck :: (MonadIO m) => AppT m HealthCheck
healthCheck = return $ HealthCheck { status="live" }
