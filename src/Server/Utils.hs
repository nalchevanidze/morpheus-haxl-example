{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Server.Utils
  ( httpEndpoint,
    startServer,
    isSchema,
  )
where

-- examples
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Morpheus
  ( runApp,
  )
import Data.Morpheus.Server
  ( httpPlayground,
  )
import Data.Morpheus.Types
  ( App,
    render,
  )
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setPort,
  )
import Web.Scotty
  ( ActionM,
    RoutePattern,
    ScottyM,
    body,
    get,
    param,
    post,
    raw,
    scottyApp,
  )
import Prelude

isSchema :: ActionM String
isSchema = param "schema"

httpEndpoint :: RoutePattern -> App e IO -> ScottyM ()
httpEndpoint route app = do
  get route $
    (isSchema *> raw (render app))
      <|> raw httpPlayground
  post route $ raw =<< (liftIO . runApp app =<< body)

startServer :: ScottyM () -> IO ()
startServer app = do
  httpApp <- scottyApp app
  runSettings settings httpApp
  where
    settings = setPort 3000 defaultSettings
