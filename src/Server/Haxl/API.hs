{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server.Haxl.API
  ( app,
    httpEndpoint,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Morpheus
  ( App,
    deriveApp,
    runApp,
  )
import Data.Morpheus.Server
  ( httpPlayground,
  )
import Data.Morpheus.Types
  ( ComposedResolver,
    QUERY,
    Resolver,
    ResolverQ,
    RootResolver (..),
    Undefined (..),
    lift,
    render,
  )
import Data.Morpheus.Types.IO
  ( MapAPI,
  )
import Data.Text (Text)
import Haxl.Core (dataFetch, initEnv, runHaxl, stateEmpty, stateSet)
import Server.Haxl.DataSource
  ( DeityReq (..),
    Haxl,
    State (DeityState),
  )
import Server.Haxl.Schema
  ( Deity (..),
    DeityArguments (..),
    ID,
    Query (..),
  )
import Server.Utils (isSchema)
import Web.Scotty
  ( RoutePattern,
    ScottyM,
    body,
    get,
    post,
    raw,
  )
import Prelude hiding (id)

-- FETCH
getDeityIds :: Haxl [ID]
getDeityIds = dataFetch GetDeityIds

getDeityNameById :: ID -> Haxl Text
getDeityNameById = dataFetch . GetDeityNameById

getDeityPowerById :: ID -> Haxl (Maybe Text)
getDeityPowerById = dataFetch . GetDeityPowerById

-- RESOLVERS
resolveDeity :: DeityArguments -> ResolverQ e Haxl Deity
resolveDeity DeityArguments {id}  =
  pure
    Deity
      { name = lift (getDeityNameById id),
        power = lift (getDeityPowerById id)
      }

resolveDeities :: ComposedResolver QUERY e Haxl [] Deity
resolveDeities = do
  ids <- lift getDeityIds
  traverse (resolveDeity . DeityArguments) ids

resolveQuery :: Query (Resolver QUERY e Haxl)
resolveQuery =
  Query
    { deity = resolveDeity,
      deities = resolveDeities
    }

rootResolver :: RootResolver Haxl () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = resolveQuery,
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

app :: App () Haxl
app = deriveApp rootResolver

runHaxlApp :: MapAPI a b => App e Haxl -> a -> IO b
runHaxlApp haxlApp input = do
  let stateStore = stateSet DeityState stateEmpty
  environment <- initEnv stateStore ()
  runHaxl environment (runApp haxlApp input)

httpEndpoint ::
  RoutePattern ->
  App () Haxl ->
  ScottyM ()
httpEndpoint route app' = do
  get route $ (isSchema *> raw (render app)) <|> raw httpPlayground
  post route $ raw =<< (liftIO . runHaxlApp app' =<< body)
