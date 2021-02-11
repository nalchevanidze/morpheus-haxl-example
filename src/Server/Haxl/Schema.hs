{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Server.Haxl.Schema
  ( Deity (..),
    Query (..),
    DeityArguments (..),
    ID,
  )
where

import Data.Morpheus.Types
  ( GQLType (..),
  )
import Data.Text (Text)
import GHC.Generics (Generic)

type ID = Text

data Deity m = Deity
  { name :: m Text,
    power :: m (Maybe Text)
  }
  deriving
    ( Generic,
      GQLType
    )

newtype DeityArguments = DeityArguments {id :: ID}
  deriving
    ( Generic,
      GQLType
    )

data Query m = Query
  { deity :: DeityArguments -> m (Deity m),
    deities :: m [Deity m]
  }
  deriving
    ( Generic,
      GQLType
    )
