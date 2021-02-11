{-# LANGUAGE OverloadedStrings #-}

module Server.Scotty
  ( scottyServer,
  )
where

-- examples
import qualified Server.Haxl.API as Haxl
import Server.Utils
  ( startServer,
  )

scottyServer :: IO ()
scottyServer =
  startServer $
    Haxl.httpEndpoint "/" Haxl.app
