module Types (
  Environment (..),
  AppContext (..),
) where

import Database.SQLite.Simple (Connection)

data AppContext = AppContext
  { environment :: Environment
  , dbConnection :: Connection
  }

data Environment = Production | Development
