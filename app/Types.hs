module Types (
  Environment (..),
  AppContext (..),
) where

import qualified Database.SQLite.Simple as SQLite

data AppContext = AppContext
  { environment :: Environment
  , dbConnection :: SQLite.Connection
  }

data Environment = Production | Development
