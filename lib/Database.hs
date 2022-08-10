module Database where

import Database.SQLite.Simple (
  Connection,
  open,
 )
import Protolude

openConnection :: IO Connection
openConnection = open "db/data/database.sqlite3"
