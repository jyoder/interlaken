module Database where

import Database.SQLite.Simple (
  Connection,
  Only (fromOnly),
  open,
 )
import Protolude

openConnection :: IO Connection
openConnection = open "db/data/database.sqlite3"

scalar :: [Only a] -> Maybe a
scalar xs = fromOnly <$> listToMaybe xs
