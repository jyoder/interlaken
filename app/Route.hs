module Route where

import qualified Development
import qualified Login
import Protolude
import Types (AppContext)
import Web.Scotty as Scotty

routes :: AppContext -> Scotty.ScottyM ()
routes appContext = do
  Scotty.get "/" $ Scotty.redirect Login.newPath
  Scotty.get Login.newPath $ Login.new appContext
  Scotty.post Login.path $ Login.create appContext
  Scotty.get Development.hotReloadPingPath $ Development.hotReloadPing appContext