module Route where

import qualified Development
import qualified Login
import Protolude
import qualified SignUp
import Types (AppContext)
import qualified Web.Scotty as Scotty

routes :: AppContext -> Scotty.ScottyM ()
routes appContext = do
  Scotty.get "/" $ Scotty.redirect Login.newPath
  Scotty.get Login.newPath $ Login.new appContext
  Scotty.post Login.createPath $ Login.create appContext

  Scotty.get SignUp.newPath $ SignUp.new appContext
  Scotty.post SignUp.createPath $ SignUp.create appContext
  Scotty.post SignUp.validatePasswordPath $ SignUp.validatePassword appContext

  Scotty.get Development.hotReloadPingPath $ Development.hotReloadPing appContext
