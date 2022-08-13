module Route where

import qualified Development
import qualified Login
import qualified Path
import Protolude
import qualified SignUp
import Types (AppContext)
import qualified Web.Scotty as Scotty

routes :: AppContext -> Scotty.ScottyM ()
routes appContext = do
  Scotty.get "/" $ Scotty.redirect Path.loginNew
  Scotty.get Path.loginNew $ Login.new appContext
  Scotty.post Path.loginCreate $ Login.create appContext

  Scotty.get Path.signUpNew $ SignUp.new appContext
  Scotty.post Path.signUpCreate $ SignUp.create appContext
  Scotty.post Path.signUpValidatePassword $ SignUp.validatePassword appContext

  Scotty.get Path.developmentHotReloadPing $ Development.hotReloadPing appContext
