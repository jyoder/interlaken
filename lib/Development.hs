module Development where

import qualified Layout
import Network.Wai (Application)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (
  Connection,
  ServerApp,
  acceptRequest,
  defaultConnectionOptions,
  receiveData,
  withPingThread,
 )
import Protolude
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Types
import Web.Scotty (ActionM, html)

hotReloadPing :: AppContext -> ActionM ()
hotReloadPing (AppContext{..}) = html $ renderHtml $ Layout.render environment ""

withHotReload :: Application -> Application
withHotReload = websocketsOr defaultConnectionOptions hotReloadApp

hotReloadApp :: ServerApp
hotReloadApp pending = do
  connection <- acceptRequest pending
  withPingThread connection 30 (pure ()) $ receive connection

receive :: Connection -> IO ()
receive connection = do
  (_ :: Text) <- receiveData connection
  pure ()
