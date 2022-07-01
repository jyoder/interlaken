module Main where

import Database.SQLite.Simple (open)
import qualified Layout
import qualified Login
import Network.Wai (Application)
import Network.Wai.Handler.Warp (
  Settings,
  defaultSettings,
  runSettings,
  setPort,
 )
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Middleware.Gzip (
  GzipFiles (GzipCompress),
  GzipSettings (gzipFiles),
  def,
  gzip,
 )
import Network.Wai.Middleware.Static (addBase, staticPolicy)
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
import Types (
  AppContext (..),
  Environment (Development, Production),
 )
import qualified Web.Scotty as Scotty

main :: IO ()
main = do
  dbConnection <- open "db/data/database.sqlite3"
  let appContext = AppContext{environment = Production, ..}
  webApp appContext >>= runSettings warpSettings

mainDevelopment :: IO ()
mainDevelopment = do
  dbConnection <- open "db/data/database.sqlite3"
  let appContext = AppContext{environment = Development, ..}
  webApp appContext
    >>= runSettings warpSettings . withHotReload

webApp :: AppContext -> IO Application
webApp appContext@(AppContext{..}) =
  staticPolicy (addBase "static/")
    `fmap` Scotty.scottyApp
      ( do
          Scotty.middleware $ gzip $ def{gzipFiles = GzipCompress}
          Scotty.get "/" $ Scotty.redirect Login.newPath
          Scotty.get Login.newPath $ Login.new appContext
          Scotty.post Login.path $ Login.create appContext
          Scotty.get "hot_reload" $ Scotty.html $ renderHtml $ Layout.render environment ""
      )

warpSettings :: Settings
warpSettings = setPort 3000 defaultSettings

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
