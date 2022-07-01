module Main where

import qualified Database.SQLite.Simple as SQLite
import Layout
import qualified Login
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Handler.Warp
import qualified Network.Wai.Handler.WebSockets as Handler.WebSockets
import qualified Network.Wai.Middleware.Gzip as Gzip
import qualified Network.Wai.Middleware.Static as Static
import qualified Network.WebSockets as WebSockets
import Protolude
import qualified Text.Blaze.Html.Renderer.Text as Renderer.Text
import Types
import qualified Web.Scotty as Scotty

main :: IO ()
main = do
  dbConnection <- SQLite.open "db/data/database.sqlite3"
  let appContext = AppContext{environment = Production, ..}
  webApp appContext >>= Handler.Warp.runSettings warpSettings

mainDevelopment :: IO ()
mainDevelopment = do
  dbConnection <- SQLite.open "db/data/database.sqlite3"
  let appContext = AppContext{environment = Development, ..}
  webApp appContext
    >>= Handler.Warp.runSettings warpSettings . withHotReload

webApp :: AppContext -> IO Wai.Application
webApp appContext@(AppContext{..}) =
  Static.staticPolicy (Static.addBase "static/")
    `fmap` Scotty.scottyApp
      ( do
          Scotty.middleware $ Gzip.gzip $ Gzip.def{Gzip.gzipFiles = Gzip.GzipCompress}
          Scotty.get "/" $ Scotty.redirect Login.newPath
          Scotty.get Login.newPath $ Login.new appContext
          Scotty.post Login.path $ Login.create appContext
          Scotty.get "hot_reload" $ Scotty.html $ Renderer.Text.renderHtml $ Layout.render environment ""
      )

warpSettings :: Handler.Warp.Settings
warpSettings = Handler.Warp.setPort 3000 Handler.Warp.defaultSettings

withHotReload :: Wai.Application -> Wai.Application
withHotReload = Handler.WebSockets.websocketsOr WebSockets.defaultConnectionOptions hotReloadApp

hotReloadApp :: WebSockets.ServerApp
hotReloadApp pending = do
  connection <- WebSockets.acceptRequest pending
  WebSockets.withPingThread connection 30 (pure ()) $ receive connection

receive :: WebSockets.Connection -> IO ()
receive connection = do
  (_ :: Text) <- WebSockets.receiveData connection
  pure ()
