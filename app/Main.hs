module Main where

import Database (openConnection)
import Development (withHotReload)
import qualified Layout
import qualified Login
import Network.Wai (Application)
import Network.Wai.Handler.Warp (
  Settings,
  defaultSettings,
  runSettings,
  setPort,
 )
import Network.Wai.Middleware.Gzip (
  GzipFiles (GzipCompress),
  GzipSettings (gzipFiles),
  def,
  gzip,
 )
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Protolude
import qualified Route
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Types (
  AppContext (..),
  Environment (..),
 )
import Web.Scotty (middleware, scottyApp)

main :: IO ()
main = do
  dbConnection <- openConnection
  let appContext = AppContext{environment = Production, ..}
  webApp appContext >>= runSettings settings

mainDevelopment :: IO ()
mainDevelopment = do
  dbConnection <- openConnection
  let appContext = AppContext{environment = Development, ..}
  webApp appContext >>= runSettings settings . withHotReload

webApp :: AppContext -> IO Application
webApp appContext =
  staticPolicy (addBase "static/")
    `fmap` scottyApp
      ( do
          middleware $ gzip $ def{gzipFiles = GzipCompress}
          Route.routes appContext
      )

settings :: Settings
settings = setPort 3000 defaultSettings
