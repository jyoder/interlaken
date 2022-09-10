module Dashboard where

import qualified Layout
import Protolude
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import Types (AppContext (..), Environment)
import Web.Scotty (
  ActionM,
  html,
 )

show :: AppContext -> ActionM ()
show (AppContext{..}) = do
  html $ renderHtml $ renderPage environment Page{errorMessage = Nothing}

newtype Page = Page {errorMessage :: Maybe Text}

renderPage :: Environment -> Page -> H.Html
renderPage environment _ = Layout.render environment $ do
  H.h1 "Hello World"
