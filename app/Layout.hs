module Layout where

import Protolude
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Types

render :: Environment -> H.Html -> H.Html
render environment pageContent = H.docTypeHtml $ do
  head_ environment
  H.body $ do
    H.section ! A.class_ "section" $
      H.div ! A.class_ "container" $
        pageContent
    startHotReload environment

head_ :: Environment -> H.Html
head_ environment = do
  H.head $ do
    metas
    title environment
    links
    scripts environment

metas :: H.Html
metas = do
  H.meta ! A.charset "utf-8"
  H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"

title :: Environment -> H.Html
title Production = H.title "App"
title Development = H.title "App (Development)"

links :: H.Html
links = do
  H.link ! A.rel "stylesheet" ! A.href "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"
  H.link ! A.rel "stylesheet" ! A.href "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"

scripts :: Environment -> H.Html
scripts = hotReload

hotReload :: Environment -> H.Html
hotReload Production = H.script ""
hotReload Development = H.script ! A.type_ "text/javascript" ! A.src "/development/hot-reload.js" $ ""

startHotReload :: Environment -> H.Html
startHotReload Production = H.script ""
startHotReload Development = H.script "startHotReloadMonitor();"
