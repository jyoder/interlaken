{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Layout where

import Protolude
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Types

render :: Environment -> H.Html
render environment = H.docTypeHtml $ do
  head_ environment
  H.body $ do
    H.p "Hello World"
    startHotReload environment

head_ :: Environment -> H.Html
head_ Production = do
  H.head $ do
    H.title "App"
head_ Development = do
  H.head $ do
    H.title "App (Development)"
    H.script ! A.type_ "text/javascript" ! A.src "development/hot-reload.js" $ ""

startHotReload :: Environment -> H.Html
startHotReload Production = H.script ""
startHotReload Development = H.script "startHotReloadMonitor();"