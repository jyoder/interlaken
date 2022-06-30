{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Login where

import qualified Layout
import Protolude
import qualified Text.Blaze.Html.Renderer.Text as Renderer.Text
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Types
import qualified Web.Scotty as Scotty

new :: Environment -> Scotty.ActionM ()
new environment = do
  Scotty.html $
    Renderer.Text.renderHtml $
      Layout.render environment $ do
        H.section ! A.class_ "hero" $ do
          H.div ! A.class_ "hero-body" $ do
            H.div ! A.class_ "container" $ do
              H.div ! A.class_ "columns is-centered" $ do
                H.div ! A.class_ "column is-6" $ do
                  H.div ! A.class_ "box" $ do
                    H.div ! A.class_ "columns is-centered" $ do
                      H.div ! A.class_ "column has-text-centered" $ do
                        H.h1 ! A.class_ "is-size-2" $ "Interlaken"
                    H.form $ do
                      H.h2 ! A.class_ "is-size-4 has-text-weight-light mb-5" $ "Log in to your account"
                      H.div ! A.class_ "field" $ do
                        H.label ! A.class_ "label" $ "Email"
                        H.input ! A.class_ "input" ! A.type_ "email" ! A.placeholder "e.g. alex@example.com"
                      H.div ! A.class_ "field" $ do
                        H.label ! A.class_ "label" $ "Password"
                        H.input ! A.class_ "input" ! A.type_ "password" ! A.placeholder "**********"
                      H.div ! A.class_ "columns is-vcentered mt-5" $ do
                        H.p ! A.class_ "column is-2" $ do
                          H.button ! A.class_ "button is-primary" $ "Log In"
                        H.p ! A.class_ "column is-4" $ do
                          H.a ! A.class_ "has-text-primary" ! A.href "#" $ "Forgot Password?"
