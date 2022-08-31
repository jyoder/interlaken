module Login where

import Crypto.BCrypt (validatePassword)
import Data.String.Conversions (cs)
import Database (scalar)
import Database.SQLite.Simple (
  Connection,
  Only (Only),
  Query (Query),
  query,
  query_,
 )
import qualified Layout
import Network.HTTP.Types (unauthorized401)
import qualified Path
import Protolude
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Types (AppContext (..), Environment)
import Web.Scotty (
  ActionM,
  html,
  liftAndCatchIO,
  param,
  redirect,
  status,
 )

new :: AppContext -> ActionM ()
new (AppContext{..}) = do
  maybeUserCount <- liftAndCatchIO $ userCount dbConnection
  putText $ "on login new page: " <> show maybeUserCount
  case maybeUserCount of
    Just 0 -> redirect Path.signUpNew
    _ -> html $ renderHtml $ renderPage environment Page{errorMessage = Nothing}

userCount :: Connection -> IO (Maybe Int)
userCount connection = query_ connection (Query "select count(*) from users") <&> scalar

create :: AppContext -> ActionM ()
create (AppContext{..}) = do
  email :: Text <- param "email"
  password :: Text <- param "password"

  maybeHashedPassword <- liftAndCatchIO $ loadHashedPassword dbConnection email
  let isPasswordValid = case maybeHashedPassword of
        Just hashedPassword -> validatePassword (cs hashedPassword) (cs password)
        Nothing -> False

  errorMessage <-
    if isPasswordValid
      then do
        _ <- redirect "/dashboard"
        pure Nothing
      else do
        status unauthorized401
        pure $ Just "Either the email or the password do not match our records. Please check whether your caps-lock key is on and try again."

  html $ renderHtml $ renderPage environment Page{..}

loadHashedPassword :: Connection -> Text -> IO (Maybe Text)
loadHashedPassword connection email =
  query
    connection
    (Query "select users.hashed_password from users where users.email = ?")
    (Only email)
    <&> scalar

newtype Page = Page {errorMessage :: Maybe Text}

renderPage :: Environment -> Page -> H.Html
renderPage environment page = Layout.render environment $ do
  H.section ! A.class_ "hero" $ do
    H.div ! A.class_ "hero-body" $ do
      H.div ! A.class_ "container" $ do
        H.div ! A.class_ "columns is-centered" $ do
          H.div ! A.class_ "column is-6" $ do
            H.div ! A.class_ "box" $ do
              H.div ! A.class_ "columns is-centered" $ do
                H.div ! A.class_ "column has-text-centered" $ do
                  H.h1 ! A.class_ "is-size-2 has-text-weight-light" $ "Interlaken"
                  renderErrorMessage page
              H.form ! A.action Path.loginCreate ! A.method "post" $ do
                H.h2 ! A.class_ "is-size-4 has-text-weight-light my-5" $ "Log in to your account"
                H.div ! A.class_ "field" $ do
                  H.label ! A.class_ "label" $ "Email"
                  H.input ! A.class_ "input" ! A.name "email" ! A.type_ "email" ! A.placeholder "e.g. alex@example.com"
                H.div ! A.class_ "field" $ do
                  H.label ! A.class_ "label" $ "Password"
                  H.input ! A.class_ "input" ! A.name "password" ! A.type_ "password" ! A.placeholder "**********"
                H.div ! A.class_ "columns is-vcentered mt-4" $ do
                  H.p ! A.class_ "column is-2" $ do
                    H.button ! A.class_ "button is-primary" ! A.type_ "submit" $ "Log In"
                  H.p ! A.class_ "column is-4" $ do
                    H.a ! A.class_ "has-text-primary-dark" ! A.href "#" $ "Help with Password?"

renderErrorMessage :: Page -> H.Html
renderErrorMessage Page{errorMessage = Just message} =
  H.div ! A.class_ "notification is-danger is-light mt-6 animate__animated animate__fadeIn" $
    H.toHtml message
renderErrorMessage Page{errorMessage = Nothing} = ""
