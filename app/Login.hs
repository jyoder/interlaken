module Login where

import Database.SQLite.Simple (
  FromRow (..),
  Only (Only),
  Query (Query),
  field,
  query,
 )
import qualified Layout
import Network.HTTP.Types (unauthorized401)
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

path :: IsString a => a
path = "/logins"

newPath :: IsString a => a
newPath = "/logins/new"

new :: AppContext -> ActionM ()
new (AppContext{..}) =
  html $ renderHtml $ renderPage environment Page{errorMessage = Nothing}

create :: AppContext -> ActionM ()
create (AppContext{..}) = do
  email :: Text <- param "email"
  password :: Text <- param "password"

  passwords <- liftAndCatchIO $ do
    query dbConnection (Query "select users.hashed_password from users where users.email = ?") (Only email) :: IO [PasswordRow]

  let loggedIn = case listToMaybe passwords of
        (Just (PasswordRow correctPassword)) -> password == correctPassword
        Nothing -> False

  let errorMessage =
        if loggedIn
          then Nothing
          else Just "Either the email or the password do not match our records. Please check whether your caps-lock  key is on and try again."

  if loggedIn
    then redirect "/dashboard"
    else status unauthorized401

  html $ renderHtml $ renderPage environment Page{..}

newtype PasswordRow = PasswordRow Text deriving (Show)

instance FromRow PasswordRow where
  fromRow = PasswordRow <$> field

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
              H.form ! A.action path ! A.method "post" $ do
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
