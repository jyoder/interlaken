module Login where

import Data.Password.Bcrypt (PasswordCheck (PasswordCheckSuccess), checkPassword)
import Data.Password.Types (
  PasswordHash (PasswordHash),
  mkPassword,
 )
import Database ()
import Database.SQLite.Simple (
  Connection,
  FromRow (..),
  Only (Only),
  Query (Query),
  field,
  query,
  query_,
 )
import qualified Layout

import Network.HTTP.Types (unauthorized401)
import Protolude
import qualified SignUp
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

createPath :: IsString a => a
createPath = "/logins"

newPath :: IsString a => a
newPath = "/logins/new"

new :: AppContext -> ActionM ()
new (AppContext{..}) = do
  maybeUserCount <- liftAndCatchIO $ userCount dbConnection
  case maybeUserCount of
    Just (UserCount 0) -> redirect SignUp.newPath
    _ -> html $ renderHtml $ renderPage environment Page{errorMessage = Nothing}

userCount :: Connection -> IO (Maybe UserCount)
userCount connection = do
  query_
    connection
    (Query "select count(*) from users")
    <&> listToMaybe

newtype UserCount = UserCount Int

instance FromRow UserCount where
  fromRow = UserCount <$> field

create :: AppContext -> ActionM ()
create (AppContext{..}) = do
  email :: Text <- param "email"
  password :: Text <- param "password"

  maybeHashedPassword <- liftAndCatchIO $ loadHashedPassword dbConnection email
  let passwordCheck = case maybeHashedPassword of
        Just (HashedPassword hashedPassword) -> Just $ checkPassword (mkPassword password) (PasswordHash hashedPassword)
        Nothing -> Nothing

  errorMessage <- case passwordCheck of
    Just PasswordCheckSuccess -> do
      _ <- redirect "/dashboard"
      pure Nothing
    _ -> do
      status unauthorized401
      pure $ Just "Either the email or the password do not match our records. Please check whether your caps-lock key is on and try again."

  html $ renderHtml $ renderPage environment Page{..}

loadHashedPassword :: Connection -> Text -> IO (Maybe HashedPassword)
loadHashedPassword connection email = do
  query
    connection
    (Query "select users.hashed_password from users where users.email = ?")
    (Only email)
    <&> listToMaybe

newtype HashedPassword = HashedPassword Text

instance FromRow HashedPassword where
  fromRow = HashedPassword <$> field

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
              H.form ! A.action createPath ! A.method "post" $ do
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
