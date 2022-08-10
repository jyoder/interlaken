module SignUp where

import Data.Text (elem, find, length)
import Database.SQLite.Simple (
  Connection,
  FromRow (..),
  Query (Query),
  field,
  query_,
 )
import qualified Layout
import Parser (
  emailAddress,
  formatEmailAddressFeedback,
  parseRequiredFormInput,
 )
import Protolude hiding (elem, find, length)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (textValue, toHtml, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Types (AppContext (..), Environment)
import Web.Scotty (
  ActionM,
  html,
  liftAndCatchIO,
  param,
 )

data Page = Page
  { email :: Text
  , maybeEmailFeedback :: Maybe Text
  , password :: Text
  , maybePasswordFeedbackSummary :: Maybe Text
  , passwordFeedback :: PasswordFeedback
  }

data PasswordFeedback = PasswordFeedback
  { minimumLength :: RuleFeedback
  , maximumLength :: RuleFeedback
  , lowercase :: RuleFeedback
  , uppercase :: RuleFeedback
  , number :: RuleFeedback
  , specialCharacter :: RuleFeedback
  }
  deriving (Eq)

data RuleFeedback
  = Undetermined
  | Fulfilled
  | Unfulfilled
  deriving (Eq)

newPath :: IsString a => a
newPath = "/sign_ups/new"

createPath :: IsString a => a
createPath = "/sign_ups"

validatePasswordPath :: IsString a => a
validatePasswordPath = "/sign_ups/validate_password"

new :: AppContext -> ActionM ()
new AppContext{..} = do
  maybeUserCount <- liftAndCatchIO $ userCount dbConnection
  case maybeUserCount of
    Just (UserCount 0) -> do
      html $ renderHtml $ renderPage environment makeEmptyPage
    _ -> pure ()

create :: AppContext -> ActionM ()
create AppContext{..} = do
  email1 :: Text <- param "email"
  password :: Text <- param "password"

  if isValidPassword password
    then do
      maybeUserCount <- liftAndCatchIO $ userCount dbConnection
      case maybeUserCount of
        Just (UserCount 0) -> do
          html $ renderHtml $ renderPage environment makeEmptyPage
        _ -> pure ()
    else html $ renderHtml $ renderPage environment $ makePage email1 password

validatePassword :: AppContext -> ActionM ()
validatePassword AppContext{} = do
  password :: Text <- param "password"
  html $ renderHtml $ renderPasswordRequirements $ makePasswordFeedback password

makeEmptyPage :: Page
makeEmptyPage =
  Page
    { email = ""
    , maybeEmailFeedback = Nothing
    , password = ""
    , maybePasswordFeedbackSummary = Nothing
    , passwordFeedback = undeterminedPasswordFeedback
    }

makePage :: Text -> Text -> Page
makePage email password =
  Page
    { maybeEmailFeedback = makeEmailFeedback email
    , maybePasswordFeedbackSummary = makePasswordFeedbackSummary password
    , passwordFeedback = makePasswordFeedback password
    , ..
    }

makeEmailFeedback :: Text -> Maybe Text
makeEmailFeedback email = do
  case parseRequiredFormInput emailAddress formatEmailAddressFeedback email of
    Left errorMessage -> Just errorMessage
    Right _ -> Nothing

makePasswordFeedbackSummary :: Text -> Maybe Text
makePasswordFeedbackSummary password =
  if isValidPassword password
    then Nothing
    else Just "does not meet all requirements given below"

makePasswordFeedback :: Text -> PasswordFeedback
makePasswordFeedback password =
  PasswordFeedback
    { minimumLength = minimumLengthFeedback password
    , maximumLength = maximumLengthFeedback password
    , lowercase = lowercaseFeedback password
    , uppercase = uppercaseFeedback password
    , number = numberFeedback password
    , specialCharacter = specialCharacterFeedback password
    }

undeterminedPasswordFeedback :: PasswordFeedback
undeterminedPasswordFeedback =
  PasswordFeedback
    { minimumLength = Undetermined
    , maximumLength = Undetermined
    , lowercase = Undetermined
    , uppercase = Undetermined
    , number = Undetermined
    , specialCharacter = Undetermined
    }

minimumLengthFeedback :: Text -> RuleFeedback
minimumLengthFeedback password =
  if length password >= minimumPasswordLength then Fulfilled else Unfulfilled

maximumLengthFeedback :: Text -> RuleFeedback
maximumLengthFeedback password =
  if length password <= maximumPasswordLength then Fulfilled else Unfulfilled

lowercaseFeedback :: Text -> RuleFeedback
lowercaseFeedback password =
  if isJust $ find isLower password then Fulfilled else Unfulfilled

uppercaseFeedback :: Text -> RuleFeedback
uppercaseFeedback password =
  if isJust $ find isUpper password then Fulfilled else Unfulfilled

numberFeedback :: Text -> RuleFeedback
numberFeedback password =
  if isJust $ find isDigit password then Fulfilled else Unfulfilled

specialCharacterFeedback :: Text -> RuleFeedback
specialCharacterFeedback password =
  if isJust $ find (`elem` specialCharacters) password then Fulfilled else Unfulfilled

isValidPassword :: Text -> Bool
isValidPassword password =
  let PasswordFeedback{..} = makePasswordFeedback password
   in (minimumLength == Fulfilled)
        && (maximumLength == Fulfilled)
        && (lowercase == Fulfilled)
        && (uppercase == Fulfilled)
        && (number == Fulfilled)
        && (specialCharacter == Fulfilled)

renderPage :: Environment -> Page -> H.Html
renderPage environment (Page{..}) = Layout.render environment $ do
  H.section ! A.class_ "hero" $ do
    H.div ! A.class_ "hero-body" $ do
      H.div ! A.class_ "container" $ do
        H.div ! A.class_ "columns is-centered" $ do
          H.div ! A.class_ "column is-6" $ do
            H.div ! A.class_ "box" $ do
              H.div ! A.class_ "columns is-centered" $ do
                H.div ! A.class_ "column has-text-centered" $ do
                  H.h1 ! A.class_ "is-size-2 has-text-weight-light" $ "Interlaken"
                  H.div ! A.class_ "notification is-info is-light mt-6 animate__animated animate__fadeIn" $
                    "There are no users in the system. To create the first administrator account, please provide an email address and password for the new account."
              H.form ! A.action createPath ! A.method "post" $ do
                H.h2 ! A.class_ "is-size-4 has-text-weight-light my-5" $ "Create an administrator account"
                H.div ! A.class_ "field" $ do
                  H.label ! A.class_ "label" $ "Email"
                  renderEmailInput email maybeEmailFeedback
                H.div ! A.class_ "field" $ do
                  H.label ! A.class_ "label" $ "Password"
                  renderPasswordInput password maybePasswordFeedbackSummary
                H.div ! A.class_ "columns mt-4 mb-0" $ do
                  H.p ! A.class_ "column" $ do
                    "Password requirements"
                H.div ! A.class_ "columns" $ do
                  renderPasswordRequirements passwordFeedback
                H.div ! A.class_ "columns mt-4" $ do
                  H.p ! A.class_ "column is-2" $ do
                    H.button ! A.class_ "button is-primary" ! A.type_ "submit" $ "Create Account"

renderPasswordInput :: Text -> Maybe Text -> H.Html
renderPasswordInput password maybePasswordFeedbackSummary = do
  H.input
    ! A.class_ class_
    ! A.name "password"
    ! A.type_ "password"
    ! A.value (textValue password)
    ! H.customAttribute "hx-post" validatePasswordPath
    ! H.customAttribute "hx-trigger" "input delay:100ms"
    ! H.customAttribute "hx-target" "#password-requirements"
    ! H.customAttribute "hx-swap" "outerHTML"
    ! A.placeholder "**********"
  renderPasswordInputFeedbackSummary
 where
  class_ = if isJust maybePasswordFeedbackSummary then "input is-danger" else "input"
  renderPasswordInputFeedbackSummary =
    maybe "" ((H.p ! A.class_ "help is-danger") . H.toHtml) maybePasswordFeedbackSummary

renderEmailInput :: Text -> Maybe Text -> H.Html
renderEmailInput email maybeEmailFeedback = do
  H.input
    ! A.class_ class_
    ! A.name "email"
    ! A.type_ "FIXME"
    ! A.value (textValue email)
    ! A.placeholder "e.g. alex@example.com"
  renderEmailInputFeedback
 where
  class_ = if isJust maybeEmailFeedback then "input is-danger" else "input"
  renderEmailInputFeedback =
    maybe "" ((H.p ! A.class_ "help is-danger") . H.toHtml) maybeEmailFeedback

renderPasswordRequirements :: PasswordFeedback -> H.Html
renderPasswordRequirements PasswordFeedback{..} =
  H.ul ! A.id "password-requirements" ! A.class_ "column is-size-7" $ do
    H.li $ do
      "At least " <> toHtml minimumPasswordLength <> " characters long "
      ruleFeedbackIcon minimumLength
    H.li $ do
      "At most " <> toHtml maximumPasswordLength <> " characters long"
      ruleFeedbackIcon maximumLength
    H.li $ do
      "At least one lowercase character"
      ruleFeedbackIcon lowercase
    H.li $ do
      "At least one uppercase character "
      ruleFeedbackIcon uppercase
    H.li $ do
      "At least one number "
      ruleFeedbackIcon number
    H.li $ do
      "At least one special character (e.g. !@#$?) "
      ruleFeedbackIcon specialCharacter

ruleFeedbackIcon :: RuleFeedback -> H.Html
ruleFeedbackIcon Undetermined = ""
ruleFeedbackIcon Fulfilled = H.span ! A.class_ "icon-text has-text-success mt-1 ml-1" $ H.i ! A.class_ "fas fa-check" $ ""
ruleFeedbackIcon Unfulfilled = H.span ! A.class_ "icon-text has-text-danger mt-1 ml-1" $ H.i ! A.class_ "fas fa-times" $ ""

userCount :: Connection -> IO (Maybe UserCount)
userCount connection = do
  query_
    connection
    (Query "select count(*) from users")
    <&> listToMaybe

newtype UserCount = UserCount Int

instance FromRow UserCount where
  fromRow = UserCount <$> field

minimumPasswordLength :: Int
minimumPasswordLength = 12

maximumPasswordLength :: Int
maximumPasswordLength = 50

specialCharacters :: Text
specialCharacters = "`~!@#$%^&*()-_+=[{]}\\|;:'\",<.>/?"
