module Parser (
  parseRequiredFormInput,
  parseOptionalFormInput,
  emailAddress,
  formatDefaultFeedback,
  formatEmailAddressFeedback,
) where

import Control.Monad.Fail (fail)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Email (EmailAddress (..))
import Protolude hiding (some)
import qualified Protolude as P
import Text.Megaparsec (
  ErrorFancy (..),
  ErrorItem (..),
  ParseError (..),
  ParseErrorBundle (..),
  Parsec,
  eof,
  parse,
  satisfy,
  sepBy1,
  some,
  (<?>),
 )
import Text.Megaparsec.Char (
  char,
  space,
  string,
 )

type Parser = Parsec Text Text

data SimplifiedParseError = SimplifiedParseError
  { general :: Maybe Text
  , unexpectedToken :: Maybe Text
  , expectedLabels :: [Text]
  }

parseRequiredFormInput :: Parser a -> (ParseErrorBundle Text Text -> Text) -> Text -> Either Text a
parseRequiredFormInput parser formatFeedback input =
  case parse ((space :: Parser ()) *> eof) "" input of
    Left _ -> case parse (formInput parser) "" input of
      Left errorBundle -> Left $ formatFeedback errorBundle
      Right result -> Right result
    Right _ -> Left "can't be blank"

parseOptionalFormInput :: Parser a -> (ParseErrorBundle Text Text -> Text) -> Text -> Either Text (Maybe a)
parseOptionalFormInput parser formatFeedback input =
  case parse ((space :: Parser ()) *> eof) "" input of
    Left _ -> case parse (formInput parser) "" input of
      Left errorBundle -> Left $ formatFeedback errorBundle
      Right result -> Right $ Just result
    Right _ -> Right Nothing

formatDefaultFeedback :: Text -> ParseErrorBundle Text Text -> Text
formatDefaultFeedback message _ = message

emailAddress :: Parser EmailAddress
emailAddress = do
  localPart <- emailAddressLocalPart <?> "localPart"
  _ <- string "@" <?> "at"
  domainName' <- domainNameP <?> "domainName"
  pure EmailAddress{localPart = localPart, domainName = domainName'}

formatEmailAddressFeedback :: ParseErrorBundle Text Text -> Text
formatEmailAddressFeedback errorBundle =
  let SimplifiedParseError{..} = simplifiedParseErrors errorBundle
   in case general of
        Just message -> message
        Nothing -> case unexpectedToken of
          Just tok ->
            if tok == "@"
              then
                if hasLocalPartLabel expectedLabels
                  then "can't begin with '@'"
                  else "can't contain multiple '@'s"
              else "can't contain '" <> tok <> "'"
          Nothing ->
            if hasAtLabel expectedLabels
              then "must have an '@'"
              else
                if hasDomainNameLabel expectedLabels
                  then "can't end with '@'"
                  else "is not a valid email address"
 where
  hasLocalPartLabel labels = isJust $ find (== "localPart") labels
  hasAtLabel labels = isJust $ find (== "at") labels
  hasDomainNameLabel labels = isJust $ find (== "domainName") labels

formInput :: Parser a -> Parser a
formInput parser = ignoreSurroundingWhitespace parser <* eof

ignoreSurroundingWhitespace :: Parser a -> Parser a
ignoreSurroundingWhitespace parser = space *> parser <* space

emailAddressLocalPart :: Parser Text
emailAddressLocalPart = do
  result <- T.pack . P.intercalate "." <$> sepBy1 segment (char '.')
  if T.length result <= 64
    then pure result
    else fail "portion before the '@' can't be longer than 64 characters"
 where
  segment = some letterOrDigitOrSpecial
  letterOrDigitOrSpecial = satisfy isLetterOrDigitOrSpecial
  isLetterOrDigitOrSpecial c = isLetterOrDigit c || isSpecialCharacter c
  isSpecialCharacter c = c `T.elem` "!#$%&'*+-/=?^_`{|}~"
  isLetterOrDigit c = isLetter c || isDigit c

domainNameP :: Parser Text
domainNameP = T.intercalate "." <$> sepBy1 dnsLabel (char '.')

dnsLabel :: Parser Text
dnsLabel = do
  result <- T.pack . P.intercalate "-" <$> sepBy1 (some letterOrDigit) (char '-')
  if T.length result <= 63
    then pure result
    else fail "segments after the '@' can't be longer than 63 characters"
 where
  letterOrDigit = satisfy isLetterOrDigit
  isLetterOrDigit c = isLetter c || isDigit c

simplifiedParseErrors :: ParseErrorBundle Text Text -> SimplifiedParseError
simplifiedParseErrors (ParseErrorBundle bundleErrors _) =
  SimplifiedParseError
    { general = head $ failErrors $ NE.toList bundleErrors
    , unexpectedToken = head $ unexpectedTokenErrors $ NE.toList bundleErrors
    , expectedLabels = expectedLabelErrors $ NE.toList bundleErrors
    }

failErrors :: [ParseError Text Text] -> [Text]
failErrors parseErrors = concat $ failMessages <$> parseErrors

failMessages :: ParseError Text Text -> [Text]
failMessages (FancyError _ errors) = catMaybes $ failMessage <$> toList errors
failMessages _ = []

failMessage :: ErrorFancy Text -> Maybe Text
failMessage (ErrorFail message) = Just $ T.pack message
failMessage _ = Nothing

unexpectedTokenErrors :: [ParseError Text Text] -> [Text]
unexpectedTokenErrors parseErrors = concat $ unexpectedTokenMessages <$> parseErrors

unexpectedTokenMessages :: ParseError Text Text -> [Text]
unexpectedTokenMessages (TrivialError _ (Just (Tokens tokens)) _) = T.singleton <$> toList tokens
unexpectedTokenMessages _ = []

expectedLabelErrors :: [ParseError Text Text] -> [Text]
expectedLabelErrors parseErrors = concat $ expectedLabelMessages <$> parseErrors

expectedLabelMessages :: ParseError Text Text -> [Text]
expectedLabelMessages (TrivialError _ _ expected) = catMaybes $ expectedLabelMessage <$> toList expected
expectedLabelMessages _ = []

expectedLabelMessage :: ErrorItem Char -> Maybe Text
expectedLabelMessage (Label label) = Just . T.pack $ toList label
expectedLabelMessage _ = Nothing
