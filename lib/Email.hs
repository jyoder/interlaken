module Email where

import Protolude

data EmailAddress = EmailAddress
  { localPart :: Text
  , domainName :: Text
  }
  deriving (Ord, Eq)

formatEmailAddress :: EmailAddress -> Text
formatEmailAddress (EmailAddress{..}) = localPart <> "@" <> domainName
