module ParserSpec where

import qualified Data.Set as Set
import Email (EmailAddress (EmailAddress))
import Parser (emailAddress)
import Protolude
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec

spec :: Spec
spec = do
  describe "emailParser" $ do
    it "parses a simple email" $ do
      parse emailAddress "" "simple@example.com" `shouldBe` Right (EmailAddress "simple" "example.com")

    it "parses a very common email" $ do
      parse emailAddress "" "very.common@example.com" `shouldBe` Right (EmailAddress "very.common" "example.com")

    it "parses a more complex email" $ do
      parse emailAddress "" "disposable.style.email.with+symbol@example.com" `shouldBe` Right (EmailAddress "disposable.style.email.with+symbol" "example.com")

    it "parses an email with a hyphen" $ do
      parse emailAddress "" "other.email-with-hyphen@example.com" `shouldBe` Right (EmailAddress "other.email-with-hyphen" "example.com")

    it "parses an email with multiple hyphens" $ do
      parse emailAddress "" "fully-qualified-domain@example.com" `shouldBe` Right (EmailAddress "fully-qualified-domain" "example.com")

    it "parses an email with tags" $ do
      parse emailAddress "" "user.name+tag+sorting@example.com" `shouldBe` Right (EmailAddress "user.name+tag+sorting" "example.com")

    it "parses an email with a one letter local part" $ do
      parse emailAddress "" "x@example.com" `shouldBe` Right (EmailAddress "x" "example.com")

    it "parses a strange email" $ do
      parse emailAddress "" "example-indeed@strange-example.com" `shouldBe` Right (EmailAddress "example-indeed" "strange-example.com")

    it "parses an email with no top level domain" $ do
      parse emailAddress "" "admin@mailserver1" `shouldBe` Right (EmailAddress "admin" "mailserver1")

    it "parses an email with a slash in the local part" $ do
      parse emailAddress "" "test/test@test.com" `shouldBe` Right (EmailAddress "test/test" "test.com")

    it "parses a bangified host route" $ do
      parse emailAddress "" "mailhost!username@example.org" `shouldBe` Right (EmailAddress "mailhost!username" "example.org")

    it "parses an escaped mail route" $ do
      parse emailAddress "" "user%example.com@example.org" `shouldBe` Right (EmailAddress "user%example.com" "example.org")

    it "parses an email with a local part ending in a non-alphanumeric character" $ do
      parse emailAddress "" "user-@example.org" `shouldBe` Right (EmailAddress "user-" "example.org")

    it "parses an email with dots in the local part" $ do
      parse emailAddress "" "a.b.c@thing.com" `shouldBe` Right (EmailAddress "a.b.c" "thing.com")

    it "parses an email with special characters in the local part" $ do
      parse emailAddress "" "!#$%&'*+-/=?^_`{|}~@thing.com" `shouldBe` Right (EmailAddress "!#$%&'*+-/=?^_`{|}~" "thing.com")

    it "parses an email with special characters in the local part" $ do
      parse emailAddress "" "!#$%&'*+-/=?^_`{|}~@thing.com" `shouldBe` Right (EmailAddress "!#$%&'*+-/=?^_`{|}~" "thing.com")

    it "parses an email with hyphens in the domain name" $ do
      parse emailAddress "" "a@b.b-o-m.co-m" `shouldBe` Right (EmailAddress "a" "b.b-o-m.co-m")

    it "parses an email which is a subset of a larger invalid email string due to multiple @ character" $ do
      parse emailAddress "" "a@b@c@d.com" `shouldBe` Right (EmailAddress "a" "b")

    it "parses an email which is a subset of a larger invalid email string due to an _ character" $ do
      parse emailAddress "" "hello@worl_d.com" `shouldBe` Right (EmailAddress "hello" "worl")

    it "rejects an email whose local part starts with a dot" $ do
      parse emailAddress "" ".a.b.c@thing.com"
        `shouldBe` Left
          ( ParseErrorBundle
              { bundleErrors = TrivialError 0 (Just (Tokens ('.' :| ""))) (Set.fromList [Label ('l' :| "ocalPart")]) :| []
              , bundlePosState =
                  PosState
                    { pstateInput = ".a.b.c@thing.com"
                    , pstateOffset = 0
                    , pstateSourcePos = SourcePos{sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1}
                    , pstateTabWidth = mkPos 8
                    , pstateLinePrefix = ""
                    }
              }
          )

    it "rejects an email whose local part ends with a dot" $ do
      parse emailAddress "" "a.b.c.@thing.com"
        `shouldBe` Left
          ( ParseErrorBundle
              { bundleErrors = TrivialError 6 (Just (Tokens ('@' :| ""))) (Set.fromList []) :| []
              , bundlePosState =
                  PosState
                    { pstateInput = "a.b.c.@thing.com"
                    , pstateOffset = 0
                    , pstateSourcePos = SourcePos{sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1}
                    , pstateTabWidth = mkPos 8
                    , pstateLinePrefix = ""
                    }
              }
          )

    it "rejects an email whose domain name starts with a dot" $ do
      parse emailAddress "" "a.b.c@.thing.com"
        `shouldBe` Left
          ( ParseErrorBundle
              { bundleErrors = TrivialError 6 (Just (Tokens ('.' :| ""))) (Set.fromList [Label ('d' :| "omainName")]) :| []
              , bundlePosState =
                  PosState
                    { pstateInput = "a.b.c@.thing.com"
                    , pstateOffset = 0
                    , pstateSourcePos = SourcePos{sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1}
                    , pstateTabWidth = mkPos 8
                    , pstateLinePrefix = ""
                    }
              }
          )

    it "rejects an email whose domain name ends with a dot" $ do
      parse emailAddress "" "a.b.c@thing.com."
        `shouldBe` Left
          ( ParseErrorBundle
              { bundleErrors = TrivialError 16 (Just EndOfInput) (Set.fromList []) :| []
              , bundlePosState =
                  PosState
                    { pstateInput = "a.b.c@thing.com."
                    , pstateOffset = 0
                    , pstateSourcePos = SourcePos{sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1}
                    , pstateTabWidth = mkPos 8
                    , pstateLinePrefix = ""
                    }
              }
          )

    it "rejects emails with spaces between quotes" $ do
      parse emailAddress "" "\" \"@example.org"
        `shouldBe` Left
          ( ParseErrorBundle
              { bundleErrors = TrivialError 0 (Just (Tokens ('"' :| ""))) (Set.fromList [Label ('l' :| "ocalPart")]) :| []
              , bundlePosState =
                  PosState
                    { pstateInput = "\" \"@example.org"
                    , pstateOffset = 0
                    , pstateSourcePos = SourcePos{sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1}
                    , pstateTabWidth = mkPos 8
                    , pstateLinePrefix = ""
                    }
              }
          )

    it "rejects emails with double-quoted dots" $ do
      parse emailAddress "" "\"john..doe\"@example.org"
        `shouldBe` Left
          ( ParseErrorBundle
              { bundleErrors = TrivialError 0 (Just (Tokens ('"' :| ""))) (Set.fromList [Label ('l' :| "ocalPart")]) :| []
              , bundlePosState =
                  PosState
                    { pstateInput = "\"john..doe\"@example.org"
                    , pstateOffset = 0
                    , pstateSourcePos = SourcePos{sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1}
                    , pstateTabWidth = mkPos 8
                    , pstateLinePrefix = ""
                    }
              }
          )

    it "rejects emails with parenthesis" $ do
      parse emailAddress "" "very.(),:;<>[]\".VERY.\"very@\\ \"very\".unusual\"@strange.example.com"
        `shouldBe` Left
          ( ParseErrorBundle
              { bundleErrors = TrivialError 5 (Just (Tokens ('(' :| ""))) (Set.fromList []) :| []
              , bundlePosState =
                  PosState
                    { pstateInput = "very.(),:;<>[]\".VERY.\"very@\\ \"very\".unusual\"@strange.example.com"
                    , pstateOffset = 0
                    , pstateSourcePos = SourcePos{sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1}
                    , pstateTabWidth = mkPos 8
                    , pstateLinePrefix = ""
                    }
              }
          )

    it "rejects emails with an IP address instead of a domain name" $ do
      parse emailAddress "" "postmaster@[123.123.123.123]"
        `shouldBe` Left
          ( ParseErrorBundle
              { bundleErrors = TrivialError 11 (Just (Tokens ('[' :| ""))) (Set.fromList [Label ('d' :| "omainName")]) :| []
              , bundlePosState =
                  PosState
                    { pstateInput = "postmaster@[123.123.123.123]"
                    , pstateOffset = 0
                    , pstateSourcePos = SourcePos{sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1}
                    , pstateTabWidth = mkPos 8
                    , pstateLinePrefix = ""
                    }
              }
          )

    it "rejects emails with an IPv6 address instead of a domain name" $ do
      parse emailAddress "" "postmaster@[IPv6:2001:0db8:85a3:0000:0000:8a2e:0370:7334]"
        `shouldBe` Left
          ( ParseErrorBundle
              { bundleErrors = TrivialError 11 (Just (Tokens ('[' :| ""))) (Set.fromList [Label ('d' :| "omainName")]) :| []
              , bundlePosState =
                  PosState
                    { pstateInput = "postmaster@[IPv6:2001:0db8:85a3:0000:0000:8a2e:0370:7334]"
                    , pstateOffset = 0
                    , pstateSourcePos = SourcePos{sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1}
                    , pstateTabWidth = mkPos 8
                    , pstateLinePrefix = ""
                    }
              }
          )

    it "rejects emails with no @ character" $ do
      parse emailAddress "" "Abc.example.com"
        `shouldBe` Left
          ( ParseErrorBundle
              { bundleErrors = TrivialError 15 (Just EndOfInput) (Set.fromList [Tokens ('.' :| ""), Label ('a' :| "t")]) :| []
              , bundlePosState =
                  PosState
                    { pstateInput = "Abc.example.com"
                    , pstateOffset = 0
                    , pstateSourcePos = SourcePos{sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1}
                    , pstateTabWidth = mkPos 8
                    , pstateLinePrefix = ""
                    }
              }
          )

    it "rejects emails with an invalid special character" $ do
      parse emailAddress "" "bill,wilbur@example.com"
        `shouldBe` Left
          ( ParseErrorBundle
              { bundleErrors = TrivialError 4 (Just (Tokens (',' :| ""))) (Set.fromList [Tokens ('.' :| ""), Label ('a' :| "t")]) :| []
              , bundlePosState =
                  PosState
                    { pstateInput = "bill,wilbur@example.com"
                    , pstateOffset = 0
                    , pstateSourcePos = SourcePos{sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1}
                    , pstateTabWidth = mkPos 8
                    , pstateLinePrefix = ""
                    }
              }
          )

    it "rejects emails with a local part longer than 64 characters" $ do
      parse emailAddress "" "1234567890123456789012345678901234567890123456789012345678901234+x@example.com"
        `shouldBe` Left
          ( ParseErrorBundle
              { bundleErrors = FancyError 66 (Set.fromList [ErrorFail "portion before the '@' can't be longer than 64 characters"]) :| []
              , bundlePosState =
                  PosState
                    { pstateInput = "1234567890123456789012345678901234567890123456789012345678901234+x@example.com"
                    , pstateOffset = 0
                    , pstateSourcePos = SourcePos{sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1}
                    , pstateTabWidth = mkPos 8
                    , pstateLinePrefix = ""
                    }
              }
          )

    it "rejects emails with a domain name labels longer than 63 characters" $ do
      parse emailAddress "" "x@123456789012345678901234567890123456789012345678901234567890123x"
        `shouldBe` Left
          ( ParseErrorBundle
              { bundleErrors = FancyError 66 (Set.fromList [ErrorFail "segments after the '@' can't be longer than 63 characters"]) :| []
              , bundlePosState =
                  PosState
                    { pstateInput = "x@123456789012345678901234567890123456789012345678901234567890123x"
                    , pstateOffset = 0
                    , pstateSourcePos = SourcePos{sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1}
                    , pstateTabWidth = mkPos 8
                    , pstateLinePrefix = ""
                    }
              }
          )
