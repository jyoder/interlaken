module Main where

import Protolude (IO)
import qualified Spec
import Test.Hspec.Runner (hspec)

main :: IO ()
main = hspec Spec.spec