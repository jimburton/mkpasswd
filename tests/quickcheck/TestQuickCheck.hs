module Main where

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import MkPasswd.MkPasswd (Flag(Length), mkPasswd)

prop_defaultLength :: Property
prop_defaultLength = monadicIO $ do
  password <- run $ mkPasswd []
  assert (6 == length password)


prop_length :: Property
prop_length = monadicIO $ do
  -- n        <- generate $ elements [1,2,3]
  n        <- run $ generate $ choose (6, 9)
  password <- run $ mkPasswd [Length (show n)]
  assert (n == length password)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testProperty "Make a password with the default length" prop_defaultLength
        , testProperty "Make a password with a given length"     prop_length ]

