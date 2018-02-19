module Main where

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import MkPasswd.MkPasswd

prop_defaultLength :: Property
prop_defaultLength = monadicIO $ do
  password <- run $ mkPasswd [WordsFile "/etc/dictionaries-common/words"]
  run $ putStrLn password
  assert (6 == length password)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testProperty "Make a password" prop_defaultLength ]

