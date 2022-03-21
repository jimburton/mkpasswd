module Main where

import Test.HUnit

import MkPasswd.MkPasswd (mkPasswd)

foo :: Int -> (Int, Int)
foo x = (1, x)

test1 :: Test
test1 = TestCase (do p <- mkPasswd []
                     assertEqual "Default password length," 6 (length p))
tests :: Test
tests = TestList [TestLabel "Test default password length" test1]

{-
tests' :: Test
tests' = test [ "test1" ~: "(foo 3)" ~: (1,2) ~=? foo 3,
                "test2" ~: do (x, y) <- partA 3
                              assertEqual "for the first result of partA," 5 x
                              partB y @? "(partB " ++ show y ++ ") failed" ]
-}

main :: IO Counts
main = runTestTT tests
       --do _ <- runTestTT tests
       --   runTestTT tests'
