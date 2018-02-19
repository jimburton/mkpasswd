{-|
MakePasswd generates passwords by one of the following methods:

. picking a word from a dictionary file and replacing some of the characters, 
. generating a stronger random string, or
. concatenating 3 words from the dictionary.

Although an entirely random password or a dictionary word with some letters
substituted for non-alphabetic characters might seem like the most secure
choices, the concatenated version will be longer, easier to remember and harder 
to crack.  

--}

module Main
    where

import System.Environment (getArgs)
import System.Console.GetOpt ( OptDescr(..)
                             , ArgDescr(..)
                             , ArgOrder(..)
                             , getOpt
                             , usageInfo )
import Data.Char (chr, toLower, toUpper)
import Data.List (nub)
import System.Random ( randomRs
                     , newStdGen
                     , randomR
                     , getStdRandom )

import MkPasswd.MkPasswd

{-| Entry point. -}
main :: IO ()
main = do xs <- getArgs
          case getOpt RequireOrder options xs of
            (flags, [], []) -> processFlags flags
            (_, nonOpts,[]) -> error $ "unrecognized arguments: " ++ unwords nonOpts
            (_, _, msgs)    -> error $ concat msgs ++ usageInfo header options

