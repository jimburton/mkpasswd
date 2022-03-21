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

import System.Environment    ( getArgs )
import System.Console.GetOpt ( OptDescr(..)
                             , ArgDescr(..)
                             , ArgOrder(..)
                             , getOpt
                             , usageInfo )
import MkPasswd.MkPasswd ( Flag(..)
                         , mkPasswd
                         , defaultWords )

-- | Some constants and default values.
version, header :: String
version       = "mkPasswd 0.1"
header        = "Usage: mkPasswd [options]" 

-- | Decide what to do based on the flags supplied. This will result
-- in printing the help or version messages, or creating a password.
processFlags :: [Flag] -> IO ()
processFlags fs | Help `elem` fs    = putStrLn $ usageInfo header options
                | Version `elem` fs = putStrLn version
                | otherwise         = do pwd <- mkPasswd fs
                                         putStrLn pwd
                                         
-- | Command-line options.
options :: [OptDescr Flag] 
options = [ Option "l"  [] (ReqArg Length "6") "length of the password" 
          , Option "s"  [] (NoArg Strong) 
                                    "create a somewhat stronger password"
          , Option "w"  [] (NoArg Wordy) 
                                    "create a password from the concatenation \
                                    \of 3 words, e.g. correcthorsebattery"
          , Option "x"  [] (NoArg VeryStrong) "create a very strong password"
          , Option "h?" [] (NoArg Help) "show this message"
          , Option "v"  [] (NoArg Version) "display the version number" 
          , Option "f"  [] (ReqArg WordsFile defaultWords) 
                                    "location of the words file" 
          , Option "e"  [] (NoArg Explain) 
                                    "if the password is based on a dictionary \
                                    \word, show the original word to make the \
                                    \password easier to remember"
          , Option "c"  [] (ReqArg Concat "3") "create a password based on concatenation \
                                               \of dictionary words, default 3"]

-- | Entry point.
main :: IO ()
main = do xs <- getArgs
          case getOpt RequireOrder options xs of
            (flags, [], []) -> processFlags flags
            (_, nonOpts,[]) -> error $ "unrecognized arguments: " ++ unwords nonOpts
            (_, _, msgs)    -> error $ concat msgs ++ usageInfo header options

