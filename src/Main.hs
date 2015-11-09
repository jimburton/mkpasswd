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

{-| Generate a random, printable ASCII character. -}
randChar :: IO Char
randChar = do i <- getStdRandom (randomR (33, 126)) -- from bang to wiggle
              return (chr i)

{-| Generate a random alphabetical character. -}
randAlpha :: IO Char
randAlpha = do i <- getStdRandom (randomR (97, 122)) -- from a to z
               let c = chr i
               maybeUpper c  

{-| Generate a random float between 0.0 and 1.0. -}
randFloat :: IO Float
randFloat = getStdRandom (randomR (0.0, 1.0))

{-| Generate a random int between 0 and n. -}
randInt :: Int -> IO Int
randInt n = getStdRandom (randomR (0, n))

{-| Generate 3 distinct random ints between 0 and n. -} 
threeRandInts :: Int -> IO (Int, Int, Int)
threeRandInts n = do g <- newStdGen
                     let [x, y, z] = take 3 . nub $ randomRs (0, n) g
                     return (x, y, z)

{-| Lookup table for character substitutions. -}
substTable :: [(Char, Char)]
substTable = [ ('e', '3')
              , ('a', '@')
              , ('l', '1')
              , ('s', '5')
              , ('o', '0')]

{-| Substitute `c' by looking it up in the lookup table. If `c' is not
present in the lookup table, return `c'. -} 
subst :: Char -> Char
subst c = case lookup (toLower c) substTable of
            (Just c') -> c'
            Nothing   -> c

{-| Capitalise the input, `c', which is an alphabetic char, about half of the time. -} 
maybeUpper :: Char -> IO Char
maybeUpper c = do f <- randFloat
                  let i = round f
                  return (if i == 0 then toUpper c else c)

{-| Create a password according to the flags supplied. -}                  
mkPasswd :: [Flag] -> IO String
mkPasswd fs = 
    do let h  = Help `elem` fs
           w  = Wordy `elem` fs
           v  = Version `elem` fs
           s  = Strong `elem` fs
           x  = VeryStrong `elem` fs
           e  = Explain `elem` fs
           l  = getLen fs
           n  = min maxLength (max defaultLength l)
           fp = getFp fs 
       mkPasswd' w s x e n fp

{-| Helper function for makePasswd. -}
mkPasswd' :: Bool -> Bool -> Bool -> Bool -> Int -> FilePath -> IO String
mkPasswd' w s x e n fp | s         = mkPasswdR n randAlpha 
                       | x         = mkPasswdR n randChar
                       | w         = mkPasswdW fp
                       | otherwise = mkPasswdFromDict n fp e

{-| Create a random string of length n. -}
mkPasswdR :: Int -> IO Char -> IO String
mkPasswdR n f = if n < 1 then return []
                else do c  <- f
                        cs <- mkPasswdR (n-1) f 
                        return (c : cs)

{-| Create a password by looking up a word from the dictionary file and optionally
performing some substitutions. -}
mkPasswdFromDict :: Int -> FilePath -> Bool -> IO String
mkPasswdFromDict n fp e = 
    do str <- readFile fp
       let ls = filter ((==n) . length) (lines str)
       if null ls 
        then error ("There are no words which are " ++ show n ++ " letters long in the file "++fp)
        else do i  <- randInt $ length ls
                let w = ls!!i
                w' <- mapM (maybeUpper . subst) w
                return (if e then w' ++ " [" ++ w ++ "]" else w')

{-| Create a password by concatenating three words from the dictionary file. -}
mkPasswdW :: FilePath -> IO String
mkPasswdW f = do str <- readFile f
                 let ls = lines str
                     n  = length ls - 1
                 (i1, i2, i3) <- threeRandInts n
                 return $ (ls !! i1) ++ (ls !! i2) ++ (ls !! i3)
               
{-| Decide what to do based on the flags supplied. This will result in printing
the help or version messages, or creating a password. -}
processFlags :: [Flag] -> IO ()
processFlags fs | Help `elem` fs    = putStrLn $ usageInfo header options
                | Version `elem` fs = putStrLn version
                | otherwise         = do pwd <- mkPasswd fs
                                         putStrLn pwd

{-| Entry point. -}
main :: IO ()
main = do xs <- getArgs
          case getOpt RequireOrder options xs of
            (flags, [], []) -> processFlags flags
            (_, nonOpts,[]) -> error $ "unrecognized arguments: " ++ unwords nonOpts
            (_, _, msgs)    -> error $ concat msgs ++ usageInfo header options


{-| Functions relating to processing command-line options supplied by the user. 
See the Haskell wiki for a short tutorial explaining the purpose of flags,
options, required arguments and so on. -}
data Flag = Length String 
          | WordsFile FilePath 
          | Strong
          | Wordy 
          | VeryStrong
          | Version
          | Help 
          | Explain deriving (Show, Eq)

header = "Usage: mkPasswd [OPTION...]" 

defaultWords = "/etc/dictionaries-common/words"

defaultLength, maxLength, wordyLength :: Int
defaultLength = 6
maxLength     = 15
wordyLength   = 4

version = "mkPasswd 0.1"

options :: [OptDescr Flag] 
options = [ Option "l" [] (ReqArg Length "6") "length of the password" 
          , Option "s" [] (NoArg Strong) 
                                    "create a somewhat stronger password"
          , Option "w" [] (NoArg Wordy) 
                                    "create a password from the concatenation \
                                    \of 3 words, e.g. correcthorsebattery"
          , Option "x" [] (NoArg VeryStrong) "create a very strong password"
          , Option "h?" [] (NoArg Help) "show this message"
          , Option "v" [] (NoArg Version) "display the version number" 
          , Option "f" [] (ReqArg WordsFile defaultWords) 
                                    "location of the words file" 
          , Option "e" [] (NoArg Explain) 
                                    "if the password is based on a dictionary \
                                    \word, show the original word to make the \
                                    \password easier to remember" ]

{-| Retrieve the Length option from the flags supplied, or use the default value if the
user didn't supply one. -}
getLen :: [Flag] -> Int
getLen = foldl (\acc x -> case x of 
                          (Length str) -> read str
                          _            -> acc) defaultLength 

{-| Retrieve the option containing the path to the dictionary file from the flags supplied, or use the 
default value if the user didn't supply one. -}
getFp :: [Flag] -> FilePath
getFp = foldl (\acc x -> case x of 
                          (WordsFile fp) -> fp
                          _              -> acc) defaultWords 
