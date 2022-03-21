module MkPasswd.MkPasswd ( mkPasswd
                         , defaultWords ) where

import Data.Functor  ( (<&>) )
import Data.Char     ( chr, toLower, toUpper )
import Data.List     ( nub )
import System.Random ( randomRs
                     , initStdGen
                     , randomR
                     , getStdRandom )
import Data.Maybe    ( fromJust, isJust, fromMaybe )
import MkPasswd.Types (Flag(..))

-- | Some constants and default values.
defaultLength, defaultConcat, maxLength :: Int
defaultLength = 6
defaultConcat = 3
maxLength     = 15

-- Location of default dictionary.
defaultWords  :: String
--defaultWords  = "/etc/dictionaries-common/words" -- std on debian
--defaultWords  = "/usr/share/dict/words" -- std on fedora
defaultWords  = "dict/en.txt" -- the dictionary that is included with the project

-- | Get a random character in a range.
randCharInRange :: (Int, Int) -> IO Char
randCharInRange r = getStdRandom (randomR r) <&> chr

-- | Get a string of random characters in a range.
randCharsInRange :: Int -> (Int, Int) -> IO String
randCharsInRange n r = initStdGen <&> randomRs r <&> map chr . take n . nub

-- | Generate a random, printable ASCII character.
randChar :: IO Char
randChar = randCharInRange (33, 126) -- from bang to wiggle

-- | Generate a string of random, printable ASCII characters.
randChars :: Int -> IO String
randChars n = randCharsInRange n (33, 126) -- from bang to wiggle

-- | Generate a random alphabetical character.
randAlpha :: IO Char
randAlpha = randCharInRange (97, 122) -- from a to z

-- | Generate a string of random alphabetical character.
randAlphas :: Int -> IO String
randAlphas n = randCharsInRange n (97, 122) 

-- | Generate a random float between 0.0 and 1.0.
randFloat :: IO Float
randFloat = getStdRandom (randomR (0.0, 1.0))

-- | Generate a random int between 0 and n.
randInt :: Int -> IO Int
randInt n = getStdRandom (randomR (0, n))

-- | Generate n random ints between 0 and m.
randInts :: Int -> Int -> IO [Int]
randInts n m = take n . nub . randomRs (0, m) <$> initStdGen

-- | Generate 3 distinct random ints between n and m.
threeRandInts :: Int -> Int -> IO (Int, Int, Int)
threeRandInts n m = initStdGen >>=
                    (\ [x, y, z] -> pure (x, y, z)) . take 3 . nub . randomRs (n, m)

-- | Lookup table for character substitutions.
substTable :: [(Char, Char)]
substTable = [ ('e', '3')
              , ('a', '@')
              , ('l', '1')
              , ('s', '5')
              , ('o', '0')]

-- | Substitute `c' by looking it up in the lookup table. If `c' is not
-- present in the lookup table, return `c'.
subst :: Char -> Char
subst c = fromMaybe c $ lookup (toLower c) substTable

-- | Retrieve the Length option from the flags supplied, or use the default
-- value if the user didn't supply one.
getLen :: [Flag] -> Int
getLen = foldl (\acc x -> case x of 
                          (Length str) -> read str
                          _            -> acc) defaultLength

-- | Retrieve the Concat option from the flags supplied.
getCon :: [Flag] -> Maybe Int
getCon = foldl (\acc x -> case x of 
                          (Concat str) -> Just (read str)
                          _            -> acc) Nothing

-- | Retrieve the option containing the path to the dictionary file from the
-- flag supplied, or the default value.
getFp :: [Flag] -> FilePath
getFp = foldl (\acc x -> case x of 
                          (WordsFile fp) -> fp
                          _              -> acc) defaultWords 

-- | Capitalise the input, `c', which is an alphabetic char, about half of the time.
maybeUpper :: Char -> IO Char
maybeUpper c = randFloat <&> (\i -> if i==0 then toUpper c else c) . round

-- | Create a password according to the flags supplied.
mkPasswd :: [Flag] -> IO String
mkPasswd fs = 
    do let w  = Wordy `elem` fs
           s  = Strong `elem` fs
           x  = VeryStrong `elem` fs
           e  = Explain `elem` fs
           l  = getLen fs
           n  = min maxLength (max defaultLength l)
           fp = getFp fs
           c  = getCon fs
       mkPasswd' w s x e n fp c

-- | Helper function for mkPasswd.
mkPasswd' :: Bool      -- ^ Make a wordy password.
          -> Bool      -- ^ Make a strong password.
          -> Bool      -- ^ Make a very strong password.
          -> Bool      -- ^ Explain the password substitutions.
          -> Int       -- ^ Length of the password.
          -> FilePath  -- ^ Path to ditionary.
          -> Maybe Int -- ^ Make a password by concatenating this number of words.
          -> IO String -- ^ The password.
mkPasswd' w s x e n fp mc | isJust mc = mkPasswdC (fromJust mc) fp
                          | s         = mkPasswdR n randAlpha 
                          | x         = mkPasswdR n randChar
                          | w         = mkPasswdW fp
                          | otherwise = mkPasswdFromDict n fp e

-- | Create a password by concatening words.
mkPasswdC :: Int -> FilePath -> IO String
mkPasswdC n fp =  do str <- readFile fp
                     let ls = filter ((== defaultLength) . length) (lines str)
                     if length ls < n 
                       then error ("There are too few words which are " ++
                                   show defaultLength ++ " letters long in the file "++fp)
                       else randInts n (length ls - 1) <&> concatMap (ls!!)

-- | Create a random string of length n.
mkPasswdR :: Int -> IO Char -> IO String
mkPasswdR n f = if n < 1 then return []
                else do c  <- f
                        cs <- mkPasswdR (n-1) f 
                        return (c : cs)

-- | Create a password by looking up a word from the dictionary file and
-- optionally performing some substitutions.
mkPasswdFromDict :: Int -> FilePath -> Bool -> IO String
mkPasswdFromDict n fp e = 
    do str <- readFile fp
       let ls = filter ((==n) . length) (lines str)
       if null ls 
        then error ("There are no words which are " ++
                    show n ++ " letters long in the file "++fp)
        else do i  <- randInt $ length ls
                let w = ls!!i
                w' <- mapM (maybeUpper . subst) w
                return (if e then w' ++ " [" ++ w ++ "]" else w')

-- | Create a password by concatenating three words from the dictionary file.
mkPasswdW :: FilePath -> IO String
mkPasswdW f = do ls <- readFile f <&> lines
                 threeRandInts 0 (length ls - 1)
                   >>= \(i1, i2, i3) ->
                         pure $ (ls !! i1) ++ (ls !! i2) ++ (ls !! i3)

