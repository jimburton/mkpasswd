{-|
Types for the MkPasswd program.
--}
module MkPasswd.Types where

data Flag = Length String 
          | WordsFile FilePath 
          | Strong
          | Wordy 
          | VeryStrong
          | Version
          | Help 
          | Explain
          | Concat String  deriving (Show, Eq)

