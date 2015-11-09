==MakePasswd==

MakePasswd is a simple password generation untility written in
Haskell, intended for teaching purposes on the University of Brighton
Haskell course. It provides a simple demonstration of how to use the
`System.Console.GetOpt` package, which is used to read in and organise
POSIX-compliant command line arguments. More information can be found
on the [Haskell
wiki](https://wiki.haskell.org/High-level_option_handling_with_GetOpt).

You could improve the program by using the `Data.Text` API instead of `String` 
(although it wouldn't really make any difference to performance  in such a small 
program) and perhaps by using applicative style in the `do` blocks of `IO` actions. 

`mkPasswd` generates passwords by one of the following methods:

. picking a word from a dictionary file and replacing some of the characters, 
. generating a stronger random string, or
. concatenating 3 words from the dictionary.

The default dictionary file, /etc/dictionaries-common/words, is
correct on Debian-based systems (including Ubuntu). It isn't tested on
Windows but if you supply the location of a file that contains a list
of words, one per line, then that should work.

Although an entirely random password or a dictionary word with some
letters substituted for non-alphabetic characters might seem like the
most secure choices, the concatenated version will be longer, easier
to remember and harder to crack. [Relevant
xkcd](https://xkcd.com/936/).