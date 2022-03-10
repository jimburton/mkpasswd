# mkpasswd

`mkpasswd` is a simple password generation utility written in
Haskell, intended for teaching purposes on the University of Brighton
Haskell course. It provides a simple demonstration of how to use the
`System.Console.GetOpt` package, which is used to read in and organise
POSIX-compliant command line arguments. More information can be found
on the [Haskell
wiki](https://wiki.haskell.org/High-level_option_handling_with_GetOpt).

You could improve the program by using the `Data.Text` API instead of `String` 
(although it wouldn't really make any difference to performance  in such a small 
program) and perhaps by using applicative style in the `do` blocks of `IO` actions. 

`mkpasswd` generates passwords by one of the following methods:

1. picking a word from a dictionary file and replacing some of the characters, 
2. generating a stronger random string, or
3. concatenating 3 words from the dictionary.

Although an entirely random password or a dictionary word with some
letters substituted for non-alphabetic characters might seem like the
most secure choices, the concatenated version will be longer, easier
to remember (making it less likely to be written down) and harder to
crack. [Relevant xkcd](https://xkcd.com/936/). [Relevant blog post
from security expert contradicting relevant
xkcd](https://www.schneier.com/blog/archives/2014/03/choosing_secure_1.html).

## Installing and running the program

````
$ git clone https://github.com/jimburton/mkPasswd.git
$ cd mkpasswd/
$ cabal configure
$ cabal install
$ cabal run mkpasswd -h
Usage: mkPasswd [OPTION...]
  -l n              length of the password
  -s                create a somewhat stronger password
  -w                create a password from the concatenation of 3 words, e.g. correcthorsebattery
  -x                create a very strong password
  -h, -?            show this message
  -v                display the version number
  -f dict/en.txt    location of the words file
  -e                if the password is based on a dictionary word, show the original word to make the password easier to remember
  -c n              create a password that concatenates n dictionary words ("Correct Horse Staple" style) 
````

## Improving the program

Add an option to generate passwords based on [Bruce Schneir's
advice](https://www.schneier.com/blog/archives/2014/03/choosing_secure_1.html),
which is to base them on memorable sentences. For example, the
sentence *When I was seven, my sister threw my stuffed rabbit in the
toilet* could become `WIw7,mstmsritt`. This kind of password isn't
susceptible to a dictionary attack.
