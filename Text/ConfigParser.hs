{-|
Module      : Text.ConfigParser
Description : A simple library to parse config files using parsec that
              generates parse errors on invalid keys
Copyright   : (c) Ben Hamlin, 2017
License     : MIT
Maintainer  : protob3n@gmail.com
Stability   : experimental
Portability : POSIX

This is yet another entry in Haskell's enourmous collection of config-file
parsing libraries. It lacks many of the bells and whistles of other config-file
parsing libraries, such as hierarchical sections and on-the-fly reloading. On
the other hand, it has a combination of features I was unable to find in other
libraries:

    * Keys and values are parsed with configurable parsec parsers, resulting in
      flexible syntax and pretty error messages.

    * Custom parsers can be created with parsec to handle values of any type.

    * Keys that aren't explicitly handled result in parse errors.

If you don't need all of these features, there are probably better libraries out
there for you. If you're free to use its idiosyncratic file format, the
config-value library, in particular, is excelent.

= Example:

By default, this library parses flat config like the following:

> a_string = "blah, blah, blah\nmore blah"
> a_number = 9001
> a_list   = [1,2,3,4,5]
> # This is a comment

If you wanted to parse the above file, saved as @./config.txt@, you might do so
as follows:

> import Text.ConfigParser
> 
> cp :: ConfigParser (Maybe String, Maybe Integer, [Integer])
> cp = configParser (Nothing, Nothing, [])
>     [ ConfigOption
>         { key    = "a_string"
>         , parser = string
>         , action = \s (_,n,ns) -> (Just s, n, ns)
>         }
>     , ConfigOption
>         { key    = "a_number"
>         , parser = integer
>         , action = \n (s,_,ns) -> (s, Just n, ns)
>         }
>     , ConfigOption
>         { key    = "a_list"
>         , parser = list integer
>         , action = \ns (s,n,_) -> (s, n, ns)
>         }
>     ]
> 
> main :: IO ()
> main = parseFromFile cp "./config.txt" >>= print
-}
module Text.ConfigParser
    ( Key
    , ConfigOption(..)
    , ConfigParser(..)
    , optionalCO
    , requiredCO
    , configParser
    , defaultKeyValue
    , defaultLineCommentInit
    , config
    , string
    , integer
    , boundedIntegral
    , bool
    , list
    , parseFromText
    , parseFromFile
    ) where

import Text.ConfigParser.Types as Types
import Text.ConfigParser.Parser as Parser
