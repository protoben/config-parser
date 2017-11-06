{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Text.ConfigParser.Types where

import Text.Parsec (string, spaces, char, many, (<?>))
import Text.Parsec.Char
import Text.Parsec.Text (Parser)

type Key = String

-- Key-value pair to parse from a config file.
data ConfigOption c = forall a. ConfigOption
    { key    :: Key         -- ^ Key name.
    , parser :: Parser a    -- ^ Parser for the given value type.
    , action :: a -> c -> c -- ^ How the value should change the state @c@.
    }

-- | Parameters for a parser that takes a config file and produces a @c@. Only
-- use this constructor if you want to specify your own 'lineParser' or
-- 'commentStart'. Otherwise, use the 'configParser' smart constructor.
data ConfigParser c = ConfigParser
    { lineParser   :: forall a. Key -> Parser a -> Parser a
      -- ^ Specifies how a key and a value parser should be represented in the
      -- config file, e.g., @key = value@, or @key: value@.
    , commentStart :: String
      -- ^ String to starts a line comment, e.g., @"#"@. Make sure this doesn't
      -- share a prefix with 'lineParser' or the 'parser' element of any
      -- 'ConfigOption's.
    , defaults     :: c
      -- Initial @c@ to fold each 'ConfigOption's action over.
    , options      :: [ConfigOption c]
      -- List of key-value pairs to parse from the config file. Any key in the
      -- config file that doesn't appear here will result in parse error.
    }

-- | Smart constructor for a 'ConfigParser' that uses a default syntax like
-- @key = value@ and line comments starting with @#@.
configParser :: c -> [ConfigOption c] -> ConfigParser c
configParser = ConfigParser defaultLineParser defaultCommentStart

-- | Default syntax like @key = value@.
defaultLineParser :: Key -> Parser a -> Parser a
defaultLineParser k p = keyParser *> separator *> p
    where
    keyParser = many (oneOf " \t") *> string k <?> "option key"
    separator = spaces *> char '=' <* spaces

-- | Line comments starting with @#@.
defaultCommentStart :: String
defaultCommentStart = "#"
