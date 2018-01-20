{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.ConfigParser.Types where

import Text.Parsec (spaces, char)
import Text.Parsec.Text (Parser)

type Key = String

-- ^ Key-value pair to parse from a config file.
data ConfigOption c
    = forall a. ConfigOption
        { key      :: Key         -- ^ Key name.
        , required :: Bool        -- ^ Whether it is an error to omit this key.
        , parser   :: Parser a    -- ^ Parser for the given value type.
        , action   :: a -> c -> c -- ^ How the value should change the state @c@.
        }

optionalCO, requiredCO :: Key -> Parser a -> (a -> c -> c) -> ConfigOption c
optionalCO k = ConfigOption k False
requiredCO k = ConfigOption k True

-- | Parameters for a parser that takes a config file and produces a @c@. Use
-- the 'ConfigParser' constructor if you want to specify your own 'lineParser'
-- or 'commentStart'. Otherwise, use the 'configParser' smart constructor.
data ConfigParser c = ConfigParser
    { keyValue :: forall a. Parser Key -> Parser a -> Parser a
      -- ^ Specifies how a key and a value parser should be represented in the
      -- config file, e.g., @key = value@, or @key: value@.
    , lineCommentInit :: [String]
      -- ^ Strings to start a line comment, such as @#@, @--@, or @//@. All
      -- characters following this string up to the following newline or EOF
      -- will be removed. You can use the string without starting a comment by
      -- escaping it with a backslash, e.g. @\\#@ or @\\--@.
    , defaults :: c
      -- ^ Initial @c@ to fold each 'ConfigOption' action over.
    , options :: [ConfigOption c]
      -- ^ List of key-value pairs to parse from the config file. Any key in the
      -- config file that doesn't appear here will result in parse error.
    }

-- | Smart constructor for a 'ConfigParser' that uses a default syntax like
-- @key = value@ and line comments starting with @#@.
configParser :: c -> [ConfigOption c] -> ConfigParser c
configParser = ConfigParser defaultKeyValue defaultLineCommentInit

-- | Default syntax like @key = value@.
defaultKeyValue :: Parser Key -> Parser a -> Parser a
defaultKeyValue k p = k *> separator *> p
    where
    separator = spaces *> char '=' <* spaces

-- | Default line comment like @# comment text@.
defaultLineCommentInit :: [String]
defaultLineCommentInit = ["#"]
