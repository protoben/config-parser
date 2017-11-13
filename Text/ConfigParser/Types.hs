{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Text.ConfigParser.Types where

import Data.List (nub)
import Text.Parsec (string, spaces, char, (<?>))
import Text.Parsec.Text (Parser)

type Key = String

-- Key-value pair to parse from a config file.
data ConfigOption c = forall a. ConfigOption
    { key    :: Key         -- ^ Key name.
    , parser :: Parser a    -- ^ Parser for the given value type.
    , action :: a -> c -> c -- ^ How the value should change the state @c@.
    }

-- | Parameters for a parser that takes a config file and produces a @c@. Use
-- the 'configParser_' constructor if you want to specify your own 'lineParser'
-- or 'commentStart'. Otherwise, use the 'configParser' smart constructor.
data ConfigParser c = ConfigParser
    { keyValue :: forall a. Key -> Parser a -> Parser a
      -- ^ Specifies how a key and a value parser should be represented in the
      -- config file, e.g., @key = value@, or @key: value@.
    , lineCommentInit :: [String]
      -- Strings to start a line comment, such as @#@, @--@, or @//@. All
      -- characters following this string up to the following newline or EOF
      -- will be removed. You can use the string without starting a comment by
      -- escaping it with a backslash, e.g. @\#@ or @\--@.
    , defaults :: c
      -- Initial @c@ to fold each 'ConfigOption's action over.
    , options :: [ConfigOption c]
      -- List of key-value pairs to parse from the config file. Any key in the
      -- config file that doesn't appear here will result in parse error.
    }

-- | Smart constructor to check that 'options' doesn't contain any duplicate
-- keys before creating a 'ConfigParser'.
configParser_ :: (forall a. Key -> Parser a -> Parser a)
              -> [String] -> c -> [ConfigOption c] -> ConfigParser c
configParser_ kv cs ds os' = ConfigParser kv cs ds os
    where
    os = if length (nub $ fmap key os') == length os'
            then os'
            else error "duplicate option keys in ConfigParser"

-- | Smart constructor for a 'ConfigParser' that uses a default syntax like
-- @key = value@ and line comments starting with @#@.
configParser :: c -> [ConfigOption c] -> ConfigParser c
configParser = configParser_ defaultKeyValue defaultLineCommentInit

-- | Default syntax like @key = value@.
defaultKeyValue :: Key -> Parser a -> Parser a
defaultKeyValue k p = keyParser *> separator *> p
    where
    keyParser = string k <?> "option key"
    separator = spaces *> char '=' <* spaces

-- | Default line comment like @# comment text@.
defaultLineCommentInit :: [String]
defaultLineCommentInit = ["#"]
