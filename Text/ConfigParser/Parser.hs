{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.ConfigParser.Parser where

import Control.Monad (void, unless, when)
import Data.List (nub, (\\), intercalate)
import Data.String (IsString(..))
import Text.Parsec (SourceName, ParseError, State(..), parserFail, alphaNum)
import Text.Parsec (getParserState, setParserState, unexpected, newline, unexpected)
import Text.Parsec (manyTill, char, choice, digit, sepBy, many, many1, try)
import Text.Parsec (spaces, eof, parse, (<|>), (<?>))
import Text.Parsec.Char (noneOf, oneOf, anyChar)
import Text.Parsec.Text (Parser)
import qualified Data.Text as T (Text)
import qualified Data.Text.IO as T (readFile)
import qualified Text.Parsec as P (string)

import Text.ConfigParser.Util
import Text.ConfigParser.Types

-- | Parse a string surrounded by quotes. Quotes within the string must be
-- escaped with backslashes.
string :: IsString s => Parser s
string = char '"' *> fmap fromString (many stringChar) <* char '"'
     <?> "string in quotes"
    where
    stringChar = noneOf "\"\n\\" <|> char '\\' *> escapeSeq
    escapeSeq  = char '"' <|> char '\\' <|> '\n' <$ char 'n'

-- | Parse an integer.
integer :: Parser Integer
integer = read .: (++) <$> sign <*> many1 digit <?> "integer"
    where
    sign = P.string "-" <|> P.string ""

-- | Parse a bounded integer. Fail to parse with a descriptive message if the
-- value is out of bounds.
boundedIntegral :: forall n. (Show n, Bounded n, Integral n) => Parser n
boundedIntegral = bound =<< integer
              <?> "integer between " ++ show intMin ++ " and " ++ show intMax
    where
    intMin  = minBound :: n
    intMax  = maxBound :: n
    bound n | n > fromIntegral intMax = unexpected $ "integer above " ++ show intMax
            | n < fromIntegral intMin = unexpected $ "integer below " ++ show intMin
            | otherwise               = return $ fromIntegral n

-- | Parse a boolean. Valid synonyms for @True@ are @true@, @yes@, @Yes@, @on@,
-- and @On@. Valid synonyms for @False@ are @false@, @no@, @No@, @off@, and
-- @Off@.
bool :: Parser Bool
bool = True <$ try truthy <|> False <$ try falsey
    where
    truthy = choice $ fmap P.string ["true", "True", "yes", "Yes", "on", "On"]
    falsey = choice $ fmap P.string ["false", "False", "no", "No", "off", "Off"]

-- | Parse a list of values surrounded by @[@ and @]@, and separated by commas.
-- The list can contain whitespace and newlines.
list :: (Parser a) -> Parser [a]
list p = initial *> (p `sepBy` separator) <* terminator <?> "list in brackets"
    where
    initial    = try $ char '[' <* spaces
    separator  = try $ spaces *> char ',' <* spaces
    terminator = try $ spaces *> char ']'

-- | Ignore zero or more spaces, tabs, or vertical tabs.
whitespace :: Parser ()
whitespace = () <$ many (oneOf " \t\v\r") <?> "whitespace"

-- | Extract a parser for a transformation on @c@s from a 'ConfigOption'.
actionParser :: ConfigParser c -> ConfigOption c -> Parser (c -> c)
actionParser c ConfigOption {..} =
    whitespace *> keyValue c key (action <$> parser)

-- Parse a string and replace the input of the parser with the result.
replaceParserInput :: Parser String -> Parser ()
replaceParserInput p = do
    s <- getParserState
    i <- p
    void $ setParserState s {stateInput = fromString i}

-- Replace each line comment with a single newline.
removeLineComments :: ConfigParser c -> Parser ()
removeLineComments ConfigParser {..} = replaceParserInput $
    mconcat <$> many (escapedComment <|> comment <|> content)
    where
    startComment   = choice $ try . P.string <$> lineCommentInit
    terminator     = void newline <|> eof
    comment        = '\n':[] <$ startComment <* anyChar `manyTill` terminator
    escapedComment = try $ char '\\' *> startComment
    content        = (:[]) <$> anyChar

-- Remove spaces from the start and end of each line, at the start of the
-- input, and at the end of the input.
removeExtraSpaces :: Parser ()
removeExtraSpaces = replaceParserInput $
    whitespace *> contentChar `manyTill` try (whitespace *> eof)
    where
    contentChar = try strippedNL <|> anyChar
    strippedNL  = whitespace *> newline <* whitespace

-- Replace sequences of multiple newlines with a single newline.
removeExtraLines :: Parser ()
removeExtraLines = replaceParserInput $
    optionalNLs *> contentChar `manyTill` try (optionalNLs *> eof)
    where
    contentChar = combinedNLs <|> anyChar
    optionalNLs = ()   <$ many newline
    combinedNLs = '\n' <$ many1 newline

-- Parse a config file as specified by a 'ConfigParser'.
config :: ConfigParser c -> Parser c
config p = do
    unless optionKeysUniq $
        parserFail "non-unique keys in ConfigParser"
    removeLineComments p
    removeExtraSpaces
    removeExtraLines
    (ks,c) <- go [] (defaults p)
    let missingKeys = requiredKeys \\ ks
    unless (null missingKeys) $
        parserFail ("missing required keys: " ++ intercalate ", " (fmap show missingKeys))
    return c
    where
    actionParser' o = (,) (key o) <$> actionParser p o
    optionParser    = choice $ try . actionParser' <$> options p
    requiredKeys    = fmap key . filter required $ options p
    optionKeysUniq  = length (nub $ key <$> options p) == length (options p)
    go ks c = (ks,c) <$ eof <|> do
        (k,f) <- optionParser <|> do
            k <- many1 alphaNum
            unexpected $ "key: \"" ++ k ++ "\""
        when (k `elem` (ks::[Key])) $
            unexpected ("duplicate key: \"" ++ k ++ "\"")
        let c' = f c
        newline *> go (k:ks) c' <|> (k:ks,c') <$ eof

-- Parse a config file from 'Text'.
parseFromText :: ConfigParser c -> SourceName -> T.Text -> Either ParseError c
parseFromText = parse . config

-- Parse a config file from disk.
parseFromFile :: ConfigParser c -> SourceName -> IO (Either ParseError c)
parseFromFile p f = parseFromText p f <$> T.readFile f
