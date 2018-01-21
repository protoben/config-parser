{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.ConfigParser.Parser where

import Control.Monad (void, unless, when)
import Data.List (nub, (\\), intercalate)
import Data.String (IsString(..))
import Text.Parsec (ParseError, unexpected, parserFail)
import Text.Parsec (newline, manyTill, char, choice, digit, sepBy, many, many1)
import Text.Parsec (try, spaces, eof, parse, (<|>), (<?>), lookAhead)
import Text.Parsec.Char (noneOf, oneOf, anyChar)
import Text.Parsec.Pos (SourceName, initialPos, sourceName)
import Text.Parsec.Prim (setInput, getPosition, setPosition)
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
boundedIntegral = try bounded
            <?> "integer between " ++ show intMin ++ " and " ++ show intMax
    where
    bounded = bound =<< integer
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

-- | Parse a string and replace the input of the parser with the result.
replaceParserInput :: Parser String -> Parser ()
replaceParserInput parser = void $ do
    input  <- parser
    source <- sourceName <$> getPosition
    setInput    $ fromString input
    setPosition $ initialPos source

-- | Replace each line comment with a single newline.
removeLineComments :: ConfigParser c -> Parser ()
removeLineComments ConfigParser {..} = replaceParserInput $
    mconcat <$> many (escapedComment <|> comment <|> content)
    where
    startComment   = choice $ try . P.string <$> lineCommentInit
    terminator     = void newline <|> eof
    comment        = '\n':[] <$ startComment <* anyChar `manyTill` terminator
    escapedComment = try $ char '\\' *> startComment
    content        = (:[]) <$> anyChar

-- | Parse a config file as specified by a 'ConfigParser'.
config :: ConfigParser c -> Parser c
config p = do
    unless optionKeysUniq $
        parserFail "non-unique keys in ConfigParser"
    removeLineComments p
    (ks,c) <- go ([]::[Key]) (defaults p)
    let missingKeys = requiredKeys \\ ks
    unless (null missingKeys) $
        parserFail ("missing required keys: " ++ intercalate ", " (fmap show missingKeys))
    return c
    where
    whitespace     = void . many  $ oneOf " \t\v\r"     :: Parser ()
    line           =        many  $ noneOf "\n\r"       :: Parser String
    requiredKeys   = fmap key . filter required $ options p
    optionKeysUniq = length (nub $ key <$> options p) == length (options p)
    maybeP parser  = Just <$> try parser <|> Nothing <$ try line
    optionParsers  = choice $ try . keyActionP <$> options p
    dummyActionP   = (Nothing,Nothing) <$ keyValue p (keyIdentifier p) (try line)
    configLineP    = (optionParsers <|> try dummyActionP) <* whitespace <* (void newline <|> eof)
    keyActionP o   = (Just o,) <$> mbActionParser o
    actionParser   ConfigOption {..} = keyValue p (P.string key) $ action <$> parser
    mbActionParser ConfigOption {..} = keyValue p (P.string key) . maybeP $ action <$> parser
    go  ks c = whitespace *> go' ks c <* whitespace
    go' ks c = (ks,c) <$ eof     -- End of document
          <|> newline *> go ks c -- Empty line
          <|> do                 -- Config line
            (mbo,mbv) <- lookAhead configLineP <|> parserFail "Parsing failed"
            case mbo of
                -- Key is bad
                Nothing -> do 
                    k <- keyIdentifier p
                    unexpected $ "unknown key " ++ show k
                -- Key is good
                Just o@(ConfigOption {..}) -> do
                    when (key `elem` ks) $ unexpected ("duplicate key " ++ show key)
                    case mbv of
                        -- Value is bad
                        Nothing -> do
                            _ <- actionParser o -- This should error out, but...
                            parserFail $ "Couldn't parse value for key " ++ show key
                        -- Value is good
                        Just f -> actionParser o >> go (key:ks) (f c)

-- Parse a config file from 'Text'.
parseFromText :: ConfigParser c -> SourceName -> T.Text -> Either ParseError c
parseFromText = parse . config

-- Parse a config file from disk.
parseFromFile :: ConfigParser c -> SourceName -> IO (Either ParseError c)
parseFromFile p f = parseFromText p f <$> T.readFile f
