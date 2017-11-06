module Text.ConfigParser.Parser
    ( config
    , string
    , integer
    , bool
    , list
    ) where

import Data.String (IsString(..))
import Text.Parsec (char, choice, digit, sepBy, many, many1, try, spaces)
import Text.Parsec (eof, endOfLine, optional, (<|>), (<?>))
import Text.Parsec.Char (noneOf, oneOf)
import Text.Parsec.Text (GenParser, Parser)
import qualified Text.Parsec as P (string)

import Text.ConfigParser.Util
import Text.ConfigParser.Types

string :: IsString s => Parser s
string = char '"' *> fmap fromString (many escapedChar) <* char '"'
             <?> "string in quotes"
    where
    escapedChar = let escapables = "\"\n\\" in choice
        [ noneOf escapables
        , char '\\' *> oneOf escapables
        ]

integer :: Parser Integer
integer = read .: (++) <$> sign <*> many1 digit <?> "integer"
    where
    sign = P.string "-" <|> P.string ""

bool :: Parser Bool
bool = True <$ truthy <|> False <$ falsey
    where
    truthy = choice $ fmap P.string ["true", "True", "yes", "Yes", "on", "On"]
    falsey = choice $ fmap P.string ["false", "False", "no", "No", "off", "Off"]

list :: (Parser a) -> Parser [a]
list p =
    initial *> (p `sepBy` separator) <* terminator <?> "list in brackets"
    where
    initial    = try $ char '[' <* spaces
    separator  = try $ spaces *> char ',' <* spaces
    terminator = try $ spaces *> char ']'

whitespace :: Parser ()
whitespace = () <$ many (oneOf " \t") <?> "whitespace"

lineEnd :: String -> Parser ()
lineEnd commentMarker = () <$ whitespace <* optional comment <* terminator
    where
    comment    = commentParser commentMarker
    terminator = endOfLine <?> "end of line"

commentParser :: String -> Parser ()
commentParser marker = () <$ P.string marker <* many (noneOf "\n") <?> "line comment"

actionParser :: ConfigParser c -> ConfigOption c -> Parser (c -> c)
actionParser c (ConfigOption k p a) = lineParser c k $ a <$> p

config :: ConfigParser c -> Parser c
config c = foldr ($) (defaults c) <$> (fileStart *> file <* fileEnd)
    where
    fileStart  = try $ many emptyLines
    file       = keyValue `sepBy` emptyLines
    fileEnd    = try (many emptyLines) *> optional whitespace *> optional comment *> eof
    comment    = commentParser $ commentStart c
    keyValue   = choice $ try . actionParser c <$> options c
    emptyLines = many1 $ lineEnd (commentStart c)
