{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Prelude hiding (unlines)

import Control.Lens
import Control.Monad (void)
import Data.Int (Int8)
import Data.Text (unlines)
import Data.Word (Word8)
import Test.Hspec
import Text.Parsec (ParseError)
import Text.Parsec (parse, errorPos, sourceLine, eof, getInput, spaces)
import Text.Parsec.Text (Parser)
import qualified Text.Parsec as P (string)

import Text.ConfigParser.Types
import Text.ConfigParser.Parser

data C = C
    { _f1 :: String
    , _f2 :: Integer
    , _f3 :: Bool
    , _f4 :: [String]
    , _f5 :: [Integer]
    , _f6 :: [Bool]
    , _f7 :: [[Integer]]
    } deriving (Show, Eq)

makeLenses ''C

defC :: C
defC = C
    { _f1 = "def"
    , _f2 = 0
    , _f3 = False
    , _f4 = []
    , _f5 = []
    , _f6 = []
    , _f7 = []
    }

co1,co2,co3,co4,co5,co6,co7 :: ConfigOption C
co1 = ConfigOption
  { key    = "f1"
  , action = set' f1
  , parser = string
  }
co2 = ConfigOption
  { key    = "f2"
  , action = set' f2
  , parser = integer
  }
co3 = ConfigOption
  { key    = "f3"
  , action = set' f3
  , parser = bool
  }
co4 = ConfigOption
  { key    = "f4"
  , action = set' f4
  , parser = list string
  }
co5 = ConfigOption
  { key    = "f5"
  , action = set' f5
  , parser = list integer
  }
co6 = ConfigOption
  { key    = "f6"
  , action = set' f6
  , parser = list bool
  }
co7 = ConfigOption
  { key    = "f7"
  , action = set' f7
  , parser = list (list integer)
  }

cp, cp' :: ConfigParser C
cp  = configParser defC [co1,co2,co3,co4,co5,co6,co7]
cp' = configParser_ lp lcs defC [co1,co2,co3,co4,co5,co6,co7]
    where
    lp :: Key -> Parser a -> Parser a
    lp k q = q <* spaces <* P.string "->" <* spaces <* P.string k
    lcs    = ["--","//"]

p, p' :: Parser C
p  = config cp
p' = config cp'

shouldFailOnLine :: Show a => Either ParseError a -> Int -> Expectation
shouldFailOnLine e n = case e of
    Left err -> sourceLine (errorPos err) `shouldBe` n
    _        -> expectationFailure $ show e

main :: IO ()
main = mapM_ hspec
    [ testString
    , testInteger
    , testBoundedIntegral
    , testBool
    , testList
    , testDefaultKeyValue
    , testActionParser
    , testRemoveLineComments
    , testRemoveExtraSpaces
    , testRemoveExtraLines
    , testConfig
    ]

testString :: Spec
testString = describe "string" $ do
    it "parses a string" $
        parse string' "test" "\"foo\"" `shouldBe` Right "foo"
    it "parses a string with a literal 'n' in it" $
        parse string' "test" "\"foonbar\"" `shouldBe` Right "foonbar"
    it "parses a string with a quote at the start" $
        parse string' "test" "\"\\\"bar\"" `shouldBe` Right "\"bar"
    it "parses a string with a quote in the middle" $
        parse string' "test" "\"foo\\\"bar\"" `shouldBe` Right "foo\"bar"
    it "parses a string with a quote at the end" $
        parse string' "test" "\"foo\\\"\"" `shouldBe` Right "foo\""
    it "parses a string with a newline at the start" $
        parse string' "test" "\"\\nbar\"" `shouldBe` Right "\nbar"
    it "parses a string with a newline in the middle" $
        parse string' "test" "\"foo\\nbar\"" `shouldBe` Right "foo\nbar"
    it "parses a string with a newline at the end" $
        parse string' "test" "\"foo\\n\"" `shouldBe` Right "foo\n"
    it "parses a string with a backslash at the start" $
        parse string' "test" "\"\\\\bar\"" `shouldBe` Right "\\bar"
    it "parses a string with a backslash in the middle" $
        parse string' "test" "\"foo\\\\bar\"" `shouldBe` Right "foo\\bar"
    it "parses a string with a backslash in front of a quote" $
        parse string' "test" "\"foo\\\\\\\"bar\"" `shouldBe` Right "foo\\\"bar"
    it "parses a string with a literal backslash-n" $
        parse string' "test" "\"foo\\\\nbar\"" `shouldBe` Right "foo\\nbar"
    it "parses a string with a backslash at the end" $
        parse string' "test" "\"foo\\\\\"" `shouldBe` Right "foo\\"
    it "ends a string at the first quote" $
        parse (string' <* eof) "test" "\"foo\"bar\"" `shouldFailOnLine` 1
    it "errors on an unterminated string" $
        parse string' "test" "\"foo" `shouldFailOnLine` 1
    it "errors on a newline inside a string" $
        parse string' "test" "\"foo\nbar\"" `shouldFailOnLine` 1
    where
    string' = string :: Parser String

testInteger :: Spec
testInteger = describe "integer" $ do
    it "parses a positive integer" $
        parse integer "test" "42" `shouldBe` Right 42
    it "parses a negative integer" $
        parse integer "test" "-42" `shouldBe` Right (-42)
    it "parses a zero" $
        parse integer "test" "0" `shouldBe` Right 0
    it "ignores initial zeroes" $
        parse integer "test" "000042" `shouldBe` Right 42

testBoundedIntegral :: Spec
testBoundedIntegral = describe "boundedIntegral" $ do
    it "parses negative values in range" $
        parse int8 "test" "-128" `shouldBe` Right (-128)
    it "parses positive values in range" $
        parse int8 "test" "127" `shouldBe` Right 127
    it "parses unsigned values in range" $
        parse word8 "test" "0" `shouldBe` Right 0
    it "bounds signed values below" $
        parse int8 "test" "-129" `shouldFailOnLine` 1
    it "bounds signed values above" $
        parse int8 "test" "128" `shouldFailOnLine` 1
    it "bounds unsigned values below" $
        parse word8 "test" "-1" `shouldFailOnLine` 1
    it "bounds unsigned values above" $
        parse word8 "test" "256" `shouldFailOnLine` 1
    where
    int8  = boundedIntegral :: Parser Int8
    word8 = boundedIntegral :: Parser Word8

testBool :: Spec
testBool = describe "bool" $ do
    it "parses \"true\"" $
        parse bool "test" "true" `shouldBe` Right True
    it "parses \"True\"" $
        parse bool "test" "True" `shouldBe` Right True
    it "parses \"on\"" $
        parse bool "test" "on" `shouldBe` Right True
    it "parses \"On\"" $
        parse bool "test" "On" `shouldBe` Right True
    it "parses \"yes\"" $
        parse bool "test" "yes" `shouldBe` Right True
    it "parses \"Yes\"" $
        parse bool "test" "Yes" `shouldBe` Right True
    it "parses \"false\"" $
        parse bool "test" "false" `shouldBe` Right False
    it "parses \"False\"" $
        parse bool "test" "False" `shouldBe` Right False
    it "parses \"off\"" $
        parse bool "test" "off" `shouldBe` Right False
    it "parses \"Off\"" $
        parse bool "test" "Off" `shouldBe` Right False
    it "parses \"no\"" $
        parse bool "test" "no" `shouldBe` Right False
    it "parses \"No\"" $
        parse bool "test" "No" `shouldBe` Right False

testList :: Spec
testList = describe "list" $ do
    it "parses an empty list" $
        parse (list integer) "test" "[]" `shouldBe` Right []
    it "parses an empty list with spaces in it" $
        parse (list integer) "test" "[  ]" `shouldBe` Right []
    it "parses an empty list with a newline in it" $
        parse (list integer) "test" "[\n]" `shouldBe` Right []
    it "parses a singleton" $
        parse (list integer) "test" "[42]" `shouldBe` Right [42]
    it "parses a singleton with spaces in it" $
        parse (list integer) "test" "[  42  ]" `shouldBe` Right [42]
    it "parses a singleton with newlines in it" $
        parse (list integer) "test" "[\n42\n]" `shouldBe` Right [42]
    it "parses a list with multiple elements" $
        parse (list integer) "test" "[42,43,44]" `shouldBe` Right [42,43,44]
    it "parses a list with multiple elements with spaces in it" $
        parse (list integer) "test" "[  42  ,  43  ,  44  ]"
            `shouldBe` Right [42,43,44]
    it "parses a list with multiple elements with newlines in it" $
        parse (list integer) "test" "[\n42\n,\n43\n,\n44\n]"
            `shouldBe` Right [42,43,44]
    it "parses a nested list" $
        parse (list (list integer)) "test" "[[],[42],[42,43,44]]"
            `shouldBe` Right [[],[42],[42,43,44]]
    it "fails on an unterminated list with no elements" $
        parse (list integer) "test" "[" `shouldFailOnLine` 1
    it "fails on an unterminated list with elements" $
        parse (list integer) "test" "[42,43" `shouldFailOnLine` 1
    it "fails on an unterminated list after a comma" $
        parse (list integer) "test" "[42," `shouldFailOnLine` 1

testWhitespace :: Spec
testWhitespace = describe "whitespace" $ do
    it "parses spaces" $
        parse whitespace "test" "       " `shouldBe` Right ()
    it "parses tabs" $
        parse whitespace "test" "\t\t\t\t" `shouldBe` Right ()
    it "parses a mix of spaces and tabs" $
        parse whitespace "test" "  \t \t  \t\t " `shouldBe` Right ()
    it "fails on newlines" $
        parse (whitespace <* eof) "test" "\n" `shouldFailOnLine` 1
    it "fails on carriage return" $
        parse (whitespace <* eof) "test" "\r" `shouldFailOnLine` 1

testDefaultKeyValue :: Spec
testDefaultKeyValue = describe "defaultKeyValue" $ do
    it "parses \"<key>=<value>\"" $
        parse (defaultKeyValue "foo" integer) "test" "foo=42"
            `shouldBe` Right 42
    it "parses \"<key> = <value>\"" $
        parse (defaultKeyValue "foo" integer) "test" "foo = 42"
            `shouldBe` Right 42
    it "parses \"<key>\\t=\\t<value>\"" $
        parse (defaultKeyValue "foo" integer) "test" "foo\t=\t42"
            `shouldBe` Right 42
    it "parses \"<key>\\n=\\n<value>\"" $
        parse (defaultKeyValue "foo" integer) "test" "foo\n=\n42"
            `shouldBe` Right 42
    it "parses \"<key>\\n  =\\n  <value>\"" $
        parse (defaultKeyValue "foo" integer) "test" "foo\n  =\n  42"
            `shouldBe` Right 42
    it "parses \"<key>  \\n=  \\n<value>\"" $
        parse (defaultKeyValue "foo" integer) "test" "foo  \n=  \n42"
            `shouldBe` Right 42
    it "fails on incorrect key" $
        parse (defaultKeyValue "foo" integer) "test" "bar = 42"
            `shouldFailOnLine` 1

testActionParser :: Spec
testActionParser = describe "actionParser" $ do
    it "doesn't require whitespace at the start of a line" $
        parse (actionParser' co2) "test" "f2 = 42" `shouldBe` Right ()
    it "allows whitespace at the start of a line" $
        parse (actionParser' co2) "test" "   f2 = 42" `shouldBe` Right ()
    where
    actionParser' = void . actionParser cp

testRemoveLineComments :: Spec
testRemoveLineComments = describe "removeLineComments" $ do
    it "preserves an empty file" $
        parse removeLineComments' "test" "" `shouldBe` Right ""
    it "preserves a file with one simple line" $
        parse removeLineComments' "test" "foobaz" `shouldBe` Right "foobaz"
    it "preserves spaces in the middle of a simple line" $
        parse removeLineComments' "test" "foo   baz"
            `shouldBe` Right "foo   baz"
    it "preserves a simple multiline file" $
        parse removeLineComments' "test" "foo\nbar\nzap"
            `shouldBe` Right "foo\nbar\nzap"
    it "preserves a multiline file with spaces" $
        parse removeLineComments' "test" "foo\nwiz   woz\nzap"
            `shouldBe` Right "foo\nwiz   woz\nzap"
    it "removes a lone comment with no spaces" $
        parse removeLineComments' "test" "#foo" `shouldBe` Right "\n"
    it "removes a lone comment preceded by spaces" $
        parse removeLineComments' "test" "   #foo" `shouldBe` Right "   \n"
    it "removes a lone comment followed by spaces" $
        parse removeLineComments' "test" "#foo   " `shouldBe` Right "\n"
    it "removes a lone comment starting with spaces" $
        parse removeLineComments' "test" "#   foo" `shouldBe` Right "\n"
    it "removes a lone comment with spaces in the middle" $
        parse removeLineComments' "test" "#foo   bar" `shouldBe` Right "\n"
    it "removes a comment before a single input line" $
        parse removeLineComments' "test" "#foobar\nbaz"
            `shouldBe` Right "\nbaz"
    it "removes a comment after a single input line" $
        parse removeLineComments' "test" "baz\n#foobar"
            `shouldBe` Right "baz\n\n"
    it "removes a comment after a single input line without spaces" $
        parse removeLineComments' "test" "baz#foobar" `shouldBe` Right "baz\n"
    it "removes a comment after a single input line with spaces" $
        parse removeLineComments' "test" "baz   #foobar"
            `shouldBe` Right "baz   \n"
    it "removes a comment at the start of a multi-line file" $
        parse removeLineComments' "test" "#foobar\nfoo\nwiz woz\nzap"
            `shouldBe` Right "\nfoo\nwiz woz\nzap"
    it "removes a full-line comment at the end of a multi-line file" $
        parse removeLineComments' "test" "foo\nwiz woz\nzap\n#foobar"
            `shouldBe` Right "foo\nwiz woz\nzap\n\n"
    it "removes a line-end comment at the end of a multi-line file" $
        parse removeLineComments' "test" "foo\nwiz woz\nzap #foobar"
            `shouldBe` Right "foo\nwiz woz\nzap \n"
    it "removes a full-line comment in the middle of a multi-line file" $
        parse removeLineComments' "test" "foo\nwiz woz\n#foobar\nzap"
            `shouldBe` Right "foo\nwiz woz\n\nzap"
    it "removes a line-end comment in the middle of a multi-line file" $
        parse removeLineComments' "test" "foo\nwiz woz #foobar\nzap #foobar"
            `shouldBe` Right "foo\nwiz woz \nzap \n"
    it "resolves an escaped `#` at the start of a line" $
        parse removeLineComments' "test" "\\#bar" `shouldBe` Right "#bar"
    it "resolves an escaped `#` at the end of a line" $
        parse removeLineComments' "test" "foo\\#" `shouldBe` Right "foo#"
    it "resolves an escaped `#` in the middle of a line" $
        parse removeLineComments' "test" "foo\\#bar" `shouldBe` Right "foo#bar"
    it "allows an unescaped `\\`" $
        parse removeLineComments' "test" "\\" `shouldBe` Right "\\"
    it "permits a literal `\\#` represented as `\\\\#'" $
        parse removeLineComments' "test" "\\\\#" `shouldBe` Right "\\#"
    it "permits an escaped `#` in a comment" $
        parse removeLineComments' "test" "#foobaz\\#asdf " `shouldBe` Right "\n"
    where
    removeLineComments' = removeLineComments cp >> getInput

testRemoveExtraSpaces :: Spec
testRemoveExtraSpaces = describe "removeExtraSpaces" $ do
    it "preserves an empty file" $
        parse removeExtraSpaces' "test" "" `shouldBe` Right ""
    it "preserves a file with one simple line" $
        parse removeExtraSpaces' "test" "foobaz" `shouldBe` Right "foobaz"
    it "preserves spaces in the middle of a simple line" $
        parse removeExtraSpaces' "test" "foo   baz" `shouldBe` Right "foo   baz"
    it "preserves a simple multiline file" $
        parse removeExtraSpaces' "test" "foo\nbar\nzap"
            `shouldBe` Right "foo\nbar\nzap"
    it "preserves a multiline file with spaces" $
        parse removeExtraSpaces' "test" "foo\nwiz   woz\nzap"
            `shouldBe` Right "foo\nwiz   woz\nzap"
    it "removes space alternatives" $
        parse removeExtraSpaces' "test" " \t\r\vbaz" `shouldBe` Right "baz"
    it "removes spaces at the start of a simple line" $
        parse removeExtraSpaces' "test" "   baz" `shouldBe` Right "baz"
    it "removes spaces at the end of a simple line" $
        parse removeExtraSpaces' "test" "foo   " `shouldBe` Right "foo"
    it "removes spaces at the start of a multi-line file" $
        parse removeExtraSpaces' "test" "   foo\nwiz woz\nzap"
            `shouldBe` Right "foo\nwiz woz\nzap"
    it "removes spaces at the end of a multi-line file" $
        parse removeExtraSpaces' "test" "foo\nwiz woz\nzap   "
            `shouldBe` Right "foo\nwiz woz\nzap"
    it "removes spaces at the start of a line in a multi-line file" $
        parse removeExtraSpaces' "test" "foo\n   wiz woz\nzap"
            `shouldBe` Right "foo\nwiz woz\nzap"
    it "removes spaces at the end of a line in a multi-line file" $
        parse removeExtraSpaces' "test" "foo\nwiz woz   \nzap"
            `shouldBe` Right "foo\nwiz woz\nzap"
    where
    removeExtraSpaces' = removeExtraSpaces >> getInput

testRemoveExtraLines :: Spec
testRemoveExtraLines = describe "removeExtraLines" $ do
    it "preserves an empty file" $
        parse removeExtraLines' "test" "" `shouldBe` Right ""
    it "preserves a file with one simple line" $
        parse removeExtraLines' "test" "foobaz" `shouldBe` Right "foobaz"
    it "preserves spaces in the middle of a simple line" $
        parse removeExtraLines' "test" "foo   baz" `shouldBe` Right "foo   baz"
    it "preserves a simple multiline file" $
        parse removeExtraLines' "test" "foo\nbar\nzap"
            `shouldBe` Right "foo\nbar\nzap"
    it "preserves a multiline file with spaces" $
        parse removeExtraLines' "test" "foo\nwiz   woz\nzap"
            `shouldBe` Right "foo\nwiz   woz\nzap"
    it "removes newlines at the start of a multi-line file" $
        parse removeExtraLines' "test" "\n\n\nfoo\nwiz woz\nzap"
            `shouldBe` Right "foo\nwiz woz\nzap"
    it "removes newlines at the end of a multi-line file" $
        parse removeExtraLines' "test" "foo\nwiz woz\nzap\n\n\n"
            `shouldBe` Right "foo\nwiz woz\nzap"
    it "removes newlines in the middle of a multi-line file" $
        parse removeExtraLines' "test" "foo\nwiz woz\n\n\nzap"
            `shouldBe` Right "foo\nwiz woz\nzap"
    where
    removeExtraLines' = removeExtraLines >> getInput

testConfig :: Spec
testConfig = describe "config" $ do
    it "parses an empty config file" $
        parse p "test" "" `shouldBe` Right defC
    it "parses a config file with just empty lines" $
        parse p "test" (unlines
            [ ""
            , "\t"
            , " "
            ]) `shouldBe` Right defC
    it "parses a config file with just a line comment" $ do
        parse p "test" "# foo" `shouldBe` Right defC
    it "parses a config file with just one line" $
        parse p "test" "f1 = \"foo\"" `shouldBe` Right defC {_f1 = "foo"}
    it "parses a string with escape sequences in it" $
        parse p "test" "f1 = \"foo\\n\\\"\\\\\""
            `shouldBe` Right defC {_f1 = "foo\n\"\\"}
    it "parses multiple in-order options from a config file" $
        parse p "test" (unlines
            [ "f1 = \"foo\""
            , "f2 = 9001"
            , "f3 = True"
            , "f4 = [\"foo\",\"bar\"]"
            , "f5 = [1,2,3,4,5]"
            ]) `shouldBe` Right defC
                { _f1 = "foo"
                , _f2 = 9001
                , _f3 = True
                , _f4 = ["foo","bar"]
                , _f5 = [1,2,3,4,5]
                }
    it "parses multiple out-of-order options from a config file" $
        parse p "test" (unlines
            [ "f4 = [\"foo\",\"bar\"]"
            , "f2 = 9001"
            , "f1 = \"foo\""
            , "f3 = True"
            , "f5 = [1,2,3,4,5]"
            ]) `shouldBe` Right defC
                { _f1 = "foo"
                , _f2 = 9001
                , _f3 = True
                , _f4 = ["foo","bar"]
                , _f5 = [1,2,3,4,5]
                }
    it "allows a comment at the top of a config file" $
        parse p "test" (unlines
            [ "# I'm a comment"
            , "f1 = \"foo\""
            , "f2 = 9001"
            , "f3 = True"
            , "f4 = [\"foo\",\"bar\"]"
            , "f5 = [1,2,3,4,5]"
            ]) `shouldBe` Right defC
                { _f1 = "foo"
                , _f2 = 9001
                , _f3 = True
                , _f4 = ["foo","bar"]
                , _f5 = [1,2,3,4,5]
                }
    it "allows a comment in the middle of a config file" $
        parse p "test" (unlines
            [ "f1 = \"foo\""
            , "f2 = 9001"
            , "f3 = True"
            , "# I'm a comment"
            , "f4 = [\"foo\",\"bar\"]"
            , "f5 = [1,2,3,4,5]"
            ]) `shouldBe` Right defC
                { _f1 = "foo"
                , _f2 = 9001
                , _f3 = True
                , _f4 = ["foo","bar"]
                , _f5 = [1,2,3,4,5]
                }
    it "allows a comment at the end of a config file" $
        parse p "test" (unlines
            [ "f1 = \"foo\""
            , "f2 = 9001"
            , "f3 = True"
            , "f4 = [\"foo\",\"bar\"]"
            , "f5 = [1,2,3,4,5]"
            , "# I'm a comment"
            ]) `shouldBe` Right defC
                { _f1 = "foo"
                , _f2 = 9001
                , _f3 = True
                , _f4 = ["foo","bar"]
                , _f5 = [1,2,3,4,5]
                }
    it "allows a comment at the end of a line" $
        parse p "test" (unlines
            [ "f1 = \"foo\""
            , "f2 = 9001"
            , "f3 = True # I'm a comment"
            , "f4 = [\"foo\",\"bar\"]"
            , "f5 = [1,2,3,4,5]"
            ]) `shouldBe` Right defC
                { _f1 = "foo"
                , _f2 = 9001
                , _f3 = True
                , _f4 = ["foo","bar"]
                , _f5 = [1,2,3,4,5]
                }
    it "allows a comment at the end of a line and file" $
        parse p "test" (unlines
            [ "f1 = \"foo\""
            , "f2 = 9001"
            , "f3 = True"
            , "f4 = [\"foo\",\"bar\"]"
            , "f5 = [1,2,3,4,5] # I'm a comment"
            ]) `shouldBe` Right defC
                { _f1 = "foo"
                , _f2 = 9001
                , _f3 = True
                , _f4 = ["foo","bar"]
                , _f5 = [1,2,3,4,5]
                }
    it "allows a newline at the top of a config file" $
        parse p "test" (unlines
            [ ""
            , "f1 = \"foo\""
            , "f2 = 9001"
            , "f3 = True"
            , "f4 = [\"foo\",\"bar\"]"
            , "f5 = [1,2,3,4,5]"
            ]) `shouldBe` Right defC
                { _f1 = "foo"
                , _f2 = 9001
                , _f3 = True
                , _f4 = ["foo","bar"]
                , _f5 = [1,2,3,4,5]
                }
    it "allows a newline in the middle of a config file" $
        parse p "test" (unlines
            [ "f1 = \"foo\""
            , "f2 = 9001"
            , "f3 = True"
            , ""
            , "f4 = [\"foo\",\"bar\"]"
            , "f5 = [1,2,3,4,5]"
            ]) `shouldBe` Right defC
                { _f1 = "foo"
                , _f2 = 9001
                , _f3 = True
                , _f4 = ["foo","bar"]
                , _f5 = [1,2,3,4,5]
                }
    it "allows a newline at the end of a config file" $
        parse p "test" (unlines
            [ "f1 = \"foo\""
            , "f2 = 9001"
            , "f3 = True"
            , "f4 = [\"foo\",\"bar\"]"
            , "f5 = [1,2,3,4,5]"
            , ""
            ]) `shouldBe` Right defC
                { _f1 = "foo"
                , _f2 = 9001
                , _f3 = True
                , _f4 = ["foo","bar"]
                , _f5 = [1,2,3,4,5]
                }
    it "allows whitespace at the start of a line" $
        parse p "test" (unlines
            [ "f1 = \"foo\""
            , "f2 = 9001"
            , "   \r\v\tf3 = True"
            , "f4 = [\"foo\",\"bar\"]"
            , "f5 = [1,2,3,4,5]"
            ]) `shouldBe` Right defC
                { _f1 = "foo"
                , _f2 = 9001
                , _f3 = True
                , _f4 = ["foo","bar"]
                , _f5 = [1,2,3,4,5]
                }
    it "allows whitespace at the start of a line and file" $
        parse p "test" (unlines
            [ "   \r\v\tf1 = \"foo\""
            , "f2 = 9001"
            , "f3 = True"
            , "f4 = [\"foo\",\"bar\"]"
            , "f5 = [1,2,3,4,5]"
            ]) `shouldBe` Right defC
                { _f1 = "foo"
                , _f2 = 9001
                , _f3 = True
                , _f4 = ["foo","bar"]
                , _f5 = [1,2,3,4,5]
                }
    it "allows whitespace at the end of a line" $
        parse p "test" (unlines
            [ "f1 = \"foo\""
            , "f2 = 9001"
            , "f3 = True   \r\v\t"
            , "f4 = [\"foo\",\"bar\"]"
            , "f5 = [1,2,3,4,5]"
            ]) `shouldBe` Right defC
                { _f1 = "foo"
                , _f2 = 9001
                , _f3 = True
                , _f4 = ["foo","bar"]
                , _f5 = [1,2,3,4,5]
                }
    it "allows whitespace at the end of a line and file" $
        parse p "test" (unlines
            [ "f1 = \"foo\""
            , "f2 = 9001"
            , "f3 = True"
            , "f4 = [\"foo\",\"bar\"]"
            , "f5 = [1,2,3,4,5]   \r\v\t"
            ]) `shouldBe` Right defC
                { _f1 = "foo"
                , _f2 = 9001
                , _f3 = True
                , _f4 = ["foo","bar"]
                , _f5 = [1,2,3,4,5]
                }
    it "allows empty lines at the start of a file" $
        parse p "test" (unlines
            [ "   \r\v\t"
            , ""
            , ""
            , "f1 = \"foo\""
            , "f2 = 9001"
            , "f3 = True"
            , "f4 = [\"foo\",\"bar\"]"
            , "f5 = [1,2,3,4,5]"
            ]) `shouldBe` Right defC
                { _f1 = "foo"
                , _f2 = 9001
                , _f3 = True
                , _f4 = ["foo","bar"]
                , _f5 = [1,2,3,4,5]
                }
    it "allows empty lines in the middle of a file" $
        parse p "test" (unlines
            [ "f1 = \"foo\""
            , "f2 = 9001"
            , "   \r\v\t"
            , ""
            , ""
            , "f3 = True"
            , "f4 = [\"foo\",\"bar\"]"
            , "f5 = [1,2,3,4,5]"
            ]) `shouldBe` Right defC
                { _f1 = "foo"
                , _f2 = 9001
                , _f3 = True
                , _f4 = ["foo","bar"]
                , _f5 = [1,2,3,4,5]
                }
    it "allows empty lines at the end of a file" $
        parse p "test" (unlines
            [ "f1 = \"foo\""
            , "f2 = 9001"
            , "f3 = True"
            , "f4 = [\"foo\",\"bar\"]"
            , "f5 = [1,2,3,4,5]"
            , ""
            , ""
            , "   \r\v\t"
            ]) `shouldBe` Right defC
                { _f1 = "foo"
                , _f2 = 9001
                , _f3 = True
                , _f4 = ["foo","bar"]
                , _f5 = [1,2,3,4,5]
                }
    it "parses an alternative key-value syntax" $
        parse p' "test" (unlines
            [ "\"foo\"           -> f1"
            , "9001              -> f2"
            , "True              -> f3"
            , "[\"foo\",\"bar\"] -> f4"
            , "[1,2,3,4,5]       -> f5"
            ]) `shouldBe` Right defC
                { _f1 = "foo"
                , _f2 = 9001
                , _f3 = True
                , _f4 = ["foo","bar"]
                , _f5 = [1,2,3,4,5]
                }
    it "parses an alternative comment syntax" $
        parse p' "test" (unlines
            [ "\"foo\"           -> f1"
            , "9001              -> f2"
            , "True              -> f3"
            , "-- This is a comment"
            , "[\"foo\",\"bar\"] -> f4"
            , "[1,2,3,4,5]       -> f5"
            ]) `shouldBe` Right defC
                { _f1 = "foo"
                , _f2 = 9001
                , _f3 = True
                , _f4 = ["foo","bar"]
                , _f5 = [1,2,3,4,5]
                }
    it "parses an multiple comment syntaxes" $
        parse p' "test" (unlines
            [ "\"foo\"           -> f1"
            , "9001              -> f2"
            , "True              -> f3"
            , "// This is a comment"
            , "[\"foo\",\"bar\"] -> f4"
            , "[1,2,3,4,5]       -> f5"
            ]) `shouldBe` Right defC
                { _f1 = "foo"
                , _f2 = 9001
                , _f3 = True
                , _f4 = ["foo","bar"]
                , _f5 = [1,2,3,4,5]
                }
    it "errors on non-existent keys" $
        parse p "test" (unlines
            [ "f1 = \"foo\""
            , "f2 = 9001"
            , "f3 = True   "
            , "f4 = [\"foo\",\"bar\"]"
            , "f5 = [1,2,3,4,5]"
            , "badkey = 9999"
            ]) `shouldFailOnLine` 6
