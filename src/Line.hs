module Line
    (
      LineType(..)
    , LineInfo(..)
    , RulerType(..)
    , scanLine
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)

------------------------------------------------------------------------------------------
--
--  Types
--
------------------------------------------------------------------------------------------

type NumberedLine = (String, Int)

data LineInfo = LineInfo { text :: String
                         , lnb  :: Int
                         , indent :: Int
                         , content :: String
                         } deriving (Show, Eq)

data RulerType = ThinRuler | MediumRuler | ThickRuler deriving (Show, Eq)

data LineType =
    ErrorToken ParseError
    | Blank LineInfo
    | Indent LineInfo
    | Ruler LineInfo RulerType
    | Text LineInfo
    deriving (Show, Eq)


rulerTypeFor :: Char -> RulerType
rulerTypeFor '-' = ThinRuler
rulerTypeFor '_' = MediumRuler
rulerTypeFor _ = ThickRuler

------------------------------------------------------------------------------------------
--
--  Parsers
--
------------------------------------------------------------------------------------------

blankParser :: Int -> Parser LineType
blankParser lnb = do
    blanks <- many space <* eof
    return (Blank (basicLineInfo blanks lnb))

indentParser :: Int -> Parser LineType
indentParser lnb = do
    prefix <- string "    "
    rest   <- many anyChar
    return (Indent (basicLineInfo (prefix ++ rest) lnb))

rulerParser :: Int -> Parser LineType
rulerParser lnb = do
    p <- many space
    f <- oneOf "-*_"
    s <- char f
    r <- many1 (char f) <* eof
    return (Ruler (basicLineInfo (p ++ (f:s:r)) lnb) (rulerTypeFor f))

textParser :: Int -> Parser LineType
textParser lnb = do
    text <- many anyChar
    return (Text (basicLineInfo text lnb))


tokenParser :: Int -> Parser LineType
tokenParser lnb =
    try (blankParser lnb) <|>
    try (indentParser lnb) <|>
        ( try (rulerParser lnb) <|>
            textParser lnb)

--------------------------------------------------------------------------------------
--
--  API
--
--------------------------------------------------------------------------------------
scanLine :: NumberedLine -> LineType
scanLine (text, lnb) =
    case parse (tokenParser lnb) "tokenParser" text of
        Right token -> token
        Left  error -> ErrorToken error

--------------------------------------------------------------------------------------
--
--  Helpers
--
--------------------------------------------------------------------------------------
basicLineInfo :: String -> Int -> LineInfo
basicLineInfo text lnb =
    let (content, indent) = getContent text 0
    in
        LineInfo {text=text, lnb=lnb, indent=indent, content=content}

type IndentCount = (String, Int)

getContent :: String -> Int -> IndentCount
getContent (' ' : rest) count = getContent rest (count + 1)
getContent text count = (text, count)


