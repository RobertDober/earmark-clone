module Line
    (
      LineType(..)
    , LineInfo(..)
    , RulerType(..)
    , scanLine
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)

type NumberedLine = (String, Int)
type IndentCount = (String, Int)

data LineInfo = LineInfo { text :: String
                         , lnb  :: Int
                         , indent :: Int
                         , content :: String
                         } deriving (Show, Eq)

data RulerType = ThinRuler | MediumRuler | ThickRuler deriving (Show, Eq)

data LineType =
    ErrorToken
    | Ruler LineInfo RulerType
    | Text LineInfo
    deriving (Show, Eq)


textParser :: Parser LineType
textParser = do
    text <- many anyChar
    return (Text LineInfo {text=text, lnb=0, indent=0, content=text})

scanLine :: NumberedLine -> LineType
scanLine (text, lnb) =
    case parse textParser "textParser" text of
        Right (Text lineInfo) -> Text lineInfo{lnb=lnb, content=text}


getContent :: String -> Int -> IndentCount
getContent (' ' : rest) count = getContent rest (count + 1)
getContent text count = (text, count)

