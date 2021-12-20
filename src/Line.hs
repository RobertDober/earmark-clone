module Line
    ( LineType(..)
    , LineInfo(..)
    , scanLine
    ) where

type NumberedLine = (String, Int)
type IndentCount = (String, Int)

data LineInfo = LineInfo { text :: String
                         , lnb  :: Int
                         , indent :: Int
                         , content :: String
                         } deriving (Show, Eq)

data LineType =
    Text LineInfo deriving (Show, Eq)

scanLine :: NumberedLine -> LineType
scanLine (text, lnb) =
    let (content, indent) = getContent text 0
        in Text LineInfo {text=text, lnb=lnb, indent=indent, content=content}

getContent :: String -> Int -> IndentCount
getContent (' ' : rest) count = getContent rest (count + 1)
getContent text count = (text, count)

