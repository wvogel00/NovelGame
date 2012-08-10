module NovelParser where

import Control.Applicative hiding ((<|>) , many)
import Text.Parsec
import Text.Parsec.String
import NovelInterface

run :: Parser [Order] -> String -> IO [Order]
run p input = case (parse p "" input) of
    Left err -> print err >> return []
    Right xs -> return xs

--start to parse
novelParse :: Parser [Order]
novelParse = return [Clear]

-- "~"
message :: Parser [Order]
message = do
    str <- (try nClear) <|> (try nCWait)
    return [str]

nString :: Parser String
nString = (char '\"' *> many (letter <|> digit) <* char '\"')

-- %c
nClear :: Parser Order
nClear = string "%c" >> return Clear

-- %w
nCWait :: Parser Order
nCWait = string "%w" >> return ClickWait

-- wait(number)
nWait :: Parser Order
nWait = do
    string "wait"
    Wait <$> (char '(' *> num <* char ')')

image :: Parser Order
image = do
    string "image"
    Image <$> (char '(' *> mkImgTuple <*char ')')
    where mkImgTuple = (,) <$> num <*> (char ',' *> nString)

--number
num :: Parser Int
num = read <$> many1 digit
