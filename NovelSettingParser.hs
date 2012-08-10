module NovelSettingParse where --(takeSettings)

import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>) , many)

data Setting = Title String | Width Int | Height Int deriving Show

takeSettings = run settings

run :: Parser [Setting] -> String -> IO [Setting]
run p input = case (parse p "" input) of
    Left err -> return []
    Right x -> return x

settings :: Parser [Setting]
settings = do
    x <- do (comment *> return Nothing)
            <|> (Just <$> width)
            <|> (Just <$> height)
            <|> (Just <$> title)
    toLineEnd
    case x of
        Just a  -> (a:) <$> settings
        Nothing -> settings

width :: Parser Setting
width = do
    string "width"
    Width <$> readValue num

height :: Parser Setting
height = do
    string "height"
    Height <$> readValue num

title :: Parser Setting
title = do
    string "gameTitle"
    Title <$> readValue sString

--文字列
sString :: Parser String
sString = char '\"' *> many (letter <|> digit) <* char '\"'

--数字
num :: Parser Int
num = read <$> many1 digit

readValue :: Parser a -> Parser a
readValue p = many spaces *> char '=' *> many spaces *> p

toLineEnd = (many  $ noneOf ['\n']) *> many newline

comment :: Parser String
comment = string "--" <|> string "\n"
