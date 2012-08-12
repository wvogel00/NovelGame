module NovelParser where

import Control.Applicative hiding ((<|>) , many)
import Text.Parsec
import Text.Parsec.String
import NovelInterface
import Data.Maybe (fromJust)

run :: Parser [Order] -> String -> IO [Order]
run p input = case (parse p "" input) of
    Left err -> print err >> return []
    Right xs -> return xs

--start to parse
novelParse :: Parser [Order]
novelParse = return [Clear]

nToken :: Parser Order
nToken = image <|> sound <|> branch

sound = return Clear
branch = return Clear

-- "~"
message :: Parser [Order]
message = do
    string "mes"
    (k,str) <- (char '(' *> (try mes1 <|> mes2)  <* char ')')
    return.f.foldl (translate k) (False,[],Nothing) $ str  where
		mes1 = (,) <$> nString <*> (char ',' *> nString)
		mes2 = (,) "" <$> nString
		f (_,xs,mes) = xs ++ [fromJust mes]

convert "%c" = Clear
convert "%w" = ClickWait
convert "%n" = NewLine

translate k (t,xs,mes) o
    | elem o "cwn" && t = (False,xs++[convert ['%',o]] ,Nothing)
    | notElem o "cwn" && t = (False,xs,addMes mes k ['%',o])
    | o == '%' = (True,xs,mes)
    | otherwise = (False,xs,addMes mes k [o])

addMes :: Maybe Order -> String -> String -> Maybe Order
addMes mes k s2 = case mes of
    Just (Mes _ s1) -> Just $ Mes k (s1++s2)
    Nothing          -> Just (Mes k s2)

nString :: Parser String
nString = (char '\"' *> many (noneOf "\"") <* char '\"')

-- wait(number)
nWait :: Parser Order
nWait = do
    string "wait"
    Wait <$> (char '(' *> num <* char ')')

fileStr :: Parser String
fileStr = char '\"' *> (combine <$> f <*> string "." <*> f ) <* char '\"'
    where f = many1 alphaNum
          combine a b c = a ++ b ++ c

image :: Parser Order
image = do
    string "image"
    (char '(' *> (try img1 <|> try img2 <|> img3) <*char ')') where
        img1 = mk1 <$> num <*> (char ',' *> nString)
        img2 = mk2 <$> nString
        img3 = mk3 <$> num <*> (char ',' *> num) <*> (char ',' *> num) <*> (char ','*> nString)
        mk1 n     file = Image n (0,0) file
        mk2       file = Image 10 (0,0) file
        mk3 n x y file = Image n (x,y) file

--number
num :: Parser Int
num = read <$> many1 digit
