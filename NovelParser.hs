module NovelParser where 

import Control.Applicative hiding ((<|>) , many)
import Text.Parsec
import Text.Parsec.String
import Data.Maybe (fromJust)
import NovelInterface

run :: Parser [Order] -> String -> IO [Order]
run p input = case (parse p "" input) of
    Left err -> print err >> return []
    Right xs -> return xs

--start to parse
novelParse :: Parser [Order]
novelParse = do
    token <- try (Just <$> (branch <|> sound <|> nWait <|> image) ) <|> return Nothing
    spaces
    message <- Just <$> try message <|> return Nothing
    spaces
    case (message,token) of
        (Nothing,Nothing) -> return []
        (Nothing,Just tk) -> (tk:) <$> novelParse
        (Just ms,Nothing) -> (ms++) <$> novelParse
        (Just ms,Just tk) -> ((tk:ms)++) <$> novelParse

branch :: Parser Order
branch = do
    string "branch" *> spaces
    Branch <$> (char '(' *> spaces *> branches <* spaces <* char ')')

branches :: Parser [ (String , String , Maybe String) ]
branches = ( do 
    str <- nString
    label <- (spaces *> string "->" *> spaces *> many1 alphaNum <* spaces)
    file <- try (Just <$>(string "in" *> spaces *> nString) )
            <|> return Nothing
    xs <- try (delimiter *> branches) <|> return []
    return ((str,label,file):xs)
   )

---------------------------------------------------
message :: Parser [Order]
message = do
    string "mes" *> spaces
    (k,orders) <- (char '(' *> spaces *> (try mes1 <|> mes2) <* spaces <* char ')')
    return $ map (convert k) orders where
		mes1 = (,) <$> nString <*> getNext scenario'
		mes2 = (,) "" <$> scenario'
		scenario' = char '\"' *> scenario

--台詞部分のパース
scenario :: Parser [Order]
scenario = do
    x <- Just <$> (mesEvent <|> mesContents)
                  <|> (char '\"' *> return Nothing)
    if x == Nothing then return [] else (fromJust x:) <$> scenario

mesContents :: Parser Order
mesContents = do
    Mes "" <$> (many1 (noneOf "\"%")
               <|>( (:) <$> char '%' <*> many1 (noneOf "%\"")))

convert k (Mes _ str) = Mes k str
convert _ order = order

mesEvent :: Parser Order
mesEvent = try nClear <|> try nCWait <|> try nNewLine

nClear,nCWait,nNewLine :: Parser Order
nClear = string "%c" *> return Clear
nCWait = string "%w" *> return ClickWait
nNewLine = string "%n" *> return NewLine
----------------------------------------------------ここまで

-- wait(number)
nWait :: Parser Order
nWait = do
    string "wait" *> spaces
    Wait <$> (char '(' *> spaces *> num <* spaces <* char ')')

image :: Parser Order
image = do
    string "image" *> spaces
    char '(' *> spaces *> (try img1 <|> try img2 <|> img3) <* spaces <* char ')' where
        img1 = mk2 <$> (spaces *> nString)
        img2 = mk1 <$> num <*> getNext nString
        img3 = mk3 <$> num <*> getNext num <*> getNext num <*> getNext nString
        mk1 n     file = Image n (0,0) file
        mk2       file = Image 10 (0,0) file
        mk3 n x y file = Image n (x,y) file
---sound----------------------------------------------
sound :: Parser Order
sound = try music <|> musicOff <|> soundEffect

soundEffect :: Parser Order
soundEffect = do
    string "effect" *> spaces
    (\s -> Music s False) <$> (char '(' *> spaces *> fileStr <* spaces <* char ')')

music :: Parser Order
music = do
    string "music" *> spaces
    (\s -> Music s True) <$> (char '(' *> spaces *> fileStr <* spaces <* char ')')
 
musicOff :: Parser Order
musicOff = do
    string "musicOff" *> spaces *> char '(' *> spaces *> char ')'
    return MusicStop   
-----------------------------------------------------

nString :: Parser String
nString = (char '\"' *> many (noneOf "\"") <* char '\"')

--ファイルを示す文字列をパース
fileStr :: Parser String
fileStr = char '\"' *> (combine <$> f <*> string "." <*> f ) <* char '\"'
    where f = many1 alphaNum
          combine a b c = a ++ b ++ c

--number
num :: Parser Int
num = read <$> many1 digit

--2つ以上の引数をもつ要素を取り出す
getNext :: Parser a -> Parser a
getNext p = delimiter *> p

--区切り文字
delimiter :: Parser ()
delimiter = spaces *> char ',' *> spaces
