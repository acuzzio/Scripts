

module Main where 


import Data.Char
import Text.Parsec
-- import Text.Parsec.ByteString (Parser,parseFromFile)
import Text.Parsec.Combinator
import Text.Parsec.String
import Data.Functor.Identity

file = "file2.hs" 

main :: IO()
main = do 
   r <- parseFromFile preProc file
   case r of 
     Left msg -> print msg
     Right x  -> writeFile "lollo.hs" $ concat x
          
     
preProc = manyTill selector eof

selector = try topParser2
           <|> try topParser
           <|> manyTill anyChar eof

topParser :: Parser String
topParser = manyTill anyChar $ lookAhead $ try $ string "{{++"

topParser2 :: Parser String
topParser2 = do
            open
            a <- manyTill anyChar close
            let b = runParser insideParser () "" a
            case b of
              Left msg -> return $ show msg
              Right x -> return x
                                                 
open :: Parser String
open = string "{{++"

close :: Parser String
close = string $ "++}}"

spaces2 :: Parser String
spaces2 =       try (string "\r")
                <|> try (string "\n")
                <|> string "\t"
                <|> string " "

insideParser :: Parser String
insideParser = do
     string "\n"
     a <- manyTill transformer eof
     return $ "\"" ++ (concat a) ++ "\""

transformer = try otherTypeV
              <|> try stringV
              <|> try carriageReturn
              <|> try percentage
              <|> try simpleBackSlash
              <|> theRest

theRest :: Parser String
theRest = manyTill anyChar $ lookAhead $ try tokenS

tokenS :: Parser Char
tokenS = char '%'
        <|> char '\n'
        <|> char '\\'

stringV :: Parser String
stringV = do
    string "%"
    a <- manyTill anyChar $ lookAhead $ try spaces2
    return $ "\" ++ " ++ a ++ " ++ \""

otherTypeV :: Parser String
otherTypeV = do
    string "%%"
    a <- manyTill anyChar $ lookAhead $ try spaces2
    return $ "\" ++ (show " ++ a ++ ") ++ \""

carriageReturn :: Parser String
carriageReturn = do
   string "\n"
   return $ "\\n"

percentage = do
   string "\\%"
   return "%"

simpleBackSlash = do
   string "\\"
   return "\\\\"
