{-# LANGUAGE DeriveDataTypeable #-}

module VerbatimParser where

import Control.Applicative ((<$>))
import Data.Data
import Data.Typeable
import Text.Parsec
import Text.Parsec.String (Parser)


parseExp :: Monad m => String -> m String
parseExp s = 
            case runP insideParser () "" s  of
                 Left err -> fail . show $ err 
                 Right xs -> return xs 

insideParser :: Parser String
insideParser = do
     string "\n"
     a <- manyTill transformer eof
     return $ "\"" ++ (concat a) ++ "\""
--     return $ (concat a) 

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
--    return $ " ++ " ++ a ++ " ++ "

otherTypeV :: Parser String
otherTypeV = do
    string "%%"
    a <- manyTill anyChar $ lookAhead $ try spaces2
    return $ "\" ++ (show " ++ a ++ ") ++ \""
--    return $ " ++ (show " ++ a ++ ") ++ "

carriageReturn :: Parser String
carriageReturn = do
   string "\n"
   return $ "\\n"
--   return $ "\n"

percentage = do
   string "\\%"
   return "%"

simpleBackSlash = do
   string "\\"
   return "\\\\"

spaces2 :: Parser String
spaces2 =       try (string "\r")
                <|> try (string "\n")
                <|> string "\t"
                <|> string " "

