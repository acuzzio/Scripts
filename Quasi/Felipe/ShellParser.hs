{-# LANGUAGE DeriveDataTypeable #-}


module ShellParser where

import Control.Applicative ((<$>))
import Data.Data
import Data.Typeable
import Text.Parsec
import Text.Parsec.String (Parser)


-- =============> Data Types <===========
data Shell =   DatShell Shell Shell
               | PlainDat  String
               | EMetaVar String 
               deriving (Data,Show,Typeable)

printShell :: Shell -> String
printShell (DatShell sh1 sh2) = (printShell sh1) ++ (printShell sh2)
printShell (PlainDat s)       = s
printShell (EMetaVar v)       =  "EMetaVar  " ++  v

-- ============> <===================
parseExp :: Monad m => String -> m Shell
parseExp s = 
            case runP parseShell () "" s  of
                 Left err -> fail . show $ err 
                 Right xs -> return xs 


parseShell :: Parser Shell
parseShell = pTerm `chainl1` (return DatShell)

pTerm :: Parser Shell
pTerm = pPlainTex <|> pMetaVar

pPlainTex :: Parser Shell
pPlainTex = fmap PlainDat $ many1 $ noneOf ['%']

pMetaVar :: Parser Shell 
pMetaVar = char '%' >> EMetaVar <$> ident

small   = lower <|> char '_'
large   = upper
idchar  = small <|> large <|> digit <|> char '\''
ident = do { c <- small; cs <- many idchar; return (c:cs) }
