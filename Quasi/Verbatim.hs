module Verbatim where

import Data.Generics
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

-- internal Modules 
import VerbatimParser


-- ================> <=====================
verbatim :: QuasiQuoter
verbatim =  QuasiQuoter {quoteExp  = quoteExpExp,
                         quotePat  = undefined,
                         quoteDec  = undefined,
                         quoteType = undefined }

quoteExpExp :: String -> Q TH.Exp
quoteExpExp s = do
       exp <- parseExp s
       dataToExpQ (const Nothing) exp 

