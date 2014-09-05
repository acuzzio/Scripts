module Alessio where

import Data.Generics
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

-- internal Modules 
import ShellParser


-- ================> <=====================
shell :: QuasiQuoter
shell = QuasiQuoter {quoteExp  = quoteExpExp,
                     quotePat  = quoteExprPat,
                     quoteDec  = undefined,
                     quoteType = undefined }

quoteExpExp :: String -> Q TH.Exp
quoteExpExp s = do
       exp <- parseExp s
       dataToExpQ (const Nothing `extQ` antiExprExpr) exp 

quoteExprPat :: String -> Q TH.Pat
quoteExprPat s = do
                pat <- parseExp s
                dataToPatQ (const Nothing `extQ` antiExpPat) pat 

antiExprExpr :: Shell -> Maybe (Q TH.Exp)
antiExprExpr (EMetaVar v) = Just $ TH.varE $ TH.mkName v
antiExprExpr _ = Nothing

antiExpPat :: Shell -> Maybe (Q TH.Pat)
antiExpPat (EMetaVar v)   = Just $ TH.varP $ TH.mkName v
antiExpPat _ = Nothing
