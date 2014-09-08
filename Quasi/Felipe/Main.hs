{-# Language QuasiQuotes #-}

module Main where


import Alessio 
import ShellParser


liftS :: String -> Shell
liftS = PlainDat 
 
main = do
      print "hello"
      writeFile "out1" $ printShell $ script1 (liftS "FunArg1")  (liftS "32")
      writeFile "out2" . printShell $ script2 . liftS . show $ [0..4] 

script1 m n = [shell|
#! /usr/bin/FunShell
Hi there Alessio I'm a funny Shell script
for computing funny stuff with some 
funny argument like %m and %n
|]
  
script2 xs = [shell|random_Path 
what if I want to print a list ?
well for the moment lets just show the list like %xs|]


-- QuasiQuoter quotePat is  [shell| $n |] (pattern) in this expression
-- script3 [shell| $n |] = [shell| some scripts|]

-- QuasiQuoter quoteDec would be this one:
-- [shell| NewFun = Exp1 Exp2 |]

-- QuasiQuoter quoteType would be here:



