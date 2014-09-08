{-# Language QuasiQuotes #-}

module Main where


import Verbatim 
import VerbatimParser

main = do
      print newOne
      putStrLn newOne


newOne = [verbatim|
LOOOL
%%lal
%lel
|]

--script1 m n = [verbatim|
-- #! /usr/bin/bash
--Hi there Alessio I'm a funny bash script
--for computing funny stuff with some 
--funny argument like %m and %n
-- |]
--  
--script2 xs = [verbatim|
--random_Path 
--what if I want to print a list ?
--well for the moment lets just show the list like %xs
-- |]



