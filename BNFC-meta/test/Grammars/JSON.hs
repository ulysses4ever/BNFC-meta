{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Grammars.JSON where

import Language.LBNF.Compiletime
import Language.LBNF(lbnf, bnfc, dumpCode)

bnfc [lbnf|
Num. Json ::= Integer;
Stri. Json ::= String ;
TBool. Json ::= "True" ;
FBool. Json ::= "False" ;

Con. Json ::= Container ;
Object. Container ::= "{" [Member] "}" ;
Array. Container ::= "[" [Json] "]" ;

MMember. Member ::= String ":" Json ;

separator Json "," ;
separator Member "," ;
|]