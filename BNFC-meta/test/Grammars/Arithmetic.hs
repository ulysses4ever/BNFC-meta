{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Grammars.Arithmetic where

import Language.LBNF.Compiletime
import Language.LBNF(lbnf, bnfc, dumpCode)

-- Arithmetic
bnfc [lbnf|
EAdd. Exp  ::= Exp "+" Exp1 ;
ESub. Exp  ::= Exp "-" Exp1 ;
EMul. Exp1 ::= Exp1 "*"  Exp2 ;
EDiv. Exp1 ::= Exp1 "/" Exp2 ;
EInt. Exp2 ::= Integer ;
coercions Exp 2 ;
|]