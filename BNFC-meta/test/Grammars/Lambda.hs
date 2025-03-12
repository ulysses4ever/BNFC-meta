{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Grammars.Lambda where

import Language.LBNF.Compiletime
import Language.LBNF(lbnf, bnfc, dumpCode)

bnfc [lbnf|
Abs. Expr ::= "\\" Ident "->" Expr ;
App. Expr1 ::= Expr1 Expr2 ;
Var. Expr2 ::= Ident ;

coercions Expr 2 ;



|]