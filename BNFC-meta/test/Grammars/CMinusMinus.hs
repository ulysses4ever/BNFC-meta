{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Grammars.CMinusMinus where

import Language.LBNF.Compiletime
import Language.LBNF(lbnf, bnfc, dumpCode)

-- C--
bnfc [lbnf|
comment "//" ;
comment "/*" "*/" ;

Prog. Program  ::= [Function] ;
Fun.  Function ::= TType Ident "(" [Decl] ")" "{" [Stm] "}" ;
Dec.  Decl     ::= TType [Ident] ; 

terminator Function "" ;
terminator Stm "" ;
separator  Decl "," ;
separator  nonempty Ident "," ;

SDecl.   Stm ::= Decl ";" ;
SExp.    Stm ::= Exp ";" ;
SBlock.  Stm ::= "{" [Stm] "}" ;
SWhile.  Stm ::= "while" "(" Exp ")" Stm;
SReturn. Stm ::= "return" Exp  ";" ;

EAss.    Exp  ::= Ident "=" Exp ;
ELt.     Exp1 ::= Exp2 "<" Exp2 ;
EAdd.    Exp2 ::= Exp2 "+" Exp3 ;
ESub.    Exp2 ::= Exp2 "-" Exp3 ;
EMul.    Exp3 ::= Exp3 "*" Exp4 ;
Call.    Exp4 ::= Ident "(" [Exp] ")" ;
EVar.    Exp4 ::= Ident ;
EStr.    Exp4 ::= String ;
EInt.    Exp4 ::= Integer ; 
EDouble. Exp4 ::= Double ;

coercions Exp 4 ;

separator Exp "," ;

TInt.    TType ::= "int" ;
TDouble. TType ::= "double" ;
|]