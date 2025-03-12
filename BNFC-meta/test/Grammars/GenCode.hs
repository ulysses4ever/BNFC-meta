{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Grammars.GenCode where

import Language.LBNF.Compiletime
import Language.LBNF (lbnf, bnfc, dumpCode)
import Instances as I
import Language.Haskell.TH.Syntax

-- Template Haskell splice to generate a grammar at compile time and store the rules and parser names
$(do
    grammar <- I.generateGrammar
    code <- bnfc grammar
    let dumpedDecls = code

    let rules = map ("Grammars.GenCode." ++) (I.getRules grammar)
    let parsers = map ("Grammars.GenCode.p" ++) (I.getRules grammar)
    rulesExp <- lift rules
    parsersExp <- lift parsers

    return (dumpedDecls ++ 
            [ ValD (VarP (mkName "rules")) (NormalB rulesExp) []
            , ValD (VarP (mkName "parsers")) (NormalB parsersExp) []
            ]) 
    )