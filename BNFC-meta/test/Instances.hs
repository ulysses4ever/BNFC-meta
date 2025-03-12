{-# LANGUAGE DeriveGeneric, StandaloneDeriving, TemplateHaskell #-}
module Instances where

import Test.Tasty
import Test.QuickCheck
import Language.LBNF.Grammar
import GHC.Generics
import Test.QuickCheck.Arbitrary.Generic
import Language.Haskell.TH
import Data.List
import Control.Monad
import Language.LBNF
import Test.QuickCheck.Gen

import Grammars.Lambda as Lambda
import Grammars.Arithmetic as Arithmetic
import Grammars.JSON as JSON
import Grammars.CMinusMinus as CMinusMinus
import Grammars.LabelledBNF as LabelledBNF

-- Lambda Calculus
deriving instance Generic Lambda.Ident

instance Arbitrary Lambda.Ident where
  arbitrary = do
    firstChar <- elements (['a'..'z'] ++ ['A'..'Z'])
    restChars <- listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'] ++ ['\'']))
    return (Lambda.Ident (firstChar : restChars))

deriving instance Generic Lambda.Expr

instance Arbitrary Lambda.Expr where
  arbitrary = genericArbitrary
  shrink = genericShrink


-- Arithmetic
deriving instance Generic Arithmetic.Exp

instance Arbitrary Arithmetic.Exp where
  arbitrary = sized genExp

genExp :: Int -> Gen Arithmetic.Exp
genExp 0 = Arithmetic.EInt <$> choose (1, 100)
genExp n = oneof $
  (Arithmetic.EInt <$> choose (1, 100)) :
  map (\cons -> cons <$> genExp half <*> genExp half)
      [Arithmetic.EAdd, Arithmetic.ESub, Arithmetic.EMul, Arithmetic.EDiv]
  where
    half = n `div` 2


-- JSON
deriving instance Generic JSON.Json

instance Arbitrary JSON.Json where
  arbitrary = sized arbitraryJson
  shrink = genericShrink

arbitraryJson :: Int -> Gen JSON.Json
arbitraryJson 0 = oneof
  [ JSON.Num <$> choose (1, 100)
  , JSON.Stri <$> arbitrary
  , return JSON.TBool
  , return JSON.FBool
  ]
arbitraryJson n = oneof
  [ JSON.Num <$> choose (1, 100)
  , JSON.Stri <$> arbitrary
  , return JSON.TBool
  , return JSON.FBool
  , JSON.Con <$> arbitraryContainer (n `div` 2)
  ]

deriving instance Generic JSON.Container

instance Arbitrary JSON.Container where
  arbitrary = sized arbitraryContainer
  shrink = genericShrink

arbitraryContainer :: Int -> Gen JSON.Container
arbitraryContainer n = oneof
  [ JSON.Object <$> listOf (arbitraryMember (n `div` 2))
  , JSON.Array <$> listOf (arbitraryJson (n `div` 2))
  ]

deriving instance Generic JSON.Member

instance Arbitrary JSON.Member where
  arbitrary = sized arbitraryMember
  shrink = genericShrink

arbitraryMember :: Int -> Gen JSON.Member
arbitraryMember n = JSON.MMember <$> arbitrary <*> arbitraryJson (n `div` 2)


-- CMinusMinus
genOptionalSingleton :: Gen a -> Gen [a]
genOptionalSingleton gen = oneof [pure [], (:[]) <$> gen]

genSafeString :: Gen String
genSafeString = listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")

deriving instance Generic CMinusMinus.Program

instance Arbitrary CMinusMinus.Program where
  arbitrary = CMinusMinus.Prog <$> arbitrary

deriving instance Generic CMinusMinus.Function

instance Arbitrary CMinusMinus.Function where
  arbitrary = sized arbitraryFunction

arbitraryFunction :: Int -> Gen CMinusMinus.Function
arbitraryFunction 0 = CMinusMinus.Fun <$> arbitrary <*> arbitrary
                      <*> genOptionalSingleton arbitrary
                      <*> resize 0 (listOf (arbitraryStm $ 0 `div` 2))
arbitraryFunction n = CMinusMinus.Fun <$> arbitrary <*> arbitrary
                      <*> genOptionalSingleton arbitrary
                      <*> resize n (listOf (arbitraryStm $ n `div` 2))

deriving instance Generic CMinusMinus.Decl

instance Arbitrary CMinusMinus.Decl where
  arbitrary = CMinusMinus.Dec <$> arbitrary <*> ((:[]) <$> arbitrary)

deriving instance Generic CMinusMinus.Stm

instance Arbitrary CMinusMinus.Stm where
  arbitrary = sized arbitraryStm

arbitraryStm :: Int -> Gen CMinusMinus.Stm
arbitraryStm 0 = oneof
  [ CMinusMinus.SDecl <$> arbitrary
  , CMinusMinus.SExp <$> arbitraryExp 0
  , CMinusMinus.SBlock <$> resize 0 (listOf (arbitraryStm 0))
  , CMinusMinus.SWhile <$> arbitraryExp 0 <*> arbitraryStm 0
  , CMinusMinus.SReturn <$> arbitraryExp 0
  ]
arbitraryStm n = oneof
  [ CMinusMinus.SDecl <$> arbitrary
  , CMinusMinus.SExp <$> arbitraryExp half
  , CMinusMinus.SBlock <$> resize half (listOf (arbitraryStm half))
  , CMinusMinus.SWhile <$> arbitraryExp half <*> arbitraryStm half
  , CMinusMinus.SReturn <$> arbitraryExp half
  ]
  where
    half = n `div` 2

deriving instance Generic CMinusMinus.Exp

instance Arbitrary CMinusMinus.Exp where
  arbitrary = sized arbitraryExp

arbitraryExp :: Int -> Gen CMinusMinus.Exp
arbitraryExp 0 = oneof
  [ CMinusMinus.EVar <$> arbitrary
  , CMinusMinus.EStr <$> genSafeString
  , CMinusMinus.EInt <$> choose (1, 100)
  , CMinusMinus.EDouble <$> choose (1.0, 100.0)
  ]
arbitraryExp n = oneof
  [ CMinusMinus.EVar <$> arbitrary
  , CMinusMinus.EStr <$> genSafeString
  , CMinusMinus.EInt <$> choose (1, 100)
  , CMinusMinus.EDouble <$> choose (1.0, 100.0)
  , CMinusMinus.EAss <$> arbitrary <*> arbitraryExp half
  , CMinusMinus.ELt <$> arbitraryExp half <*> arbitraryExp half
  , CMinusMinus.EAdd <$> arbitraryExp half <*> arbitraryExp half
  , CMinusMinus.ESub <$> arbitraryExp half <*> arbitraryExp half
  , CMinusMinus.EMul <$> arbitraryExp half <*> arbitraryExp half
  , CMinusMinus.Call <$> arbitrary
         <*> resize half (listOf (arbitraryExp half))
  ]
  where half = n `div` 2

deriving instance Generic CMinusMinus.TType

instance Arbitrary CMinusMinus.TType where
  arbitrary = elements [CMinusMinus.TInt, CMinusMinus.TDouble]

deriving instance Generic CMinusMinus.Ident

instance Arbitrary CMinusMinus.Ident where
  arbitrary = do
    firstChar <- elements (['a'..'z'] ++ ['A'..'Z'])
    restChars <- listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"))
    return (CMinusMinus.Ident (ensureValidIdent (firstChar : restChars)))

ensureValidIdent :: String -> String
ensureValidIdent ident
  | ident == "int" || ident == "double" = "X" ++ ident
  | otherwise = ident


-- LabelledBNF
deriving instance Generic LabelledBNF.Grammar

instance Arbitrary LabelledBNF.Grammar where
  arbitrary = LabelledBNF.MkGrammar <$> listOf arbitrary

deriving instance Generic LabelledBNF.Def

instance Arbitrary LabelledBNF.Def where
  arbitrary = oneof
    [ LabelledBNF.Rule <$> arbitrary <*> arbitrary <*> listOf arbitrary
    , LabelledBNF.Comment <$> genSafeString2
    , LabelledBNF.Comments <$> genSafeString2 <*> genSafeString2
    , LabelledBNF.Internal <$> arbitrary <*> arbitrary <*> listOf arbitrary
    , LabelledBNF.Token <$> arbitrary <*> arbitrary
    , LabelledBNF.PosToken <$> arbitrary <*> arbitrary
    , LabelledBNF.Entryp <$> ((:[]) <$> arbitrary)
    , LabelledBNF.Separator <$> arbitrary <*> arbitrary <*> arbitrary
    , LabelledBNF.Terminator <$> arbitrary <*> arbitrary <*> arbitrary
    , LabelledBNF.Coercions <$> arbitrary <*> choose (1,100)
    , LabelledBNF.Rules <$> arbitrary <*> listOf1 arbitrary
    , LabelledBNF.Layout <$> ((:[]) <$> genSafeString2)
    , LabelledBNF.LayoutStop <$> ((:[]) <$> genSafeString2)
    , pure LabelledBNF.LayoutTop
    ]

deriving instance Generic LabelledBNF.Item

instance Arbitrary LabelledBNF.Item where
  arbitrary = oneof
    [ LabelledBNF.Terminal <$> genSafeString
    , LabelledBNF.NTerminal <$> arbitrary
    ]

deriving instance Generic LabelledBNF.Cat

instance Arbitrary LabelledBNF.Cat where
  arbitrary = oneof
    [ LabelledBNF.ListCat <$> arbitrary
    , LabelledBNF.IdCat <$> arbitrary
    ]

deriving instance Generic LabelledBNF.Label

instance Arbitrary LabelledBNF.Label where
  arbitrary = oneof
    [ LabelledBNF.LabNoP <$> arbitrary
    , LabelledBNF.LabP <$> arbitrary <*> listOf1 arbitrary
    , LabelledBNF.LabPF <$> arbitrary <*> arbitrary <*> listOf1 arbitrary
    , LabelledBNF.LabF <$> arbitrary <*> arbitrary
    ]

deriving instance Generic LabelledBNF.LabelId

instance Arbitrary LabelledBNF.LabelId where
  arbitrary = oneof
    [ LabelledBNF.Id <$> arbitrary
    , pure LabelledBNF.Wild
    , pure LabelledBNF.ListE
    , pure LabelledBNF.ListCons
    , pure LabelledBNF.ListOne
    ]

deriving instance Generic LabelledBNF.ProfItem

instance Arbitrary LabelledBNF.ProfItem where
  arbitrary = LabelledBNF.ProfIt <$> listOf arbitrary
                                          <*> listOf (choose (1, 100))

deriving instance Generic LabelledBNF.IntList

instance Arbitrary LabelledBNF.IntList where
  arbitrary = LabelledBNF.Ints <$> listOf (choose (1, 100))

deriving instance Generic LabelledBNF.RHS

instance Arbitrary LabelledBNF.RHS where
  arbitrary = LabelledBNF.MkRHS <$> listOf arbitrary

deriving instance Generic LabelledBNF.MinimumSize

instance Arbitrary LabelledBNF.MinimumSize where
  arbitrary = oneof
    [ pure LabelledBNF.MNonempty
    , pure LabelledBNF.MEmpty
    ]

deriving instance Generic LabelledBNF.Reg

instance Arbitrary LabelledBNF.Reg where
  arbitrary = sized arbitraryReg

arbitraryReg :: Int -> Gen LabelledBNF.Reg
arbitraryReg 0 = oneof
  [ pure LabelledBNF.REps
  , LabelledBNF.RChar <$> arbitrary
  , LabelledBNF.RAlts <$> arbitrary
  , LabelledBNF.RSeqs <$> arbitrary
  , pure LabelledBNF.RDigit
  , pure LabelledBNF.RLetter
  , pure LabelledBNF.RUpper
  , pure LabelledBNF.RLower
  , pure LabelledBNF.RAny
  ]
arbitraryReg n = oneof
  [ pure LabelledBNF.REps
  , LabelledBNF.RChar <$> arbitrary
  , LabelledBNF.RAlts <$> arbitrary
  , LabelledBNF.RSeqs <$> arbitrary
  , pure LabelledBNF.RDigit
  , pure LabelledBNF.RLetter
  , pure LabelledBNF.RUpper
  , pure LabelledBNF.RLower
  , pure LabelledBNF.RAny
  , LabelledBNF.RSeq <$> arbitraryReg half <*> arbitraryReg half
  , LabelledBNF.RAlt <$> arbitraryReg half <*> arbitraryReg half
  , LabelledBNF.RMinus <$> arbitraryReg half <*> arbitraryReg half
  , LabelledBNF.RStar <$> arbitraryReg half
  , LabelledBNF.RPlus <$> arbitraryReg half
  , LabelledBNF.ROpt <$> arbitraryReg half
  ]
  where half = n `div` 2

deriving instance Generic LabelledBNF.Ident

instance Arbitrary LabelledBNF.Ident where
  arbitrary = do
    firstChar <- elements (['a'..'z'] ++ ['A'..'Z'])
    restChars <- listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"))
    return (LabelledBNF.Ident (ensureValidIdent2 (firstChar : restChars)))

ensureValidIdent2 :: String -> String
ensureValidIdent2 ident
  | ident `elem` ["comment", "internal", "token", "position", "entrypoints", "separator", "terminator", "coercions", "rules", "layout"] = 'X' : ident
  | otherwise = ident

genSafeString2 :: Gen String
genSafeString2 = do
  str <- listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
  return $ ensureValidString str

ensureValidString :: String -> String
ensureValidString str
  | str `elem` ["stop", "toplevel"] = 'X' : str
  | otherwise = map (\c -> if c == '\\' then 'x' else c) str


-- Level 3 
deriving instance Generic Language.LBNF.Grammar.Ident

instance Arbitrary Language.LBNF.Grammar.Ident where
  arbitrary = do
    firstChar <- elements (['A'..'Z'])
    restChars <- listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"))
    return (Language.LBNF.Grammar.Ident (ensureValidIdent2 (firstChar : restChars)))

getNonTerms :: Int -> Gen [Language.LBNF.Grammar.Ident]
getNonTerms n = vectorOf n arbitrary

deriving instance Generic Language.LBNF.Grammar.Def

genRules :: [Language.LBNF.Grammar.Ident] -> Gen [Language.LBNF.Grammar.Def]
genRules idents =
  let rules = concat <$> zipWithM genRule (tails idents) idents
      listRules = mapM genSeparator idents
  in (++) <$> rules <*> listRules

genRule :: [Language.LBNF.Grammar.Ident] -> Language.LBNF.Grammar.Ident -> Gen [Language.LBNF.Grammar.Def]
genRule idents ident =
  let numLabels = choose (0, 4)
      labelledRule = Language.LBNF.Grammar.Rule <$> arbitrary
                                                   <*> pure (Language.LBNF.Grammar.IdCat ident)
                                                   <*> (Language.LBNF.Grammar.RHS <$> genRHS idents)
      labelledRules = numLabels >>= \n -> vectorOf n labelledRule
      terminalRule = Language.LBNF.Grammar.Rule <$> arbitrary
                                                   <*> pure (Language.LBNF.Grammar.IdCat ident)
                                                   <*> (Language.LBNF.Grammar.RHS <$> genTerminals)
  in (:) <$> terminalRule <*> labelledRules

genTerminals :: Gen [Language.LBNF.Grammar.Item]
genTerminals =
  choose (1, 5) >>= \numTerms ->
    vectorOf numTerms $ Language.LBNF.Grammar.Terminal <$> genSafeString

genRHS :: [Language.LBNF.Grammar.Ident] -> Gen [Language.LBNF.Grammar.Item]
genRHS idents =
  choose (1, 5) >>= \numItems ->
    vectorOf numItems $ frequency
      [ (6, Language.LBNF.Grammar.Terminal <$> genSafeString)
      , (3, Language.LBNF.Grammar.NTerminal . Language.LBNF.Grammar.IdCat <$> elements idents)
      , (1, Language.LBNF.Grammar.NTerminal . Language.LBNF.Grammar.ListCat . Language.LBNF.Grammar.IdCat <$> elements idents)
      ]

genListRules :: Language.LBNF.Grammar.Ident -> [Language.LBNF.Grammar.Def]
genListRules ident =
  [ Language.LBNF.Grammar.Rule Language.LBNF.Grammar.ListE
                               (Language.LBNF.Grammar.ListCat (Language.LBNF.Grammar.IdCat ident))
                               (Language.LBNF.Grammar.RHS [])
  , Language.LBNF.Grammar.Rule Language.LBNF.Grammar.ListOne
                               (Language.LBNF.Grammar.ListCat (Language.LBNF.Grammar.IdCat ident))
                               (Language.LBNF.Grammar.RHS
                                  [
                                    Language.LBNF.Grammar.NTerminal (Language.LBNF.Grammar.IdCat ident)
                                  ]
                               )
  , Language.LBNF.Grammar.Rule Language.LBNF.Grammar.ListCons
                               (Language.LBNF.Grammar.ListCat (Language.LBNF.Grammar.IdCat ident))
                               (Language.LBNF.Grammar.RHS
                                  [ Language.LBNF.Grammar.NTerminal (Language.LBNF.Grammar.IdCat ident)
                                  , Language.LBNF.Grammar.NTerminal (Language.LBNF.Grammar.ListCat (Language.LBNF.Grammar.IdCat ident))
                                  ]
                               )
  ]

genSeparator ::Language.LBNF.Grammar.Ident -> Gen Language.LBNF.Grammar.Def
genSeparator ident =
  Language.LBNF.Grammar.Separator
    Language.LBNF.Grammar.MNonempty
      (Language.LBNF.Grammar.IdCat ident)
        <$> genSafeString

deriving instance Generic Language.LBNF.Grammar.Grammar

instance Arbitrary Language.LBNF.Grammar.Grammar where
  arbitrary = Grammar <$> (choose (1, 10) >>= getNonTerms >>= genRules)

deriving instance Generic Language.LBNF.Grammar.Label

instance Arbitrary Language.LBNF.Grammar.Label where
  arbitrary = Language.LBNF.Grammar.Id <$> arbitrary

deriving instance Generic Language.LBNF.Grammar.RHS

instance Arbitrary Language.LBNF.Grammar.RHS where
  arbitrary = Language.LBNF.Grammar.RHS <$> listOf1 arbitrary

deriving instance Generic Language.LBNF.Grammar.Item

instance Arbitrary Language.LBNF.Grammar.Item where
  arbitrary = Language.LBNF.Grammar.Terminal <$> genSafeString

generateGrammar :: Q Language.LBNF.Grammar.Grammar
generateGrammar = runIO $ generate (arbitrary :: Gen Language.LBNF.Grammar.Grammar)

getRules :: Language.LBNF.Grammar.Grammar -> [String]
getRules (Language.LBNF.Grammar.Grammar defs) =
  nub [str | Language.LBNF.Grammar.Rule _ (Language.LBNF.Grammar.IdCat (Language.LBNF.Grammar.Ident str)) _ <- defs]


-- Function to generate Generic and Arbitrary instances for a type
makeInstances :: String -> Q [Dec]
makeInstances typeName = do
    Just tyName <- lookupTypeName typeName
    let ty = conT tyName

    let genericInst = standaloneDerivD (cxt []) (appT (conT ''Generic) ty)

    let arbitraryInst = instanceD (cxt []) (appT (conT ''Arbitrary) ty)
                        [ funD 'arbitrary [clause [] (normalB [| genericArbitrary |]) []]
                        , funD 'shrink [clause [] (normalB [| genericShrink |]) []]
                        ]

    sequence [genericInst, arbitraryInst]

makeAllInstances :: [String] -> Q [Dec]
makeAllInstances typeNames = do
    instances <- mapM makeInstances typeNames
    return $ concat instances

stringToType :: String -> Q Type
stringToType name = do
    Just typeName <- lookupTypeName name
    conT typeName  

stringToFunction :: String -> Q Language.Haskell.TH.Exp 
stringToFunction name = do 
    Just funcName <- lookupValueName name 
    varE funcName

stringToQuoter :: String -> Q Language.Haskell.TH.Exp
stringToQuoter name = do
    Just quoterName <- lookupValueName name
    [| $(varE quoterName) |]
    