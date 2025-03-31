{-# LANGUAGE TemplateHaskell, QuasiQuotes, StandaloneDeriving, DeriveGeneric, ScopedTypeVariables, FlexibleInstances, UndecidableInstances #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Monadic (monadicIO, run)

import Language.LBNF.Runtime
import Language.LBNF

import qualified Grammars.Arithmetic as Arithmetic
import qualified Grammars.CMinusMinus as CMinusMinus
import qualified Grammars.LabelledBNF as LabelledBNF
import qualified Grammars.Lambda as Lambda
import qualified Grammars.JSON as JSON
import Language.LBNF.Grammar

import Prelude hiding (exp)
import GHC.Generics (Generic)
import Control.Monad
import System.IO
import Data.List
import Control.Monad.IO.Class (liftIO)
import Control.Exception
import System.FilePath
import System.Directory
import System.Exit
import System.Process
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Instances
import Grammars.GenCode

-- Level One tests are fixed grammars with fixed parsetrees
levelOne :: TestTree
levelOne = testGroup "Level One: Fixed Grammars -> Fixed Parsetrees" [
    -- Lambda Calculus
    testCase "Lambda Grammar" $ do
      let parsed = [Lambda.expr| \ f -> (\ x -> f (x x)) (\ x -> f (x x)) |]
      let Ok result = Lambda.pExpr $ Lambda.myLexer (printTree parsed)
      assertEqual "Fixed Parse is correct" parsed result
    -- Arithmetic
    , testCase "Arithmetic Grammar" $ do
      let parsed = [Arithmetic.exp| 1 + 2 * 3 - 4 / 5|]
      let Ok result = Arithmetic.pExp $ Arithmetic.myLexer (printTree parsed)
      assertEqual "Fixed Parse is correct" parsed result
    -- JSON
    , testCase "JSON Grammar" $ do
      let parsed = [JSON.json|
          {
            "name": "John Doe",
            "age": 30,
            "city": "Exampleville",
            "isStudent": False,
            "grades": [90, 85, 92],
            "address": {
              "street": "123 Main St",
              "zipCode": "12345"
            }
          }
          |]
      let Ok result = JSON.pJson $ JSON.myLexer (printTree parsed)
      assertEqual "Fixed Parse is correct" parsed result
    -- C-- 
    , testCase "C-- Grammar" $ do
      let parsed = [CMinusMinus.program| 
          int mx () { 
              x = 0 ; 
              while ( x < m ) {
                  y = 0 ; 
                  while ( y < n ) { 
                      y = y + 1 ; 
                  } 
                  x = x + 1 ; 
              } 
          }
          |]
      let Ok result = CMinusMinus.pProgram $ CMinusMinus.myLexer (printTree parsed)
      assertEqual "Fixed Parse is correct" parsed result
    -- Labelled BNF
    , testCase "LBNF Grammar" $ do
      let parsed = [LabelledBNF.grammar| 
          Num. Json ::= Integer;
          Stri. Json ::= String ;
          TBool. Json ::= "True" ;
          FBool. Json ::= "False" ;
          Con. Json ::= Container ;
          Object. Container ::= "{" ListMember "}" ;
          Array. Container ::= "[" ListJson "]" ;
          MMember. Member ::= String ":" Json ;

          NilJson. ListJson ::= ;
          SingleJson. ListJson ::= Json ;
          ConsJson. ListJson ::= Json Empty "," Empty ListJson ;

          NilMember. ListMember ::= ;
          SingleMember. ListMember ::= Member ;
          ConsMember. ListMember ::= Member Empty "," ListMember ;
          |]
      let Ok result = LabelledBNF.pGrammar $ LabelledBNF.myLexer (printTree parsed)
      assertEqual "Fixed Parse is correct" parsed result
    ]

-- Level Two tests are fixed grammars with random parsetrees
levelTwo :: TestTree
levelTwo = testGroup "Level Two: Fixed Grammars -> Random Parsetrees" [
    -- Lambda Calculus
    testProperty "Lambda Grammar" $ \parsed ->
      monadicIO $ do
        let result = Lambda.pExpr $ Lambda.myLexer (printTree parsed)
        return $ result == Ok parsed
    -- Arithmetic
    , testProperty "Arithmetic Grammar" $ \parsed ->
      monadicIO $ do
        let result = Arithmetic.pExp $ Arithmetic.myLexer (printTree parsed)
        return $ result == Ok parsed
    -- JSON    
    , testProperty "JSON Grammar" $ \parsed ->
      monadicIO $ do
        let result = JSON.pJson $ JSON.myLexer (printTree parsed)
        return $ result == Ok parsed
    -- C--
    , testProperty "C-- Grammar" $ \parsed ->
      monadicIO $ do
        let result = CMinusMinus.pProgram $ CMinusMinus.myLexer (printTree parsed)
        return $ result == Ok parsed 
    -- Labelled BNF
    , testProperty "LBNF Grammar" $ \parsed ->
      monadicIO $ do
        let result = LabelledBNF.pGrammar $ LabelledBNF.myLexer (printTree parsed)
        return $ result == Ok parsed 
    ]

-- Instances for the the random grammar must be made at compiletime
$(makeAllInstances Grammars.GenCode.rules)

-- Level Three tests a random grammar with random parsetrees
levelThree :: TestTree
levelThree = $(do
  let headRule = head Grammars.GenCode.rules
  headTypeQ <- stringToType headRule
  
  let headParser = head Grammars.GenCode.parsers
  headParserQ <- stringToFunction headParser

  [| testGroup "Level Three: Random Grammar -> Random Parsetrees" [
    testProperty "Random Grammar Test" $ \(parsed :: $(return headTypeQ)) -> monadicIO $ do
      let result = $(return headParserQ) $ Grammars.GenCode.myLexer (printTree parsed)
      return $ result == Ok parsed
    ]|])

-- Dynamically generate the failing antiquote grammar
setupDuplicateAntiquoteTest :: IO FilePath
setupDuplicateAntiquoteTest = do
    let testDir = "test-tmp"
    createDirectoryIfMissing True testDir
    let testFile = testDir </> "DuplicateAntiquote.hs"
    
    writeFile testFile $ unlines
        [ "{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}"
        , "module DuplicateAntiquote where"
        , "import Language.LBNF.Compiletime"
        , "import Language.LBNF (lbnf, bnfc)"
        , ""
        , "do"
        , "    let grammar = [lbnf|"
        , "                    antiquote \"[\" \":\" \":]\" ;"
        , "                    antiquote \"{\" \":\" \":}\" ;"
        , "                    antiquote \"(\" \":\" \":)\" ;"
        , "                    antiquote \"[\" \":\" \":]\" ;"
        , "                  |]"
        , "    bnfc grammar"
        , ""
        ]
    
    return testFile

-- Failure tests invalid grammars to ensure that they fail
failureTests :: TestTree
failureTests = testGroup "Failure Tests"
  [ withResource setupDuplicateAntiquoteTest removeTestFile $ \getFilePath ->
      testCase "Duplicate antiquote fails" $ do
        filePath <- getFilePath
        result <- readProcessWithExitCode
          "cabal"
          ["exec", "ghc", "--", filePath]
          ""
        checkDuplicateAntiquoteError result
  ]

-- Helper to remove the test file after the test
removeTestFile :: FilePath -> IO ()
removeTestFile path = do
  exists <- doesFileExist path
  when exists $ removeFile path
  let dir = takeDirectory path
  dirExists <- doesDirectoryExist dir
  when dirExists $ removeDirectoryRecursive dir

-- Function to check the compiler output
checkDuplicateAntiquoteError :: (ExitCode, String, String) -> Assertion
checkDuplicateAntiquoteError (exitCode, stdout, stderr) = do
  assertBool "Expected compilation to fail" (exitCode /= ExitSuccess)
  assertBool "Expected error about duplicate antiquotes" 
    ("aqSyntax: Multiple antiquote pragmas" `isInfixOf` stderr)

main :: IO ()
main = do
  defaultMain $
    testGroup "BNFC-Meta Tests"
      [ levelOne
      , levelTwo
      , levelThree
      , failureTests
      ]