{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Grammars.FailureGrammars where

import Language.LBNF.Compiletime
import Language.LBNF (lbnf, bnfc, dumpCode)
import Instances as I
import Language.Haskell.TH.Syntax
import qualified Language.Haskell.TH as TH
import Control.Exception
import Data.Typeable
import Test.Tasty.HUnit

{-assertFail :: (Exception e, Eq e, Show e) => e -> IO a -> IO ()
assertFail expectedEx action = do
    result <- try action
    case result of
        Left actualEx -> handleException actualEx
        Right _       -> assertFailure "Expected exception"
  where
    handleException actualEx = do
        case (cast actualEx, cast expectedEx) of
            (Just (ErrorCall msg1), Just (ErrorCall msg2)) | msg1 == msg2 -> return ()
            _ | actualEx == expectedEx -> return ()
              | otherwise -> assertFailure "Unexpected exception"-}

$(do
    let grammar = [lbnf|
                    antiquote "[" ":" ":]" ;
                    antiquote "{" ":" ":}" ;
                    antiquote "(" ":" ":)" ;
                    antiquote "[" ":" ":]" ;
                  |]

    (bnfc grammar >>
      reportWarning "Unexpected success: Code generation succeeded" >>
      fail "Test failed: bnfc did not fail as expected") 
      `TH.handleQ` \errorMsg -> do
        reportWarning $ "Error captured as expected: " ++ show errorMsg
        [d| testDummyFunction :: String
            testDummyFunction = "Test passed - error was caught" |]          
    )
