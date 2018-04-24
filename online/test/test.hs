{-# OPTIONS_GHC -Wall #-}

module Main where

import Protolude
import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.DocTest

main :: IO ()
main = do
    doctest
      [ "src/Online/Averages.hs"
      , "src/Online/Medians.hs"
      , "src/Online/Reversable.hs"
      ]
    defaultMain tests

tests :: TestTree
tests =
    testGroup ""
    [
    ]

