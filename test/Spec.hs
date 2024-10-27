{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,
    testCase "Query should throw error if empty" $
      Lib2.parseQuery "" @?= (Left "Input cannot be empty"),
    testCase "Query should throw error if input is less than 2 characters" $
      Lib2.parseQuery "o" @?= (Left "Input must be at least 2 characters long")
  ]