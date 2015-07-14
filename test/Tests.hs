import Test.Tasty
import Test.Tasty.HUnit

import Myo.Foreign.String
import Foreign.C.String

main = defaultMain tests

tests :: TestTree
tests = testGroup "Myo Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "stringToMacAddress" testStringToMacAddress
  ]

testStringToMacAddress :: Assertion
testStringToMacAddress = do
	input <- newCString "0A-00-00-00-00-00"
	res <- stringToMacAddress input
	assertBool (show res) (res `compare` 10 == EQ)
