import Test.Tasty
import Test.Tasty.HUnit

import Myo
import Foreign.C.String

main = defaultMain tests

tests :: TestTree
tests = testGroup "Myo Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "stringToMacAddress" testStringToMacAddress
  , testCase "MAC roundtrip" testMACRoundtrip
  ]

testStringToMacAddress :: Assertion
testStringToMacAddress = do
	input <- newCString "0A-00-00-00-00-00"
	let res = stringToMacAddress input
	assertBool (show res) (res `compare` 10 == EQ)

testMACRoundtrip :: Assertion
testMACRoundtrip = do
  let mac = "0a-00-00-00-00-00"
  input <- newCString mac
  let res = stringToMacAddress input
  expected <- peekCString $ fromMyoString (macAddressToString res)
  assertBool (show expected) (expected `compare` mac == EQ)
