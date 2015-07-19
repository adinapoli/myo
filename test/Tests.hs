import Test.Tasty
import Test.Tasty.HUnit

import Myo
import Myo.Foreign.Types

import Control.Monad
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr

main = defaultMain tests

tests :: TestTree
tests = testGroup "Myo Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "stringToMacAddress" testStringToMacAddress
  , testCase "MAC roundtrip" testMACRoundtrip
  , testGroup "Hub tests" [
    testCase "initHub succeeds" testInitHub
  ]
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
  mString <- macAddressToString res
  ms <- fromMyoString mString
  expected <- peekCString ms
  assertBool (show expected) (expected `compare` mac == EQ)

testInitHub :: Assertion
testInitHub = do
 hub <- mallocForeignPtr
 eDetails <- mallocForeignPtr
 aId <- newCString "com.example.hello-world"
 r <- initHub hub aId eDetails
 assertBool (show r) (r == Success)
