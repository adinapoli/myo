{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Myo
import Myo.Foreign.Types

import Control.Monad
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import Data.Either
import Foreign.Marshal.Alloc

main = defaultMain tests

tests :: TestTree
tests = testGroup "Myo Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "stringToMacAddress" testStringToMacAddress
  , testCase "MAC roundtrip" testMACRoundtrip
  , testGroup "Hub tests" [
      testCase "initHub succeeds" testInitHub
    , testCase "initHub correctly fails for wrong app id" testInitHubWrongAppId
    , testCase "newHub succeeds" testNewHub
    ]
  , testGroup "Myo-specific tests" [
      testCase "run succeeds" testRunMyo
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

testHubFinalise :: Assertion
testHubFinalise = do
 hub <- malloc >>= newForeignPtr freeHub
 aId <- newCString "com.example.hello-world"
 eDetails <- mallocForeignPtr
 _ <- initHub hub aId eDetails
 return ()

testInitHub :: Assertion
testInitHub = do
 hub <- mallocForeignPtr
 eDetails <- mallocForeignPtr
 aId <- newCString "com.example.hello-world"
 r <- initHub hub aId eDetails
 assertBool (show r) (r == Success)

testInitHubWrongAppId :: Assertion
testInitHubWrongAppId = do
 hub <- mallocForeignPtr
 eDetails <- mallocForeignPtr
 aId <- newCString "eoueoue"
 r <- initHub hub aId eDetails
 assertBool (show r) (r == InvalidArgument)

testNewHub :: Assertion
testNewHub = do
 res <- newHub "com.example.hello-world"
 assertBool (show res) (isRight res)

testRunMyo :: Assertion
testRunMyo = return ()
