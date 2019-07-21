{-# LANGUAGE OverloadedStrings #-}
module ParseSpec where

import Test.Hspec
import Test.Hspec.Megaparsec

import Text.Megaparsec

import Node
import Parse
import Type

isNumber KNumber{} = True
isNumber _         = False

spec :: Spec
spec = do
  let emptyPosition = Position 0 0 ""
  describe "String Literals" $ do
    let subject = parse stringLiteral ""
    it "correctly parses" $ subject "\"abc\"" `shouldParse` "abc"
    it "handles escape codes" $ subject "\"a\nb\"" `shouldParse` "a\nb"
    it "handles interpolation" $ subject "\"a ${b} c\"" `shouldParse` "a ${b} c"
    it "handles string quotes in string" $ subject "\"a\\\"b\\\"\"" `shouldParse` "a\"b\""
  describe "Number Literals" $ do
    let subject = parse numberLiteral ""
    it "correctly parses" $ subject "5" `shouldParse` 5
    it "correctly parses floating point" $ subject "5.0" `shouldParse` 5.0
  describe "Strings" $ do
    let subject = parse kString ""
    it "correctly parses" $ subject "\"abc\"" `parseSatisfies` (\(KString _ _ x) -> x == "abc")
  describe "Numbers" $ do
    let subject = parse kNumber ""
    it "correctly parses" $ subject "5" `parseSatisfies` (\(KNumber _ _ x) -> x == 5)
  describe "Bools" $ do
    let subject = parse kBool ""
    it "correctly parses" $ subject "false" `parseSatisfies` (\(KBool _ _ x) -> not x)
  describe "Identifiers" $ do
    let subject = parse kIdentifier ""
    it "correctly parses" $ subject "hello" `shouldParse` "hello"
    it "correctly parses upcase" $ subject "Hello" `shouldParse` "Hello"
  describe "variables" $ do
    let subject = parse kVariable ""
    it "correctly parses" $ subject "$hello" `parseSatisfies` (\(KVariable _ _ x) -> x =="hello")
  describe "function calls" $ do
    let subject = parse kCall ""
    it "correctly parses no args" $
      subject "%hello()" `parseSatisfies` (\(KCall _ _ n a) -> n =="hello" && null a)
    it "correctly parses with 1 arg" $
      subject "%hello(1)" `parseSatisfies` (\(KCall _ _ n a) -> n =="hello" && length a == 1)
    it "correctly parses with many args" $
      subject "%hello(1,true,[3,4])" `parseSatisfies` (\(KCall _ _ n a) -> n =="hello" && length a == 3)
  describe "hashes" $ do
    let subject = parse kHash ""
    it "correctly parses" $
      subject "{a: 1}" `parseSatisfies` (\(KHash _ _ x) -> lookup "hello" x)
  describe "lists" $ do
    let subject = parse kList ""
    it "correctly parses" $
      subject "[1,2,3]" `parseSatisfies` (\(KList _ _ xs) -> [1,2,3])
