module ParserTests (tests) where

import Parser
import Control.Monad
import Text.Printf
import Data.Maybe
import Data.Char

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import Test.HUnit (assertBool)

import Control.Applicative

test :: (Eq a, Show a) => Parser Char a -> String -> a -> String -> Spec
test parser testSet expected expectedTail = do 
  it (show testSet) $ assertBool "" $
                              (Just expectedTail == (snd <$> parsingResult))
                           && (Just expected == (fst <$> parsingResult))
    where parsingResult = (runParser parser testSet)
          result = fst <$> parsingResult
          tail = snd <$> parsingResult
          showMaybe res = maybe "Parsing failed" show res
          message = "Expected " ++ show expected ++ ":" ++ expectedTail ++ "\n"
                 ++ "Actual   " ++ showMaybe result ++ ":" ++ showMaybe tail

accept :: Parser Char a -> String -> Spec
accept parser testSet = do
  it (show testSet) $
    (snd <$> runParser parser testSet) `shouldBe` Just [] 

testFail :: Parser Char a -> String -> Spec 
testFail parser testSet = do
  it (show testSet) $ 
    (snd <$> (runParser parser testSet)) `shouldSatisfy` isNothing

tests :: IO TestTree
tests = testSpec "" $ do
  describe "Parser tests" $ do
    describe "Functor" $ do
      test ((\x -> isSpace x) <$> element 'a') "abc" False "bc"
    describe "Applicative" $ do
      test (pure 10) "abc" 10 "abc"
      test (element 'a' *> pure 10) "abc" 10 "bc"
      testFail (element 'b' *> pure 10) "abc"
      test (element 'c' *> element 'a' *> element 'b') "cabc" 'b' "c"
      testFail (element 'c' *> element 'c' *> element 'a') "cabc"
    describe "Monad" $ do
      test (return 10) "abc" 10 "abc"
      test (element 'a' >> return 10) "abc" 10 "bc"
      testFail (element 'b' >> return 10) "abc"
      test (element 'c' >> element 'a' >> element 'b') "cabc" 'b' "c"
      testFail (element 'c' >> element 'c' >> element 'a') "cabc"
    describe "Alternative" $ do
      testFail empty ""
      testFail empty "123"
      test (element 'c' <|> element 'a') "cabc" 'c' "abc"
      test (element 'a' <|> element 'c') "cabc" 'c' "abc"
      testFail (element 'b' <|> element 'd') "cabc"
      test (ok <|> (element 'c' *> ok)) "cabc" () "cabc"
      test ((element 'c' *> ok) <|> ok) "cabc" () "abc"
      test (eof <|> (stream "cabc" *> eof) <|> eof) "cabc" () ""
      testFail ((element 'b' *> eof) <|> eof) "cabc" 

    describe "ok, eof" $ do
      test ok ""     () ""
      test ok "123"  () "123"
      test ok "    " () "    "
      accept eof "" 
      testFail eof "()" 
    describe "element" $ do
      test     (element 'c') "cabc" 'c' "abc"
      testFail (element 'C') " cabc"
      testFail (element 'd') " cabc"
    describe "stream" $ do
      test (stream "cabc") "cabc" "cabc" ""
      test (stream "c")    "cabc" "c" "abc"
      test (stream "")     "cabc" "" "cabc"
      testFail (stream "abc") "cabc"
      testFail (stream "a")   "cabc"

    describe "Brackets" $ do
      accept brackets "()"
      accept brackets "((()))()()()" 
      accept brackets "(()(()())())(()(())(()))(()())(((())))" 
      accept brackets ""
      testFail brackets "((("
      testFail brackets ")("
      testFail brackets "("
      testFail brackets ")"
      testFail brackets "()a"
      testFail brackets "a()"
      testFail brackets "()()()a"
    describe "Numbers" $ do
      test parseInt "0" 0 ""
      test parseInt "-0" 0 ""
      test parseInt "+0" 0 ""
      test parseInt "1" 1 ""
      test parseInt "+1" 1 ""
      test parseInt "-1" (-1) ""
      test parseInt "123456" 123456 ""
      test parseInt "+123456" 123456 ""
      test parseInt "-123456" (-123456) ""
      test parseInt "123456aa" 123456 "aa"
      test parseInt "+123456aa" 123456 "aa"
      test parseInt "-123456aa" (-123456) "aa"

    describe "Lists" $ do
      test parseListOfLists "0"    [[]]  ""
      test parseListOfLists "1, 1" [[1]] "" 
      test parseListOfLists "1, 1aa" [[1]] "aa" 
      test parseListOfLists "5, 1, 2, -2, +2, 12345, 0, 1, 1" [[1, 2, (-2), 2, 12345], [], [1]] "" 
      test parseListOfLists "5,1,2,-2,+2,12345,0,1,1" [[1, 2, (-2), 2, 12345], [], [1]] "" 
      test parseListOfLists "5,1,2,-2,+2,12345,0,1,1#" [[1, 2, (-2), 2, 12345], [], [1]] "#" 
      test parseListOfLists "2, 1,+10  , 3,5,-7, 2" [ [1, 10], [5, -7, 2] ] ""
     

