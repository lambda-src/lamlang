module Test where

import Test.Tasty
import Test.Tasty.HUnit
import Lang.Data (Token(..), LispObject(..))
import Lang.Lexer (tokenize)
import Lang.Parser (parseProgram)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "LamLang Tests"
    [ lexerTests
    , parserTests
    -- TODO write test cases for eval 
    ]

lexerTests :: TestTree
lexerTests = testGroup "Lexer"
    [ testCase "Tokenize number" $
        tokenize "42" @?= [NumberToken 42]
    , testCase "Tokenize symbol" $
        tokenize "foo" @?= [SymbolToken "foo"]
    , testCase "Tokenize expr" $
        tokenize "(+ 1 2)" @?=
            [LParen, BinaryOpToken "+", NumberToken 1, NumberToken 2, RParen]
    , testCase "Tokenize more complex expr" $
        tokenize "(= (+ 1 2) puts \"meow\" else puts \"grrr\")" @?= 
            [LParen, BinaryOpToken "=", 
                LParen, BinaryOpToken "+", NumberToken 1, NumberToken 2, RParen, KeywordToken "puts", 
                StringToken "meow", KeywordToken "else", KeywordToken "puts", StringToken "grrr", RParen]
    ]

parserTests :: TestTree 
parserTests = testGroup "Parser"
    [ testCase "Parse number" $
        parseProgram "(42)" @?= [List [Number 42]]
    , testCase "Parse lambda" $ 
        parseProgram "(lam (x y) (+ x y))" @?= 
            [Lambda ["x", "y"] 
                [List [BinaryOp "+", Symbol "x", Symbol "y"]]]
    , testCase "Parse program" $
        parseProgram "(def sqr (lam (n) (* n n))) (sqr 10)" @?= 
            [List [Keyword "def",Symbol "sqr",Lambda ["n"] 
                [List [BinaryOp "*",Symbol "n",Symbol "n"]]],
                    List [Symbol "sqr",Number 10]]
    ]

