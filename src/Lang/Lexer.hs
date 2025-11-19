module Lang.Lexer (tokenize) where

import Lang.Data (Token(..))
import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf, find)

-- Supported binary ops
binaryOps :: [String] 
-- The longer binary ops need to be first
binaryOps = ["**", "<<", ">>", "&&", "||", "^^", "/=", "<=", ">=", "+", "-", "*", "^", "=", "/", "<", ">"] 

-- Supported keywords
keywords :: [String]
keywords =
    [ "def", "let", "lam" -- Bindings
    , "if", "else", "when", "unless", "cond" -- Conditionals
    , "head", "tail", "nil?", "len" -- List functions
    , "puts" -- Util funcs
    ]

-- To support various sized ops
matchOp :: String -> Maybe (String, String)
matchOp s = case find (`isPrefixOf` s) binaryOps of
    Just op ->
        let rest = drop (length op) s
        in Just (op, rest)
    Nothing -> Nothing

tokenize :: String -> [Token]
tokenize [] = []
tokenize s@(x:xs)
    | isSpace x = tokenize xs
    | x == '('  = LParen : tokenize xs
    | x == ')'  = RParen : tokenize xs
    | x == '"'  = 
        let (string, rest) = span (/= '"') xs
            -- Drop the ending "
            strippedString = drop 1 rest  
        in StringToken string : tokenize strippedString
    | isDigit x =
        let (digits, rest) = span isDigit s
        in NumberToken (read digits) : tokenize rest
    | otherwise =
        case matchOp s of
            Just (op, rest) -> BinaryOpToken op : tokenize rest
            Nothing ->
                let (symbol, rest) =
                        span (\c -> not (isDigit c || isSpace c || c == '(' || c == ')')) s
                in if symbol `elem` keywords
                    then KeywordToken symbol : tokenize rest
                    else SymbolToken symbol : tokenize rest



