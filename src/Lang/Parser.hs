module Lang.Parser (parseProgram) where 

import Lang.Data hiding (Env)
import Lang.Lexer

parseProgram :: String -> Program
parseProgram = parse . tokenize

parse :: [Token] -> Program
parse [] = []
parse tokens =
    let (expr, rest) = parseExpr tokens
    in expr : parse rest

parseExpr :: [Token] -> (LispObject, [Token])
parseExpr [] = error "Got unexpected EOF"
parseExpr (RParen : _) = error "Unexpected closing parenthesis"
parseExpr (LParen : rest) = parseList rest []
parseExpr (NumberToken n : rest)   = (Number n, rest)
parseExpr (SymbolToken s : rest)   = (Symbol s, rest)
parseExpr (StringToken s : rest)   = (String s, rest)
parseExpr (KeywordToken s : rest)  = (Keyword s, rest)
parseExpr (BinaryOpToken o : rest) = (BinaryOp o, rest)

parseList :: [Token] 
          -> Program 
          -> (LispObject, [Token])
parseList [] _ = error "No closing ')'"
parseList (RParen : rest) acc = (List (reverse acc), rest)
-- Parse every item in the list unless its a lambda in which case parse it with parseLam
parseList tokens acc = case tokens of 
    (KeywordToken "lam" : rest) -> parseLam rest acc 
    _ -> 
        let (expr, rest) = parseExpr tokens
        in parseList rest (expr : acc)

parseLam :: [Token] 
         -> Program 
         -> (LispObject, [Token])
parseLam (LParen : rest) _ =
    -- For lambdas parse the args list then the body
    let (args, afterArgs) = parseArgs rest []
        (body, afterBody) = parseBody afterArgs []
    in (Lambda args body, afterBody)
parseLam _ _ = error "Expected list after 'lam'"

parseArgs :: [Token] 
          -> [String] 
          -> ([String], [Token])
parseArgs [] _ = error "Excpected a closing ) arg list in lambda"
parseArgs (RParen : rest) acc = (reverse acc, rest)
parseArgs (SymbolToken s : rest) acc = parseArgs rest (s : acc)
parseArgs _ _ = error "Lambda args must be symbols like: x, y, num, etc"

parseBody :: [Token] 
          -> Program 
          -> (Program, [Token])
parseBody [] _ = error "Expected a closing ) to the lambda body"
parseBody (RParen : rest) acc = (reverse acc, rest)
parseBody tokens acc =
    let (expr, rest) = parseExpr tokens
    in parseBody rest (expr : acc)