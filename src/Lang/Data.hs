module Lang.Data where 

import Data.Map (Map)
import Data.List (intercalate)

type Program = [LispObject]

-- Variables are going to be stored as a map where each variables name is the string and the type and value will be stored as LispObj
type Env = Map String LispObject

-- During the lexxing phase raw strings are parsed into tokens
data Token 
    = LParen
    | RParen
    -- The token names were added so they dont clash with lisp object
    | StringToken String
    | NumberToken Int 
    | SymbolToken String
    | BinaryOpToken String
    | KeywordToken String  
    deriving (Show, Eq)

-- During the parsing phase tokens are verified and are parsed into lisp objects 
data LispObject 
    = Null 
    | Symbol String 
    | Number Int
    | Bool Bool 
    | String String
    | BinaryOp String
    | Keyword String
    | Lambda [String] [LispObject]
    | List [LispObject]
    deriving (Eq)

-- For prettier printing
instance Show LispObject where 
    show Null            = "null"
    show (Number n)      = show n
    show (Symbol s)      = s
    show (Bool b)        = show b 
    show (String s)      = "'" ++ s ++ "'"
    show (BinaryOp op)   = op 
    show (Keyword w)     = w 
    show (Lambda args _) = "fun<" ++ intercalate "," args ++ ">"
    show (List l)        = show l