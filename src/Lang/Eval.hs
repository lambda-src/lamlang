module Lang.Eval (evalProgram) where

import           Lang.Data hiding (Token)
import           Data.Bits (shiftL, shiftR, xor, (.|.), (.&.))
import           Data.Maybe (mapMaybe)
import qualified Data.Map as Map

evalProgram :: Env -> Program -> (LispObject, Env)
evalProgram env [] = (Null, env)
evalProgram env [x] = eval env x
evalProgram env (x:xs) =
    let (_, env') = eval env x
    in evalProgram env' xs


eval :: Env -> LispObject -> (LispObject, Env)
eval env obj = case obj of
    Number _ -> (obj, env)
    Bool _   -> (obj, env)
    String _ -> (obj, env)
    Null     -> (obj, env)
    Symbol n ->
        case Map.lookup n env of
            Just v  -> (v, env)
            Nothing -> error ("Unbound symbol: " ++ n)

    Lambda args body -> (Lambda args body, env)

    BinaryOp op -> (BinaryOp op, env)

    Keyword w -> (Keyword w, env)

    List [] -> (Null, env)

    -- Handles (if ...) (len (...)) etc
    List (Keyword w : xs) -> evalKeyword env w xs

    -- Handles functions
    List (fn : args) ->
        let (fn', env')    = eval env fn
            (args', env'') = evalArgs env' args
        in apply env'' fn' args'

evalArgs :: Env
         -> Program
         -> (Program, Env)
evalArgs env [] = ([], env)
evalArgs env (x:xs) =
    -- This is kinda evil :sob:
    let (x', env')   = eval env x
        (xs', env'') = evalArgs env' xs
    in (x' : xs', env'')

evalBody :: Env
         -> Program
         -> (LispObject, Env)
evalBody env [] = (Null, env)
evalBody env [x] = eval env x
evalBody env (x:xs) =
    let (_, env') = eval env x
    in evalBody env' xs

apply :: Env
      -> LispObject
      -> Program
      -> (LispObject, Env)
apply env (Lambda params body) args
    | length params /= length args = error "Not enough args were passed"
    | otherwise =
        let newEnv = Map.union (Map.fromList $ zip params args) env
        in evalBody newEnv body
apply env (BinaryOp op) args = (evalBinary op args, env)
apply _ fn _ = error ("Cannot apply non function: " ++ show fn)

-- TODO rework this to also handle bools and not just numbers
evalBinary :: String
           -> Program
           -> LispObject
evalBinary op [Number a, Number b] = case op of
    "+"  -> Number (a + b)
    "-"  -> Number (a - b)
    "*"  -> Number (a * b)
    "/"  -> Number (a `div` b)
    "**" -> Number (a ^ b)
    "<<" -> Number (a `shiftL` b)
    ">>" -> Number (a `shiftR` b)
    "^^" -> Number (a `xor` b)
    "&&" -> Number (a .&. b)
    "||" -> Number (a .|. b)
    "/=" -> Bool (a /= b)
    "="  -> Bool (a == b)
    "<"  -> Bool (a < b)
    ">"  -> Bool (a > b)
    _    -> error ("Unknown binary operator: " ++ op)
evalBinary _ args = error ("Binary operator expects two integers: got " ++ show args)

-- This shit ugly as hell brah
evalKeyword :: Env
            -> String
            -> Program
            -> (LispObject, Env)
evalKeyword env keyword args = case keyword of
    "def" -> case args of
        [Symbol n, v] ->
            let (v', _) = eval env v
                env' = Map.insert n v' env
            in (v', env')
        _ -> error "Expected the form: (def name expr)"

    -- TODO test this
    "let" -> case args of
        [List vars, body] ->
            let extend e (List [Symbol n, val]) =
                    let (v, _) = eval env val
                    in Map.insert n v e
                extend _ _ = error "Expected the form: (name value)"
                env' = foldl extend env vars
            in eval env' body
        _ -> error "Expected the form: (let ((t v) ...) body)"

    "lam" -> case args of
        [List args', body] ->
            let names = [ n | Symbol n <- args' ]
            in (Lambda names [body], env)
        _ -> error "lam expects (lam (params...) body)"

    "if" -> case args of
        [cond, thenBranch, elseBranch] ->
            case eval env cond of
                (Bool True, _)  -> eval env thenBranch
                (Bool False, _) -> eval env elseBranch
                _ -> error "A if expression must be of type Bool"
        _ -> error "Expected the form: (if cond then else)"

    "else" -> case args of
        [v] -> eval env v
        _   -> error "Expected the form: (else expr)"

    -- TODO test this
    "when" -> case args of
        [cond, body] ->
            case eval env cond of
                (Bool True, _)  -> eval env body
                (Bool False, _) -> (Null, env)
                _ -> error "A when condition must be of type Bool"
        _ -> error "Expected the form: (when cond body)"

    "unless" -> case args of
        [cond, body] ->
            case eval env cond of
                (Bool False, _) -> eval env body
                (Bool True, _)  -> (Null, env)
                _ -> error "A unless condition must be of type Bool"
        _ -> error "Expected the form: (unless cond body)"

    -- TODO test this
    "cond" ->
        let evalCase (List [test, expr]) =
                case test of
                    Symbol "else" -> Just (eval env expr)
                    _ -> case eval env test of
                            (Bool True, _)  -> Just (eval env expr)
                            (Bool False, _) -> Nothing
                            _ -> error "A condition expression must be of type Bool"
            evalCase _ = error "Expected the form: (test expr)"
        in case mapMaybe evalCase args of
            ((v,_):_) -> (v, env)
            []        -> (Null, env)

    "head" -> case args of
        [List (x:_)] -> (x, env)
        _ -> error "List must be non empty"

    "tail" -> case args of
        [List (_:xs)] -> (List xs, env)
        _ -> error "List must be non empty"

    "nil?" -> case args of
        [List []] -> (Bool True, env)
        [_]       -> (Bool False, env)
        _         -> error "A nil expression must be of type Bool"

    "len" -> case args of
        [List l] -> (Number $ length l, env)
        _ -> error "A len expression must be of type List"

    "puts" -> case args of
        [v] ->
            let (v', _) = eval env v
            in print v' `seq` (v', env)
        _ -> error "A put expression must only have 1 arg"

    _ -> error ("Unknown keyword: " ++ keyword)
