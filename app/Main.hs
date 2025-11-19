module Main where

import           Lang.Parser (parseProgram)
import           Lang.Eval (evalProgram)
import           Lang.Data (Env)
import           System.IO (hFlush, stdout)
import qualified Data.Map as Map

main :: IO ()
main = putStrLn "Shitisp lang repl type :quit to give up" >> repl Map.empty 

repl :: Env -> IO ()
repl env = do 
    putStr "λ> " 
    hFlush stdout
    line <- getLine 
    if line == ":quit"
        then putStrLn "Thanks for trying my thing!"
        else do 
            let program = parseProgram line 
            let (res, env') = evalProgram env program
            print res 
            repl env'

