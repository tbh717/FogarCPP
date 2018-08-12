module Main where

import Parser
import Lexer
import Eval
import System.IO
import Data.Char    -- for isSpace
import Data.Maybe   -- for fromMaybe

{-----------------------
-- REPL FUNCTIONALITY --
------------------------}

main :: IO ()
main = do
    repl (Context {env=[], store=[]})

repl :: Context -> IO ()
repl c = do
    endBool <- isEOF
    if(endBool) 
        then do return ()
        else do
            expr <- getBlock
            if (isCommand expr)
                then do
                    case (stripl expr) of
                        ":quit" -> execCommand c Quit []
                        ":q" -> execCommand c Quit []
                        ":exit" -> execCommand c Quit []
                        ":env" -> execCommand c PrintEnv []
                        ":store" -> execCommand c PrintStore []
                        (':':'e':'x':'p':'r':params) -> execCommand c PrintExpr params
                        (':':'t':'o':'k':'s':params) -> execCommand c PrintToks params
                        _ -> do
                            putStrLn ("Unknown command")
                            repl c
                else do
                    case (scanTokens expr) of 
                        (Just []) -> repl c
                        (Just tokens) -> case (parse tokens) of
                            (Just parsed) -> case (evalS c parsed) of
                                (Just (ans, newC)) -> case ans of
                                    UnitV -> do
                                        repl newC
                                    _ -> do
                                        putStrLn $ show ans
                                        repl newC
                                Nothing -> do
                                    putStrLn "Evaluating error"
                                    repl c
                            Nothing -> do
                                putStrLn "Parsing error"
                                repl c
                        Nothing -> do
                            putStrLn "Lexing error"
                            repl c

getBlock :: IO String
getBlock =
    let aux acc = do
        if isCommand acc
            then return acc
            else do
                line <- getLine
                let acc' = acc++"\n"++line
                if isTerminated line
                    then return acc'
                    else aux acc'
    in aux ""

{-------------
-- COMMANDS --
--------------}

data Command = Quit | PrintEnv | PrintStore | PrintExpr | PrintToks

execCommand :: Context -> Command -> String -> IO ()
execCommand c command params = case command of
    Quit -> return ()
    PrintEnv -> do
        putStrLn $ show (env c)
        repl c
    PrintStore -> do
        putStrLn $ show (store c)
        repl c
    PrintExpr -> do
        let expr = fromMaybe "Invalid expression" (printExpr params)
        putStrLn expr
        repl c
    PrintToks -> do
        let toks = fromMaybe [] (scanTokens params)
        let str = if (toks == []) 
            then "Invalid tokens" 
            else (show toks)
        putStrLn str
        repl c

{--------------------------------------
-- INPUT MANIPULATION & VERIFICAITON --
---------------------------------------}

-- Whitespace stripping
stripl :: String -> String
stripl s = dropWhile (`elem` ['\n',' ','\t']) s
stripr :: String -> String
stripr s = reverse $ dropWhile (isSpace) (reverse s)

isCommand :: String -> Bool
isCommand [] = False
isCommand s = 
    let s' = stripl s
    in case s' of
        [] -> False
        _ -> head s' == ':'

isTerminated :: String -> Bool
isTerminated s = 
    let s' = stripr s
    in case s' of
        "" -> True
        _ -> ((last s') == '?')

{--------------
-- DEBUGGING --
---------------}

printExpr :: String -> (Maybe String)
printExpr s = case (scanTokens s) of
    (Just t) -> case (parse t) of
        (Just p) -> Just $ (show p)
        Nothing -> Just $ "Parsing error"
    Nothing -> Just $ "Lexing error"