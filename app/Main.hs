module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Exception (catch, SomeException)

import ParserRe
import LexerRe
import CSVHandler
import Interpreter
import RelationalOps

-- Main entry point
main :: IO ()
main = do
    args <- getArgs
    case args of
        [queryFile] -> processQueryFile queryFile
        _ -> do
            putStrLn "Usage: csv-query-language <query-file>"
            exitFailure

-- Process a query file and write results to output.csv
processQueryFile :: FilePath -> IO ()
processQueryFile queryFile = do
    putStrLn $ "Processing query file: " ++ queryFile
    queryStr <- readFile queryFile
    processQuery queryStr `catch` handleCSVError
                         `catch` handleRelOpError
                         `catch` handleInterpreterError
                         `catch` handleGenericError
    where
        handleCSVError :: CSVError -> IO ()
        handleCSVError e = do
            putStrLn $ "CSV Error: " ++ show e
            exitFailure
            
        handleRelOpError :: RelOpError -> IO ()
        handleRelOpError e = do
            putStrLn $ "Relational Operation Error: " ++ show e
            exitFailure
            
        handleInterpreterError :: InterpreterError -> IO ()
        handleInterpreterError e = do
            putStrLn $ "Interpreter Error: " ++ show e
            exitFailure
            
        handleGenericError :: SomeException -> IO ()
        handleGenericError e = do
            putStrLn $ "Error: " ++ show e
            exitFailure

-- Process a query string
processQuery :: String -> IO ()
processQuery queryStr = do
    let tokens = alexScanTokens queryStr
    putStrLn "Lexical analysis completed."
    
    let ast = parseQuery tokens
    putStrLn "Parsing completed."
    
    result <- evalProgram ast
    putStrLn "Query evaluation completed."
    
    writeCSV "output.csv" result
    putStrLn "Results written to output.csv"
-- For testing in GHCI
runQuery :: String -> IO ()
runQuery queryStr = processQuery queryStr