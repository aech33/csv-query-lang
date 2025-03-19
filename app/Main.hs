module Main where

import System.Environment (getArgs)
import System.IO
import System.Exit (exitFailure)
import Control.Exception (catch, SomeException)

import ParserRe
import LexerRe
import CSVHandler
import Interpreter

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
    processQuery queryStr `catch` handleError
    where
        handleError :: SomeException -> IO ()
        handleError e = do
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