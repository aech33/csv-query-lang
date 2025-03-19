module CSVHandler (
    Relation,
    readCSV,
    writeCSV,
    loadTable,
    trim
) where

import System.IO
import Data.List (sort)
import Data.Char (isSpace)

-- A relation is a list of rows, each row is a list of string values
type Relation = [[String]]

-- Trim leading and trailing whitespace from a string
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Parse a CSV line into columns, handling whitespace
parseCSVLine :: String -> [String]
parseCSVLine line = map trim (splitOn ',' line)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter str = case break (== delimiter) str of
      (a, _:rest) -> a : splitOn delimiter rest
      (a, []) -> [a]

-- Read a CSV file into a relation
readCSV :: FilePath -> IO Relation
readCSV filePath = do
  content <- readFile filePath
  let rows = map parseCSVLine (lines content)
  return rows

-- Load a table by name (e.g., "A" loads "A.csv")
loadTable :: String -> IO Relation
loadTable tableName = readCSV (tableName ++ ".csv")

-- Write a relation to a CSV file, ensuring lexicographical ordering
writeCSV :: FilePath -> Relation -> IO ()
writeCSV filePath rows = do
  let sortedRows = sort rows  -- Lexicographical ordering
  let content = unlines (map (joinCSVLine) sortedRows)
  writeFile filePath content
  where
    joinCSVLine :: [String] -> String
    joinCSVLine = concat . intersperse ","
    
    intersperse :: a -> [a] -> [a]
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:xs) = x : sep : intersperse sep xs