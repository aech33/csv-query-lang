module CSVHandler (
    Relation,
    readCSV,
    writeCSV,
    loadTable,
    trim,
    CSVError(..)
) where

import Data.List (sort)
import Data.Char (isSpace)
import Control.Exception (Exception, throwIO, try, IOException)
import Debug.Trace (trace)

-- custom exception types for CSV operations
data CSVError = 
    InconsistentColumnCount FilePath Int Int  -- FilePath, expected count, actual count
  | FileNotFound FilePath
  | EmptyCSVFile FilePath
  deriving (Eq)  

instance Show CSVError where
  show (InconsistentColumnCount file expected actual) = 
    "CSV format error in file '" ++ file ++ "': " ++
    "Expected " ++ show expected ++ " columns, but found " ++ show actual ++ " columns. " ++
    "Ensure all rows in your CSV have the same number of columns, separated by commas."
  
  show (FileNotFound file) = 
    "File not found: '" ++ file ++ "'. " ++
    "Check that the file exists and is accessible from the current directory."
  
  show (EmptyCSVFile file) = 
    "Empty CSV file: '" ++ file ++ "'. " ++
    "The file exists but contains no data. Make sure your CSV file has content."

instance Exception CSVError


-- A relation is a list of rows, each row is a list of string values
type Relation = [[String]]

-- Trim leading and trailing whitespace from a string
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Read a CSV file into a relation with correct empty line handling
readCSV :: FilePath -> IO Relation
readCSV filePath = do
  -- Check if file exists by attempting to open it
  result <- try (readFile filePath) :: IO (Either IOException String)
  case result of
    Left _ -> throwIO $ FileNotFound filePath
    Right content -> do
      let allLines = lines content
      
      if null allLines 
        then return []  -- Return empty relation for empty CSV
        else do
          -- Find the first non-empty line to determine expected column count
          let nonEmptyLines = filter (not . all isSpace) allLines
              
              -- If all lines are empty, use 1 as the expected column count
              expectedCols = if null nonEmptyLines 
                             then 1  
                             else countColumns (head nonEmptyLines)
              
              -- Filter and process lines based on arity rules
              validRows = if expectedCols == 1
                          then -- For arity 1, keep all lines including empty ones
                               map (const [""]) (filter (all isSpace) allLines) ++ 
                               map (processLine 1) (filter (not . all isSpace) allLines)
                          else -- For arity > 1, filter out empty lines without enough commas
                               filter (validCommaCount expectedCols) allLines >>=
                               \line -> [processLine expectedCols line]
              
              debug = trace ("File: " ++ filePath ++ 
                            ", Expected cols: " ++ show expectedCols ++ 
                            ", Valid rows: " ++ show (length validRows)) id
          
          -- Return valid rows
          return $ debug validRows

-- Check if a line has enough commas for the expected column count
validCommaCount :: Int -> String -> Bool
validCommaCount expectedCols line
  | all isSpace line = expectedCols - 1 <= countCommas line  -- Empty line needs enough commas
  | otherwise = True  -- Non-empty lines are processed normally and padded if needed

-- Count commas in a string
countCommas :: String -> Int
countCommas = length . filter (== ',')

-- Count the number of columns in a CSV line
countColumns :: String -> Int
countColumns "" = 1  -- Empty line is one column
countColumns s = 1 + countCommas s  -- Count commas and add 1

-- Process a line to have the correct column count
processLine :: Int -> String -> [String]
processLine expectedCols line = 
    let rawFields = splitLine line
        trimmedFields = map trim rawFields
        numFields = length trimmedFields
    in if numFields /= expectedCols then
           error $ "Inconsistent column count: expected " ++ show expectedCols ++
                   " columns, but found " ++ show numFields
       else
           trimmedFields
           
-- Split a line by commas
-- Split a line by commas
splitLine :: String -> [String]
splitLine "" = [""]  -- Empty string becomes a single empty field
splitLine s = 
    -- Special handling for trailing commas
    let result = splitByComma s []
        -- If input ends with comma, add empty field
        finalResult = if last s == ',' then result ++ [""] else result
    in finalResult
  where
    splitByComma :: String -> [String] -> [String]
    splitByComma "" acc = reverse acc  -- End of string, reverse accumulated fields
    splitByComma s acc = 
      case break (== ',') s of
        (field, "")   -> reverse (field : acc)  -- Last field
        (field, rest) -> splitByComma (drop 1 rest) (field : acc)  -- More fields follow
        
-- Load a table by name (e.g., "A" loads "A.csv")
loadTable :: String -> IO Relation
loadTable tableName = readCSV (tableName ++ ".csv")

-- Write a relation to a CSV file, ensuring lexicographical ordering
writeCSV :: FilePath -> Relation -> IO ()
writeCSV filePath rows = do
  let sortedRows = sort rows  -- Lexicographical ordering
  let content = unlines (map joinCSVLine sortedRows)
  writeFile filePath content
  where
    joinCSVLine :: [String] -> String
    joinCSVLine = concat . intersperse ","
    
    intersperse :: a -> [a] -> [a]
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:xs) = x : sep : intersperse sep xs