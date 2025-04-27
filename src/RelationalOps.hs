module RelationalOps (
    projectRelation,
    selectRelation,
    cartesianProduct,
    renameColumn,
    mergeRelations,
    evaluateProjectItem,
    evaluateCondition,
    evalColExpr,
    RelOpError(..)
) where

import ParserRe
import CSVHandler (Relation, trim)
import Data.List (intercalate)
import Control.Exception (Exception, throw)
import Debug.Trace (trace)

data RelOpError = 
    ColumnOutOfBounds Int Int String  -- requested column index, max column index, operation context
  | InvalidColumnReference String
  | OperationError String
  deriving (Eq)  

instance Exception RelOpError

instance Show RelOpError where
  show (ColumnOutOfBounds col maxCol context) = 
    "Column reference error " ++ context ++ ": Column " ++ show col ++ 
    " is out of bounds (valid columns are 1 to " ++ show maxCol ++ "). " ++
    "Check that your column references match the schema of your data."
  
  show (InvalidColumnReference msg) = 
    "Invalid column reference: " ++ msg ++ 
    ". Ensure column references use valid table names and column indices."
  
  show (OperationError msg) = 
    "Operation error: " ++ msg ++
    ". This usually happens when an operation cannot be performed on the given data."


projectRelation :: [ProjectItem] -> Relation -> Relation
projectRelation items relation = 
    let 
        -- Get max columns from schema or throw error if relation is empty
        maxCols = if null relation
                 then throw $ OperationError "Cannot project empty relation with column references"
                 else length (head relation)
        
        -- Validate all items regardless of row count
        validateItem (Col (TableCol _ n)) = 
            if n <= 0 || n > maxCols then
                throw $ ColumnOutOfBounds n maxCols "in validateItem"
            else True
        validateItem _ = True
    in
    -- Only proceed if validation passes
    if all validateItem items
    then map (\row -> map (\item -> evaluateProjectItem item relation row) items) relation
    else []
           
-- Evaluate a projection item against a row
evaluateProjectItem :: ProjectItem -> Relation -> [String] -> String
evaluateProjectItem item relation row = case item of
    Col (TableCol _ n) -> 
        if n <= 0 || n > length row 
        then throw $ ColumnOutOfBounds n (length row) "in evaluateProjectItem"
        else row !! (n-1)
    Const str -> str
    Star -> intercalate "," row
    Coalesce item1 item2 -> 
        let val1 = evaluateProjectItem item1 relation row
        in if val1 /= "" 
           then val1 
           else evaluateProjectItem item2 relation row

-- Select rows from a relation based on a condition
selectRelation :: Condition -> Relation -> Relation
selectRelation cond relation = filter (evaluateCondition cond relation) relation

-- Evaluate a condition against a row
evaluateCondition :: Condition -> Relation -> [String] -> Bool
evaluateCondition cond relation row = case cond of
    Compare op expr1 expr2 -> compareValues op 
                              (evalColExpr expr1 relation row)
                              (evalColExpr expr2 relation row)
    IsEmpty expr -> evalColExpr expr relation row == ""
    IsNotEmpty expr -> evalColExpr expr relation row /= ""
    And cond1 cond2 -> evaluateCondition cond1 relation row && 
                       evaluateCondition cond2 relation row
    Or cond1 cond2 -> evaluateCondition cond1 relation row || 
                      evaluateCondition cond2 relation row
    Not innerCond -> not (evaluateCondition innerCond relation row)

-- Evaluate a column expression to get its value
evalColExpr :: ColExpr -> Relation -> [String] -> String
evalColExpr expr relation row = case expr of
    ColRef (TableCol _ n) -> if n <= length row && n > 0 
                            then row !! (n-1) 
                            else throw $ ColumnOutOfBounds n (length row) "in evalColExpr"
    ConstVal str -> str

-- Compare values based on the comparison operator
compareValues :: CompOp -> String -> String -> Bool
compareValues op val1 val2 = case op of
    Eq -> val1 == val2
    Lt -> val1 < val2
    Gt -> val1 > val2
    Lte -> val1 <= val2
    Gte -> val1 >= val2
    Neq -> val1 /= val2

-- Perform Cartesian product of multiple relations
cartesianProduct :: [Relation] -> Relation
cartesianProduct [] = [[]]  -- Empty product is a single empty row
cartesianProduct [rel] = rel
cartesianProduct (rel:rels) = 
    [row1 ++ row2 | row1 <- rel, row2 <- cartesianProduct rels]

mergeRelations :: Relation -> Relation -> Relation
mergeRelations rel1 rel2 = 
    [row1 ++ row2 | (row1,row2) <- zip rel1 rel2]  -- Merge rows from both relations

-- Rename a column in a relation schema (this doesn't affect the data,
-- just how we refer to it in subsequent operations)
renameColumn :: String -> String -> Relation -> Relation
renameColumn _ _ relation = relation  -- For now, just pass through as we don't track column names

validateColumnReference :: String -> Relation -> Bool
validateColumnReference colNum relation =
    let maxCols = if null relation then 0 else length (head relation)
        parsedColNum = reads colNum :: [(Int, String)]
    in case parsedColNum of
        [(colNumInt, "")] -> True
        _ -> error $ show (InvalidColumnReference colNum)
