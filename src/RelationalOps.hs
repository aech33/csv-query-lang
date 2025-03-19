module RelationalOps (
    projectRelation,
    selectRelation,
    cartesianProduct,
    renameColumn,
    evaluateProjectItem,
    evaluateCondition,
    evalColExpr
) where

import ParserRe
import CSVHandler (Relation, trim)
import Data.List (intercalate)

-- Project specified columns from a relation
projectRelation :: [ProjectItem] -> Relation -> Relation
projectRelation items relation = 
    -- For each row in the relation, create a new row by evaluating each projection item
    map (\row -> map (\item -> evaluateProjectItem item relation row) items) relation

-- Evaluate a projection item against a row
evaluateProjectItem :: ProjectItem -> Relation -> [String] -> String
evaluateProjectItem item relation row = case item of
    Col (TableCol _ n)          -> if n <= length row && n > 0 
                                   then row !! (n-1) 
                                   else ""
    Const str                   -> str
    Star                        -> intercalate "," row
    Coalesce item1 item2        -> let val1 = evaluateProjectItem item1 relation row
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
    IsEmpty expr          -> evalColExpr expr relation row == ""
    IsNotEmpty expr       -> evalColExpr expr relation row /= ""
    And cond1 cond2       -> evaluateCondition cond1 relation row && 
                             evaluateCondition cond2 relation row
    Or cond1 cond2        -> evaluateCondition cond1 relation row || 
                             evaluateCondition cond2 relation row
    Not innerCond         -> not (evaluateCondition innerCond relation row)

-- Evaluate a column expression to get its value
evalColExpr :: ColExpr -> Relation -> [String] -> String
evalColExpr expr relation row = case expr of
    ColRef (TableCol _ n) -> if n <= length row && n > 0 
                            then row !! (n-1) 
                            else ""
    ConstVal str         -> str

-- Compare values based on the comparison operator
compareValues :: CompOp -> String -> String -> Bool
compareValues op val1 val2 = case op of
    Eq  -> val1 == val2
    Lt  -> val1 < val2
    Gt  -> val1 > val2
    Lte -> val1 <= val2
    Gte -> val1 >= val2
    Neq -> val1 /= val2

-- Perform Cartesian product of multiple relations
cartesianProduct :: [Relation] -> Relation
cartesianProduct [] = [[]]  -- Empty product is a single empty row
cartesianProduct [rel] = rel
cartesianProduct (rel:rels) = 
    [row1 ++ row2 | row1 <- rel, row2 <- cartesianProduct rels]

-- Rename a column in a relation schema (this doesn't affect the data,
-- just how we refer to it in subsequent operations)
renameColumn :: String -> String -> Relation -> Relation
renameColumn _ _ relation = relation  -- For now, just pass through as we don't track column names