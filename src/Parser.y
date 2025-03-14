{
module Parser (
  parseQuery,
  CSVQuery(..),
  Column(..),
  TableRef(..),
  Condition(..)
) where

import Lexer
}

%name parseQuery
%tokentype { Token }
%error { parseError }

%token
  SELECT    { TSelect _ }
  FROM      { TFrom _ }
  WHERE     { TWhere _ }
  CROSS     { TCross _ }
  JOIN      { TJoin _ }
  LEFT      { TLeft _ }
  ON        { TOn _ }
  IS        { TIs _ }
  NOT       { TNot _ }
  EMPTY     { TEmpty _ }
  COALESCE  { TCoalesce _ }
  ident     { TIdent _ $$ }
  string    { TString _ $$ }
  int       { TInteger _ $$ }
  '='       { TEquals _ }
  ','       { TComma _ }
  '('       { TLParen _ }
  ')'       { TRParen _ }
  '.'       { TDot _ }
  '*'       { TStar _ }

%%

-- Query types
Query : SelectQuery              { $1 }

SelectQuery : SELECT ColumnList FROM TableExpr WhereClause  { QSelect $2 $4 $5 }
            | SELECT ColumnList FROM TableExpr              { QSelect $2 $4 Nothing }

-- Column specifications
ColumnList : Column                 { [$1] }
           | ColumnList ',' Column  { $1 ++ [$3] }
           | '*'                    { [CStar] }

Column : ident '.' int             { CColumn $1 $3 }
       | string                    { CString $1 }
       | COALESCE '(' Column ',' Column ')'  { CCoalesce $3 $5 }

-- Table expressions with join types
TableExpr : ident                   { TTable $1 }
          | TableExpr CROSS JOIN ident  { TCrossJoin $1 (TTable $4) }
          | TableExpr LEFT JOIN ident ON JoinCond { TLeftJoin $1 (TTable $4) $6 }

JoinCond : Column '=' Column        { JoinEquals $1 $3 }

-- WHERE clause conditions
WhereClause : WHERE Condition       { Just $2 }

Condition : Column '=' Column       { CEquals $1 $3 }
          | Column IS NOT EMPTY     { CNotEmpty $1 }
          | Column IS EMPTY         { CEmpty $1 }

{
-- Parse error handling
parseError :: [Token] -> a
parseError tokens = error $ case tokens of
  [] -> "Parse error at end of input"
  (t:_) -> "Parse error at " ++ showPos (tokenPos t)
  where
    showPos (Pos line col) = "line " ++ show line ++ ", column " ++ show col
-- Simple AST data types directly mapping to the tasks

-- Main query type
data CSVQuery
  = QSelect [Column] TableRef (Maybe Condition)  -- Basic SELECT query
  deriving (Show, Eq)

-- Column reference or literal
data Column
  = CColumn String Int    -- table.column_number (eg. A.1)
  | CString String        -- String literal (eg. "foo")
  | CStar                 -- * (all columns)
  | CCoalesce Column Column  -- COALESCE(col1, col2) for Task 5
  deriving (Show, Eq)

-- Table references including joins
data TableRef
  = TTable String                     -- Simple table reference
  | TCrossJoin TableRef TableRef      -- Task 1: CROSS JOIN
  | TLeftJoin TableRef TableRef JoinCondition  -- Task 5: LEFT JOIN
  deriving (Show, Eq)

-- Join condition for LEFT JOIN
data JoinCondition
  = JoinEquals Column Column  -- col1 = col2 condition
  deriving (Show, Eq)

-- WHERE clause conditions
data Condition
  = CEquals Column Column     -- col1 = col2 (for Task 2)
  | CEmpty Column             -- col IS EMPTY
  | CNotEmpty Column          -- col IS NOT EMPTY (for Task 3)
  deriving (Show, Eq)

  type Enviroment = [(String,CSVQuery)]
}