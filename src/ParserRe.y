{
module ParserRe (
 parseQuery,
  Program(..),
  RelAlgExpr(..),
  ProjectItem(..),
  TableCol(..),
  Condition(..),
  CompOp(..),
  ColExpr(..)  
) where

import LexerRe


}

%name parseQuery
%tokentype { Token }
%error { parseError }

-- Operators precedence
%left or
%left and
%nonassoc not
%nonassoc '=' '<' '>' '<=' '>=' '!='

%token
  project     { TProject _ }
  select      { TSelect _ }
  cartesian   { TCartesian _ }
  join        { TJoin _ }
  rename      { TRename _ }
  union       { TUnion _ }
  difference  { TDifference _ }
  intersect   { TIntersect _ }
  merge       { TMerge _ }
  col         { TCol _ }
  coalesce    { TCoalesce _ }
  star        { TStar _ }
  let         { TLet _ }
  in          { TIn _ }
  empty       { TEmpty _ }
  notEmpty    { TNotEmpty _ }
  and         { TAnd _ }
  or          { TOr _ }
  not         { TNot _ }
  ident       { TIdent _ $$ }
  string      { TString _ $$ }
  int         { TInteger _ $$ }
  '='         { TEquals _ }
  '<'         { TLessThan _ }
  '>'         { TGreaterThan _ }
  '<='        { TLessThanEq _ }
  '>='        { TGreaterThanEq _ }
  '!='        { TNotEquals _ }
  ','         { TComma _ }
  '('         { TLParen _ }
  ')'         { TRParen _ }
  '['         { TLBracket _ }
  ']'         { TRBracket _ }
  '.'         { TDot _ }
  '->'        { TArrow _ }

%%

-- A program can be a single expression or multiple let bindings followed by an expression
Program : Expr                        { SingleExpr $1 }
        | let ident '=' Expr in Program  { LetExpr $2 $4 $6 }

-- Expression types
Expr : TableRef                       { $1 }
     | ProjectExpr                    { $1 }
     | SelectExpr                     { $1 }
     | CartesianExpr                  { $1 }
     | JoinExpr                       { $1 }
     | RenameExpr                     { $1 }
     | UnionExpr                      { $1 }
     | DifferenceExpr                 { $1 }
     | IntersectExpr                  { $1 }
     | MergeExpr                      { $1 }
     | '(' Expr ')'                   { $2 }

-- Simple table reference or variable reference
TableRef : ident                      { if isUpper (head $1) then Table $1 else VarRef $1 }

-- Projection operation: project[items](expr)
ProjectExpr : project '[' ProjectItems ']' '(' Expr ')'  { Project $3 $6 }

-- Selection operation: select[condition](expr)
SelectExpr : select '[' Condition ']' '(' Expr ')'  { Select $3 $6 }

-- Cartesian product with multiple relations: cartesian(expr1, expr2, ...)
CartesianExpr : cartesian '(' ExprList ')'  { CartesianProduct $3 }

-- Join operation: join[condition](expr1, expr2)
JoinExpr : join '[' Condition ']' '(' Expr ',' Expr ')'  { Join $3 $6 $8 }

-- Rename operation: rename[old->new](expr)
RenameExpr : rename '[' ident '->' ident ']' '(' Expr ')'  { Rename $3 $5 $8 }

-- Union operation: union(expr1, expr2)
UnionExpr : union '(' Expr ',' Expr ')'  { Union $3 $5 }

-- Difference operation: difference(expr1, expr2)
DifferenceExpr : difference '(' Expr ',' Expr ')'  { Difference $3 $5 }

-- Intersection operation: intersect(expr1, expr2)
IntersectExpr : intersect '(' Expr ',' Expr ')'  { Intersect $3 $5 }

-- Merge operation: merge(expr1, expr2)
MergeExpr : merge '(' Expr ',' Expr ')'  { Merge $3 $5 }

-- List of expressions for cartesian product
ExprList : Expr                          { [$1] }
          | ExprList ',' Expr            { $1 ++ [$3] }

-- Items to project
ProjectItems : ProjectItem                    { [$1] }
             | ProjectItems ',' ProjectItem   { $1 ++ [$3] }

-- Different kinds of projectable items
ProjectItem : col '(' int ')'                 { Col (TableCol "" $3) }
            | col '(' ident '.' int ')'       { Col (TableCol $3 $5) }
            | string                          { Const $1 }
            | star                            { Star }
            | coalesce '(' ProjectItem ',' ProjectItem ')'  { Coalesce $3 $5 }

-- Conditions for selections and joins
Condition : ColExpr CompOp ColExpr            { Compare $2 $1 $3 }
          | empty '(' ColExpr ')'             { IsEmpty $3 }
          | notEmpty '(' ColExpr ')'          { IsNotEmpty $3 }
          | Condition and Condition           { And $1 $3 }
          | Condition or Condition            { Or $1 $3 }
          | not '(' Condition ')'             { Not $3 }
          | '(' Condition ')'                 { $2 }

-- Column expressions for conditions
ColExpr : col '(' int ')'                     { ColRef (TableCol "" $3) }
        | col '(' ident '.' int ')'           { ColRef (TableCol $3 $5) }
        | string                              { ConstVal $1 }

-- Comparison operators
CompOp : '='                                  { Eq }
       | '<'                                  { Lt }
       | '>'                                  { Gt }
       | '<='                                 { Lte }
       | '>='                                 { Gte }
       | '!='                                 { Neq }

{

showPos :: Pos -> String
showPos (Pos line col) = "line " ++ show line ++ ", column " ++ show col

-- Helper function to show token in a human-readable way
showToken :: Token -> String
showToken (TProject _)       = "project keyword"
showToken (TSelect _)        = "select keyword"
showToken (TCartesian _)     = "cartesian keyword"
showToken (TJoin _)          = "join keyword"
showToken (TRename _)        = "rename keyword"
showToken (TUnion _)         = "union keyword"
showToken (TDifference _)    = "difference keyword"
showToken (TIntersect _)     = "intersect keyword"
showToken (TMerge _)         = "merge keyword"
showToken (TCol _)           = "col function"
showToken (TCoalesce _)      = "coalesce function"
showToken (TStar _)          = "star (*)"
showToken (TLet _)           = "let keyword"
showToken (TIn _)            = "in keyword"
showToken (TEmpty _)         = "empty function"
showToken (TNotEmpty _)      = "notEmpty function"
showToken (TAnd _)           = "and operator"
showToken (TOr _)            = "or operator"
showToken (TNot _)           = "not operator"
showToken (TIdent _ s)       = "identifier '" ++ s ++ "'"
showToken (TString _ s)      = "string \"" ++ s ++ "\""
showToken (TInteger _ n)     = "integer " ++ show n
showToken (TEquals _)        = "equals operator (=)"
showToken (TLessThan _)      = "less than operator (<)"
showToken (TGreaterThan _)   = "greater than operator (>)"
showToken (TLessThanEq _)    = "less than or equal operator (<=)"
showToken (TGreaterThanEq _) = "greater than or equal operator (>=)"
showToken (TNotEquals _)     = "not equals operator (!=)"
showToken (TComma _)         = "comma (,)"
showToken (TSemicolon _)     = "semicolon (;)"
showToken (TLParen _)        = "left parenthesis '('"
showToken (TRParen _)        = "right parenthesis ')'"
showToken (TLBracket _)      = "left bracket '['"
showToken (TRBracket _)      = "right bracket ']'"
showToken (TDot _)           = "dot (.)"
showToken (TArrow _)         = "arrow (->)"

parseError :: [Token] -> a
parseError tokens = error $ case tokens of
  [] -> "Parse error: Unexpected end of input. Your query might be incomplete."
  
  (TInteger p n : ts) | n <= 0 -> 
    "Parse error at " ++ showPos p ++ ": Column indices must be positive. Found: " ++ show n
  
  (TRParen p : _) -> 
    "Parse error at " ++ showPos p ++ ": Unexpected right parenthesis ')'. Check for mismatched parentheses."
  
  (TRBracket p : _) -> 
    "Parse error at " ++ showPos p ++ ": Unexpected right bracket ']'. Check for mismatched brackets."
  
  (TIdent p name : _) | isUpper (head name) && length name > 1 ->
    "Parse error at " ++ showPos p ++ ": Table names should be single uppercase letters (e.g., 'A', 'B')."
    
  (t:_) -> 
    "Parse error at " ++ showPos (tokenPos t) ++ ": Unexpected " ++ showToken t ++ ". " ++
    "Check your syntax and ensure your expressions are properly formed."


-- Helper function to check if a string starts with an uppercase letter
isUpper :: Char -> Bool
isUpper c = c >= 'A' && c <= 'Z'

-- Program structure with let bindings
data Program
  = SingleExpr RelAlgExpr
  | LetExpr String RelAlgExpr Program
  deriving (Show, Eq)

-- AST data types for relational algebra expressions
data RelAlgExpr
  = Table String                              -- Reference to a CSV table (uppercase)
  | VarRef String                             -- Reference to a variable (lowercase)
  | Project [ProjectItem] RelAlgExpr          -- Projection operation (π)
  | Select Condition RelAlgExpr               -- Selection operation (σ)
  | CartesianProduct [RelAlgExpr]             -- N-ary cartesian product (×)
  | Join Condition RelAlgExpr RelAlgExpr      -- Theta join (⨝θ)
  | Rename String String RelAlgExpr           -- Rename operation (ρ)
  | Union RelAlgExpr RelAlgExpr               -- Union (∪)
  | Difference RelAlgExpr RelAlgExpr          -- Difference (-)
  | Intersect RelAlgExpr RelAlgExpr           -- Intersection (∩)
  | Merge RelAlgExpr RelAlgExpr               -- Merge operation (⨝)
  deriving (Show, Eq)

-- Projection items
data ProjectItem
  = Col TableCol       -- A reference to a column
  | Const String       -- A constant string
  | Star               -- All columns (*)
  | Coalesce ProjectItem ProjectItem  -- COALESCE function for Task 5
  deriving (Show, Eq)

-- Table column reference
data TableCol = TableCol String Int  -- Table name and column number (e.g., A.1)
  deriving (Show, Eq)

-- Column expressions for conditions
data ColExpr
  = ColRef TableCol    -- Reference to a column
  | ConstVal String    -- Constant value
  deriving (Show, Eq)

-- Comparison operators for conditions
data CompOp = Eq | Lt | Gt | Lte | Gte | Neq
  deriving (Show, Eq)

-- Condition expressions for selection and joins
data Condition
  = Compare CompOp ColExpr ColExpr  -- col1 op col2
  | IsEmpty ColExpr                 -- col IS EMPTY
  | IsNotEmpty ColExpr              -- col IS NOT EMPTY
  | And Condition Condition         -- cond1 AND cond2
  | Or Condition Condition          -- cond1 OR cond2
  | Not Condition                   -- NOT cond
  deriving (Show, Eq)
}