{
module Lexer (
  Token(..),
  Pos(..),
  alexScanTokens,
  tokenPos
) where

import Data.Char (isSpace)
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z0-9]
{-
Task 1 (Cartesian Product): SELECT * FROM A CROSS JOIN B
Task 2 (Permutation, Drop, Matching): SELECT A.3, A.1 FROM A WHERE A.1 = A.2
Task 3 (Existence Check): SELECT A.1, A.2 FROM A WHERE A.2 IS NOT EMPTY
Task 4 (Copying Constants): SELECT A.1, "foo", A.1 FROM A
Task 5 (Left Merge): SELECT P.1, COALESCE(P.2, Q.2), COALESCE(P.3, Q.3), COALESCE(P.4, Q.4) FROM P LEFT JOIN Q ON P.1 = Q.1
Note we could also add explicit order by; if not ordering will be handled in interpreter
or we could also add Group
-}
tokens :-
  $white+                       ;  -- ignore whitespace
  "--".*                        ;  -- ignore comments

  -- Core SQL keywords
  SELECT                        { \p s -> TSelect (getPos p) } --what column to retrieve
  FROM                          { \p s -> TFrom (getPos p) } --specifying the source
  WHERE                         { \p s -> TWhere (getPos p) } -- any filtering options
  AS                            { \p s -> TAs (getPos p) } -- aliasing any tables or coulmns
  
  -- Join operations
  JOIN                          { \p s -> TJoin (getPos p) } --the generic join operation
  ON                            { \p s -> TOn (getPos p) } -- specifes the above join condition
  CROSS                         { \p s -> TCross (getPos p) } -- cartesian product -> Task 1
  LEFT                          { \p s -> TLeft (getPos p) } --left outer join --> Task 5
  
  -- Relational algebra operations
  PROJECT                       { \p s -> TProject (getPos p) } --to select specific columns like SQL SELECT but no filtering
  UNION                         { \p s -> TUnion (getPos p) } -- to combine rows from two relations
  INTERSECT                     { \p s -> TIntersect (getPos p) } -- to keep rows present in both relations
  EXCEPT                        { \p s -> TExcept (getPos p) } -- set any differences. Eg. rows in first relation and not in second
  RENAME                        { \p s -> TRename (getPos p) } -- to change column names
  
  -- Value handling
  COALESCE                      { \p s -> TCoalesce (getPos p) } -- to get first non empty values -> Task 5
  CONCAT                        { \p s -> TConcat (getPos p) } -- to combine values from two columns
  IS                            { \p s -> TIs (getPos p) } --check for null or empty values --> Task 3
  NULL                          { \p s -> TNull (getPos p) } -- same as above
  EMPTY                         { \p s -> TEmpty (getPos p) }-- same as above
  
  -- Logical operators
  NOT                           { \p s -> TNot (getPos p) }
  AND                           { \p s -> TAnd (getPos p) }
  OR                            { \p s -> TOr (getPos p) }
  
  -- Identifiers and literals
  $alpha [$alphanum \_ \']*     { \p s -> TIdent (getPos p) s }
  \" [^\"]* \"                  { \p s -> TString (getPos p) (stripQuotes s) }
  $digit+                       { \p s -> TInteger (getPos p) (read s) }
  
  -- Operators and punctuation
  \=                            { \p s -> TEquals (getPos p) }
  \!\=                          { \p s -> TNotEquals (getPos p) }
  \,                            { \p s -> TComma (getPos p) }
  \(                            { \p s -> TLParen (getPos p) }
  \)                            { \p s -> TRParen (getPos p) }
  \;                            { \p s -> TSemicolon (getPos p) }
  \.                            { \p s -> TDot (getPos p) } --member access operator like A.1
  \*                            { \p s -> TStar (getPos p) } --to select all
  \|                            { \p s -> TPipe (getPos p) }

{
-- Position data type
data Pos = Pos Int Int  -- line, column numbers
  deriving (Eq, Show)

getPos :: AlexPosn -> Pos --converts the alex positions to our custom one
getPos (AlexPn _ line col) = Pos line col

stripQuotes :: String -> String --retrieving the string from the literal without the quotes
stripQuotes s = init (tail s)

-- Token definitions
data Token
  = TSelect Pos
  | TFrom Pos
  | TWhere Pos
  | TAs Pos
  | TJoin Pos
  | TOn Pos
  | TCross Pos
  | TLeft Pos
  | TProject Pos
  | TUnion Pos
  | TIntersect Pos
  | TExcept Pos
  | TRename Pos
  | TCoalesce Pos
  | TConcat Pos
  | TIs Pos
  | TNull Pos
  | TEmpty Pos
  | TNot Pos
  | TAnd Pos
  | TOr Pos
  | TIdent Pos String
  | TString Pos String
  | TInteger Pos Int
  | TEquals Pos
  | TNotEquals Pos
  | TComma Pos
  | TLParen Pos
  | TRParen Pos
  | TSemicolon Pos
  | TDot Pos
  | TStar Pos
  | TPipe Pos
  deriving (Eq, Show)

-- Extract position from a token
tokenPos :: Token -> Pos
tokenPos (TSelect p)    = p
tokenPos (TFrom p)      = p
tokenPos (TWhere p)     = p
tokenPos (TAs p)        = p
tokenPos (TJoin p)      = p
tokenPos (TOn p)        = p
tokenPos (TCross p)     = p
tokenPos (TLeft p)      = p
tokenPos (TProject p)   = p
tokenPos (TUnion p)     = p
tokenPos (TIntersect p) = p
tokenPos (TExcept p)    = p
tokenPos (TRename p)    = p
tokenPos (TCoalesce p)  = p
tokenPos (TConcat p)    = p
tokenPos (TIs p)        = p
tokenPos (TNull p)      = p
tokenPos (TEmpty p)     = p
tokenPos (TNot p)       = p
tokenPos (TAnd p)       = p
tokenPos (TOr p)        = p
tokenPos (TIdent p _)   = p
tokenPos (TString p _)  = p
tokenPos (TInteger p _) = p
tokenPos (TEquals p)    = p
tokenPos (TNotEquals p) = p
tokenPos (TComma p)     = p
tokenPos (TLParen p)    = p
tokenPos (TRParen p)    = p
tokenPos (TSemicolon p) = p
tokenPos (TDot p)       = p
tokenPos (TStar p)      = p
tokenPos (TPipe p)      = p
}