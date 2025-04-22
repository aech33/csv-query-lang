{
module LexerRe (
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

tokens :-
  $white+                       ;  -- ignore whitespace
  "--".*                        ;  -- ignore comments

  -- Keywords must come BEFORE identifiers
  "project"                     { \p s -> TProject (getPos p) }
  "select"                      { \p s -> TSelect (getPos p) }
  "cartesian"                   { \p s -> TCartesian (getPos p) }
  "join"                        { \p s -> TJoin (getPos p) }
  "rename"                      { \p s -> TRename (getPos p) }
  "union"                       { \p s -> TUnion (getPos p) }
  "difference"                  { \p s -> TDifference (getPos p) }
  "intersect"                   { \p s -> TIntersect (getPos p) }
  "merge"                       { \p s -> TMerge (getPos p) }
  "col"                         { \p s -> TCol (getPos p) }
  "coalesce"                    { \p s -> TCoalesce (getPos p) }
  "star"                        { \p s -> TStar (getPos p) }
  "let"                         { \p s -> TLet (getPos p) }
  "in"                          { \p s -> TIn (getPos p) }
  "empty"                       { \p s -> TEmpty (getPos p) }
  "notEmpty"                    { \p s -> TNotEmpty (getPos p) }
  "and"                         { \p s -> TAnd (getPos p) }
  "or"                          { \p s -> TOr (getPos p) }
  "not"                         { \p s -> TNot (getPos p) }
  
  -- Identifiers and literals
  $alpha [$alphanum \_ \']*     { \p s -> TIdent (getPos p) s }
  \" [^\"]* \"                  { \p s -> TString (getPos p) (stripQuotes s) }
  $digit+                       { \p s -> TInteger (getPos p) (read s) }
  
  -- Operators and punctuation (explicit characters)
  "="                           { \p s -> TEquals (getPos p) }
  "<"                           { \p s -> TLessThan (getPos p) }
  ">"                           { \p s -> TGreaterThan (getPos p) }
  "<="                          { \p s -> TLessThanEq (getPos p) }
  ">="                          { \p s -> TGreaterThanEq (getPos p) }
  "!="                          { \p s -> TNotEquals (getPos p) }
  ","                           { \p s -> TComma (getPos p) }
  ";"                           { \p s -> TSemicolon (getPos p) }
  "("                           { \p s -> TLParen (getPos p) }
  ")"                           { \p s -> TRParen (getPos p) }
  "["                           { \p s -> TLBracket (getPos p) }
  "]"                           { \p s -> TRBracket (getPos p) }
  "."                           { \p s -> TDot (getPos p) }
  "->"                          { \p s -> TArrow (getPos p) }
{
-- Position data type
data Pos = Pos Int Int  -- line, column numbers
  deriving (Eq, Show)

getPos :: AlexPosn -> Pos
getPos (AlexPn _ line col) = Pos line col

stripQuotes :: String -> String
stripQuotes s = init (tail s)

-- Token definitions
data Token
  = TProject Pos
  | TSelect Pos
  | TCartesian Pos
  | TJoin Pos
  | TRename Pos
  | TUnion Pos
  | TDifference Pos
  | TIntersect Pos
  | TMerge Pos
  | TCol Pos
  | TCoalesce Pos
  | TStar Pos
  | TLet Pos
  | TIn Pos
  | TEmpty Pos
  | TNotEmpty Pos
  | TAnd Pos
  | TOr Pos
  | TNot Pos
  | TIdent Pos String
  | TString Pos String
  | TInteger Pos Int
  | TEquals Pos
  | TLessThan Pos
  | TGreaterThan Pos
  | TLessThanEq Pos
  | TGreaterThanEq Pos
  | TNotEquals Pos
  | TComma Pos
  | TSemicolon Pos
  | TLParen Pos
  | TRParen Pos
  | TLBracket Pos
  | TRBracket Pos
  | TDot Pos
  | TArrow Pos
  deriving (Eq, Show)

-- Extract position from a token
tokenPos :: Token -> Pos
tokenPos (TProject p)       = p
tokenPos (TSelect p)        = p
tokenPos (TCartesian p)     = p
tokenPos (TJoin p)          = p
tokenPos (TRename p)        = p
tokenPos (TUnion p)         = p
tokenPos (TDifference p)    = p
tokenPos (TIntersect p)     = p
tokenPos (TCol p)           = p
tokenPos (TCoalesce p)      = p
tokenPos (TStar p)          = p
tokenPos (TLet p)           = p
tokenPos (TIn p)            = p
tokenPos (TEmpty p)         = p
tokenPos (TNotEmpty p)      = p
tokenPos (TAnd p)           = p
tokenPos (TOr p)            = p
tokenPos (TNot p)           = p
tokenPos (TIdent p _)       = p
tokenPos (TString p _)      = p
tokenPos (TInteger p _)     = p
tokenPos (TEquals p)        = p
tokenPos (TLessThan p)      = p
tokenPos (TGreaterThan p)   = p
tokenPos (TLessThanEq p)    = p
tokenPos (TGreaterThanEq p) = p
tokenPos (TNotEquals p)     = p
tokenPos (TComma p)         = p
tokenPos (TSemicolon p)     = p
tokenPos (TLParen p)        = p
tokenPos (TRParen p)        = p
tokenPos (TLBracket p)      = p
tokenPos (TRBracket p)      = p
tokenPos (TDot p)           = p
tokenPos (TArrow p)         = p
}