{
-- alexScanTokens
module Token where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
@ops = ("||"|"&&"|"+"|"-"|"*"|"/"|"++"|"::"|"%"|"<"|">"|"=="|";")

tokens :-

  $white+                        ;
  "--".*                         ;
  def                            {\s -> Def}
  "@"                            {\s -> At}
  ":="                           {\s -> DefEq}
  if                             {\s -> If}
  then                           {\s -> Then}
  else                           {\s -> Else}
  fun                            {\s -> Fun}
  "=>"                           {\s -> FunTo}
  "#"                            {\s -> ConsLead}
  "<-"                           {\s -> LArrow}
  let                            {\s -> Let}
  in                             {\s -> In}
  match                          {\s -> Match}
  with                           {\s -> With}
  end                            {\s -> End}
  true                           {\s -> TTrue}
  false                          {\s -> TFalse}
  \(                             {\s -> ParenL}
  \)                             {\s -> ParenR}
  \[                             {\s -> BracketL}
  \]                             {\s -> BracketR}
  \{                             {\s -> BraceL}
  \}                             {\s -> BraceR}
  \,                             {\s -> Comma}
  $digit+                        {Int . read}
  $digit+\.$digit+               {Float . read}
  @ops                           {Op}
  \|                             {\s -> Bar}
  \"[^\"]*\"                     {String . init . tail }  
  forall                         {\s -> Forall}
  exist                          {\s -> Exist}
  for                            {\s -> For}
  ":"                            {\s -> Anno}
  $alpha [$alpha $digit \_ \']*  {Id}
  

{
-- Each action has type :: String -> Token

-- The token type:
data Token
  = Def | DefEq
  | If  | Then | Else
  | Fun | FunTo 
  | Let | In 
  | Match | With | Bar | End  
  | Op String 
  | Id String 
  | Int Int
  | Float Double 
  | String String
  | ParenL | ParenR
  | BracketL | BracketR 
  | BraceL | BraceR
  | LArrow
  | Comma | TTrue | TFalse | ConsLead
  | Forall
  | Exist
  | For
  | Anno
  | At
  deriving (Eq, Show)

-- main = do
--   s <- getContents
--   print (alexScanTokens s)
}