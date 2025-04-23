{
module Parser where
import qualified Token as T 
}
%name parseTok

%tokentype { T.Token }
%error { parseError }
%monad { Either String }

%token
  def       {T.Def}
  ':='      {T.DefEq}
  if        {T.If}
  then      {T.Then}
  else      {T.Else}
  fun       {T.Fun}
  exist     {T.Exist}
  forall    {T.Forall}
  for       {T.For}
  '=>'      {T.FunTo}
  let       {T.Let}
  in        {T.In}
  match     {T.Match}
  with      {T.With}
  '|'       {T.Bar}
  '#'       {T.ConsLead}
  end       {T.End}
  int       {T.Int $$}
  float     {T.Float $$}
  string    {T.String $$}
  true      {T.TTrue}
  false     {T.TFalse}
  '('       {T.ParenL}
  ')'       {T.ParenR}
  '['       {T.BracketL}
  ']'       {T.BracketR}
  ','       {T.Comma}
  '||'      {T.Op "||" }
  '&&'      {T.Op "&&" }
  '+'       {T.Op "+"  }
  '-'       {T.Op "-"  }
  '*'       {T.Op "*"  }
  '/'       {T.Op "/"  }
  '%'       {T.Op "%"  }
  '++'      {T.Op "++" }
  '<'       {T.Op "<"  }
  '>'       {T.Op ">"  }
  '=='      {T.Op "==" }
  '::'      {T.Op "::" }
  ';'       {T.Op ";"}
  '@'       {T.At}
  ':'       {T.Anno}
  '{'       {T.BraceL}
  '}'       {T.BraceR}
  '<-'      {T.LArrow}
  id        {T.Id $$}
  
-- ::
-- * / % 
-- + - ++  
-- < > == 
-- &&
-- ||
%right else '=>' in
%right '<-'
%right ';'
%right ',' 
%left '||'
%left '&&'
%nonassoc '<' '>' left '=='
%left '+' '-' '++'
%left '*' '/' '%'
%right '::'
%nonassoc ':' '@'
%nonassoc let fun forall exist for if match '(' id string true false int float '[' '#' '{'
%nonassoc APP

%%
Program :: {Program}
Program 
  : {- empty -}                 {% pure [] }
  | FuncDecl Program            {% pure $ $1 : $2 }

FuncDecl :: {FuncDecl}
FuncDecl
  : def id ArgList ':=' Term    {% pure $ FuncDecl $2 $3 $5 }

ArgList :: {[Var]}
ArgList 
  : {- empty -}                 {% pure [] }
  | id ArgList                  {% pure $ $1 : $2 }

Term :: {Term}
Term
  : let id ':=' Term in Term    {% pure $ Let $2 $4 $6}
  | if Term then Term else Term {% pure $ If $2 $4 $6}
  | fun id '=>' Term            {% pure $ Lam $2 $4}

  | forall id ':' Term  '=>' Term {% pure $ Forall $2 $4 $6}
  | exist id ':' Term  '=>' Term  {% pure $ Exist $2 $4 $6}
  | for id ':' Term '@' id ArgList '=>' Term {% pure $ For $2 $4 ($6 : $7) $9}

  | match Term with Clauses end {% pure $ Match $2 $4}
  | Term Term %prec APP         {% pure $ App $1 $2}
  | Term '||' Term              {% pure $ makeOp $1 $3 OpOr     }
  | Term '&&' Term              {% pure $ makeOp $1 $3 OpAnd    } 
  | Term '+'  Term              {% pure $ makeOp $1 $3 OpPlus   } 
  | Term '-'  Term              {% pure $ makeOp $1 $3 OpMin    } 
  | Term '*'  Term              {% pure $ makeOp $1 $3 OpMul    } 
  | Term '/'  Term              {% pure $ makeOp $1 $3 OpDiv    } 
  | Term '%'  Term              {% pure $ makeOp $1 $3 OpMod    } 
  | Term '++' Term              {% pure $ makeOp $1 $3 OpAppend }
  | Term '<'  Term              {% pure $ makeOp $1 $3 OpLT     } 
  | Term '>'  Term              {% pure $ makeOp $1 $3 OpGT     } 
  | Term '==' Term              {% pure $ makeOp $1 $3 OpEQ     }
  | Term '::' Term              {% pure $ makeOp $1 $3 OpCons   }
  | Term ';'  Term              {% pure $ makeOp $1 $3 OpSeq    }
  | '(' Term ')'                {% pure $ $2}
  | '{' Term '|' Binders '}'    {% pure $ ListComp $2 $4}
  | Lit                         {% pure $ Lit $1}
  | id                          {% pure $ Id $1}

Binders 
  : id '<-' Term                {% pure $ [($1, $3)]}
  | id '<-' Term ',' Binders    {% pure $ ($1, $3) : $5}

Lit :: {Lit}
Lit 
  : string                      {% pure $ LitStr $1}
  | float                       {% pure $ LitFloat $1}
  | int                         {% pure $ LitInt $1}
  | true                        {% pure $ LitBool True}
  | false                       {% pure $ LitBool False}
  | '[' ListLit ']'             {% pure $ LitList $2}

ListLit :: {[Term]}
ListLit 
  : {- empty -}                 {% pure $ []}
  | Term                        {% pure $ [$1]}
  | Term ',' ListLit            {% pure $ $1 : $3}                        

Clauses :: {[Clause]}
Clauses 
  : {- empty -}                 {% pure $ []}
  | '|' Pattern '=>' Term 
    Clauses                     {% pure $ Clause $2 $4 : $5}

Pattern :: {Pattern}
Pattern 
  : Pattern1 '::' Pattern       {% pure $ PListCons $1 $3}
  | Pattern1                    {% pure $1}  

Pattern1
  : '#' id Pattern2s            {% pure $ PCon $2 $3}
  | Pattern2                    {% pure $1}

Pattern2 
  : '(' Pattern ')'             {% pure $ $2}
  | '[' PatternsWithComma ']'   {% pure $ PList $2}
  | id                          {% pure $ PVar $1}

Pattern2s :: {[Pattern]}
Pattern2s 
  : {- empty -}                 {% pure $ []}
  | Pattern2 Pattern2s          {% pure $ $1 : $2}

PatternsWithComma :: {[Pattern]}
PatternsWithComma 
  : {- empty -}                   {% pure $ []}
  | Pattern                       {% pure $ [$1]}
  | Pattern ',' PatternsWithComma {% pure $ $1 : $3}

{

makeOp :: Term -> Term -> Op -> Term
makeOp l r op = BiOp op l r   

parse :: String -> Either String Program
parse = parseTok . T.alexScanTokens

parseError tok = Left $ "Parse error in: " ++ show tok ++ "\nExpecting: " -- ++ show exp

data Op = OpPlus | OpMin | OpMul | OpDiv | OpLT | OpGT | OpEQ
        | OpOr   | OpAnd | OpMod | OpCons | OpAppend | OpSeq
  deriving (Show, Eq)

type Var = String
type Constructor = String

type Program = [FuncDecl]

data FuncDecl = FuncDecl
  { funcName :: String
  , arguments :: [Var]
  , funcBody :: Term
  } deriving Show

type ArgList = [String]

data Pattern 
  = PVar Var 
  | PCon Constructor [Pattern]
  | PList [Pattern]
  | PListCons Pattern Pattern 
  deriving (Show, Eq)

data Clause = Clause Pattern Term 
  deriving (Show, Eq)
  
data Lit 
  = LitStr String 
  | LitInt Int 
  | LitFloat Double 
  | LitBool Bool
  | LitList [Term]
  deriving (Show, Eq)

data Term 
  = If Term Term Term 
  | Lit Lit
  | Lam Var Term 
  | Id Var   
  | App Term Term 
  | BiOp Op Term Term
  | Let Var Term Term 
  | Match Term [Clause]

  | Forall Var Term Term 
  | Exist Var Term Term 
  | For Var Term [Var] Term

  | ListComp Term [(Var, Term)]

  deriving (Show, Eq)
}