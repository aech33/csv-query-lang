module Interpret where 
import Resolver
import qualified Parser as P
import AST 
import Eval 
import qualified Data.Map as M 
import Control.Monad (join)
import Data.List (intercalate)

prelude :: String 
prelude = intercalate "\n"
  [ "def putCSV csv := printCSV (sortCSV (removeEmpty csv))"
  , "def head ls := match ls with | x :: xs => x end"
  , "def tail ls := match ls with | x :: xs => xs end"
  , "def get ls i := if i == 0 then head ls else get (tail ls) (i - 1)"
  ]

mkEnv :: P.Program -> Either String Env 
mkEnv prog = do 
    decls <- mapM (resolveFuncDecl $ readContext prog) prog
    pure $ Env M.empty $ M.fromList (map (\decl -> (funcName decl, decl)) decls)

callMain :: Env -> IO Value 
callMain env@(Env _ funs) = case M.lookup "main" funs of 
  Just (FuncDecl _ [] rhs) -> eval env rhs  
  _ -> error "main is not well defined."

run :: FilePath -> IO ()
run fp = do 
  source <- readFile fp 
  let env = P.parse (prelude ++ source) >>= mkEnv
  case env of 
    Left err -> error err 
    Right env -> do 
      v <- callMain env
      pure ()

