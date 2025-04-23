{-# LANGUAGE LambdaCase #-}
module Resolver where

import AST as A
import Parser as P
import qualified Data.Map as M
import Control.Monad (join)
import Data.List (nub)

data Ctx = Ctx
  { functionNames :: [String]
  , boundedNames :: [String]
  } deriving Show

resolveTerm :: Ctx -> P.Term -> Either String A.Term
resolveTerm ctx@(Ctx funs vars) = \case
  P.If t1 t2 t3 -> do
    t1 <- resolveTerm ctx t1
    t2 <- resolveTerm ctx t2
    t3 <- resolveTerm ctx t3
    pure $ A.If t1 t2 t3
  P.Lit t -> case t of
    P.LitBool b -> pure $ A.Lit $ A.LitBool b
    P.LitFloat f -> pure $ A.Lit $ A.LitFloat f
    P.LitInt n -> pure $ A.Lit $ A.LitInt n
    P.LitStr s -> pure $ A.Lit $ A.LitStr s
    P.LitList ls -> do
      ls' <- mapM (resolveTerm ctx) ls
      pure $ A.Lit $ A.LitList ls'
  P.Lam x b -> do
    b' <- resolveTerm (ctx {boundedNames = x : boundedNames ctx}) b
    pure $ A.Lam x b'
  P.Id x
    | x `elem` boundedNames ctx -> pure $ A.Var x
    | x `elem` functionNames ctx -> pure $ A.FunCall x
    | otherwise -> case M.lookup x nameToPrim of
        Just prim -> pure $ PrimCall prim
        Nothing -> Left $ "unknown identifier: " ++ x
  P.App f x -> do
    f <- resolveTerm ctx f
    x <- resolveTerm ctx x
    pure $ A.App f x
  P.BiOp op l r -> do
    l <- resolveTerm ctx l
    r <- resolveTerm ctx r
    pure $ A.BiOp op l r
  P.Let x t b -> do
    t <- resolveTerm ctx t
    b <- resolveTerm (ctx {boundedNames = x : boundedNames ctx}) b
    pure $ A.Let x t b
  P.Match t cls -> do
    t <- resolveTerm ctx t
    cls <- mapM (resolveClause ctx) cls
    pure $ A.Match t cls
  P.Forall x t b -> do 
    t <- resolveTerm ctx t 
    b <- resolveTerm (ctx {boundedNames = x : boundedNames ctx}) b
    pure $ A.Forall x t b
  P.Exist x t b -> do 
    t <- resolveTerm ctx t 
    b <- resolveTerm (ctx {boundedNames = x : boundedNames ctx}) b
    pure $ A.Exist x t b
  P.For v t vs b -> do 
    t <- resolveTerm ctx t 
    b <- resolveTerm (newVars (v : vs) ctx) b
    pure $ A.For v t vs b
  P.ListComp b ls -> do 
    let (names, terms) = (fst <$> ls, snd <$> ls)
    b <- resolveTerm (newVars names ctx) b
    terms <- mapM (resolveTerm (newVars names ctx) {- THIS IS SIMPLIFIED -}) terms 
    pure $ A.ListComp b $ zip names terms
  
newVar x ctx = ctx {boundedNames = x : boundedNames ctx}

newVars vs env = foldr newVar env vs


resolveClause :: Ctx -> P.Clause -> Either String A.Clause
resolveClause ctx (P.Clause pat term) = do
  (fv, pat) <- resolvePattern pat
  term <- resolveTerm (ctx {boundedNames = fv ++ boundedNames ctx}) term
  pure $ A.Clause pat term

resolvePattern :: P.Pattern -> Either String ([String], A.Pattern)
resolvePattern = go where
  go = \case
    P.PVar x -> pure ([x], A.PVar x)
    P.PCon con ps -> case M.lookup con nameToCon of
        Just con' -> do
          ls <- mapM go ps
          let (fv', ps') = (ls >>= fst, snd <$> ls)
          pure (nub fv', A.PCon con' ps')
        Nothing -> Left $ "unknown constructor: " ++ con
    P.PList ps -> do 
        ls <- mapM go ps
        let (fv', ps') = (ls >>= fst, snd <$> ls)
        pure (fv', A.PList ps')
    P.PListCons l r -> do 
      (fvl, l') <- go l 
      (fvr, r') <- go r
      pure (fvl ++ fvr, A.PListCons l' r')  

resolveFuncDecl :: Ctx -> P.FuncDecl -> Either String A.FuncDecl
resolveFuncDecl ctx (P.FuncDecl name args rhs) = resolveTerm (ctx {boundedNames = args ++ boundedNames ctx}) rhs >>= pure . A.FuncDecl name args

readContext :: P.Program -> Ctx 
readContext prog = Ctx (go prog) [] where 
  go = \case 
    [] -> []
    (P.FuncDecl name _ _ : rest) -> name : go rest

