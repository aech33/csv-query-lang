module Interpreter (
    evalProgram,
    evalExpr,
    Env
) where

import ParserRe
import CSVHandler
import RelationalOps
import Data.List (intercalate)
import qualified Data.Map as Map

-- Environment to store variable bindings
type Env = Map.Map String Relation

-- Evaluate a program
evalProgram :: Program -> IO Relation
evalProgram prog = evalProgram' prog Map.empty

-- Helper function for evalProgram with environment
evalProgram' :: Program -> Env -> IO Relation
evalProgram' (SingleExpr expr) env = evalExpr env expr
evalProgram' (LetExpr var expr rest) env = do
    relValue <- evalExpr env expr
    let newEnv = Map.insert var relValue env
    evalProgram' rest newEnv

-- Evaluate a relational algebra expression
evalExpr :: Env -> RelAlgExpr -> IO Relation
evalExpr env expr = case expr of
    Table name -> loadTable name
    VarRef var -> case Map.lookup var env of
                    Just rel -> return rel
                    Nothing -> error $ "Variable not found: " ++ var
    
    Project items subExpr -> do
        rel <- evalExpr env subExpr
        return $ projectRelation items rel
    
    Select cond subExpr -> do
        rel <- evalExpr env subExpr
        return $ selectRelation cond rel
    
    CartesianProduct exprs -> do
        rels <- mapM (evalExpr env) exprs
        return $ cartesianProduct rels
    
    Join cond expr1 expr2 -> do
        rel1 <- evalExpr env expr1
        rel2 <- evalExpr env expr2
        let cart = cartesianProduct [rel1, rel2]
        return $ selectRelation cond cart
    
    Rename oldName newName subExpr -> do
        rel <- evalExpr env subExpr
        return $ renameColumn oldName newName rel
    
    Union expr1 expr2 -> do
        rel1 <- evalExpr env expr1
        rel2 <- evalExpr env expr2
        -- Simple union (assumes compatible schemas)
        return $ rel1 ++ rel2
    
    Difference expr1 expr2 -> do
        rel1 <- evalExpr env expr1
        rel2 <- evalExpr env expr2
        -- Simple difference (keeps rows from rel1 that don't appear in rel2)
        return $ filter (`notElem` rel2) rel1
    
    Intersect expr1 expr2 -> do
        rel1 <- evalExpr env expr1
        rel2 <- evalExpr env expr2
        -- Simple intersection (keeps rows that appear in both relations)
        return $ filter (`elem` rel2) rel1