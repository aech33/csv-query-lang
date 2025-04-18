module Interpreter (
    evalProgram,
    evalExpr,
    Env,
    InterpreterError(..)
) where

import ParserRe
import CSVHandler
import RelationalOps
import Data.List (intercalate)
import qualified Data.Map as Map
import Control.Exception (Exception, try, SomeException, throwIO, catch)
import System.IO.Error (isDoesNotExistError)

-- Custom exception type for interpreter errors
data InterpreterError = 
    VariableNotFound String
  | TableNotFound String
  | InterpreterOperationError String  -- Renamed from OperationError
  deriving (Show)

instance Exception InterpreterError

-- Environment to store variable bindings
type Env = Map.Map String Relation

-- Evaluate a program
evalProgram :: Program -> IO Relation
evalProgram prog = evalProgram' prog Map.empty

-- Helper function for evalProgram with environment
evalProgram' :: Program -> Env -> IO Relation
evalProgram' (SingleExpr expr) env = evalExpr env expr
evalProgram' (LetExpr var expr rest) env = do
    relValueResult <- try (evalExpr env expr) :: IO (Either SomeException Relation)
    case relValueResult of
        Left err -> throwIO $ InterpreterOperationError $ "Error in let binding for " ++ var ++ ": " ++ show err
        Right relValue -> do
            let newEnv = Map.insert var relValue env
            evalProgram' rest newEnv

-- Evaluate a relational algebra expression
evalExpr :: Env -> RelAlgExpr -> IO Relation
evalExpr env expr = case expr of
    Table name -> do
        result <- try (loadTable name) :: IO (Either SomeException Relation)
        case result of
            Left err -> throwIO $ TableNotFound $ "Table not found: " ++ name ++ " (" ++ show err ++ ")"
            Right rel -> return rel
    
    VarRef var -> case Map.lookup var env of
                    Just rel -> return rel
                    Nothing -> throwIO $ VariableNotFound $ "Variable not found: " ++ var
    
    Project items subExpr -> do
        rel <- evalExpr env subExpr
        result <- try (return $ projectRelation items rel) :: IO (Either SomeException Relation)
        case result of
            Left err -> throwIO $ InterpreterOperationError $ "Projection error: " ++ show err
            Right projectedRel -> return projectedRel
        
    Select cond subExpr -> do
        rel <- evalExpr env subExpr
        result <- try (return $ selectRelation cond rel) :: IO (Either SomeException Relation)
        case result of
            Left err -> throwIO $ InterpreterOperationError $ "Selection error: " ++ show err
            Right selectedRel -> return selectedRel
    
    CartesianProduct exprs -> do
        if length exprs < 2 then
            throwIO $ InterpreterOperationError "Cartesian product requires at least two relations"
        else do
            rels <- mapM (evalExpr env) exprs
            return $ cartesianProduct rels

    Join cond expr1 expr2 -> do
        rel1 <- evalExpr env expr1
        rel2 <- evalExpr env expr2
        let cart = cartesianProduct [rel1, rel2]
        result <- try (return $ selectRelation cond cart) :: IO (Either SomeException Relation)
        case result of
            Left err -> throwIO $ InterpreterOperationError $ "Join error: " ++ show err
            Right joinedRel -> return joinedRel
    
    Rename oldName newName subExpr -> do
        rel <- evalExpr env subExpr
        return $ renameColumn oldName newName rel
    
    Union expr1 expr2 -> do
        rel1 <- evalExpr env expr1
        rel2 <- evalExpr env expr2
        -- Verify the schemas are compatible (same number of columns)
        if not (null rel1) && not (null rel2) && length (head rel1) /= length (head rel2) then
            throwIO $ InterpreterOperationError $ "Union error: Relations have incompatible schemas"
        else
            return $ rel1 ++ rel2
    
    Difference expr1 expr2 -> do
        rel1 <- evalExpr env expr1
        rel2 <- evalExpr env expr2
        -- Verify the schemas are compatible
        if not (null rel1) && not (null rel2) && length (head rel1) /= length (head rel2) then
            throwIO $ InterpreterOperationError $ "Difference error: Relations have incompatible schemas"
        else
            return $ filter (`notElem` rel2) rel1
    
    Intersect expr1 expr2 -> do
        rel1 <- evalExpr env expr1
        rel2 <- evalExpr env expr2
        -- Verify the schemas are compatible
        if not (null rel1) && not (null rel2) && length (head rel1) /= length (head rel2) then
            throwIO $ InterpreterOperationError $ "Intersection error: Relations have incompatible schemas"
        else
            return $ filter (`elem` rel2) rel1