{-# LANGUAGE OverloadedStrings #-}
module Simplify where

import qualified Data.Text as T
import Data.List (elemIndex)

import AST

simplify :: [T.Text] -> ParsedExpr -> Expr
simplify ctx e = simplify' ctx [] e

simplify' _ _ (PEInt x) = EInt x
simplify' ctx _ (PEName name) = case elemIndex name ctx of Just idx -> EName idx ; Nothing -> error $ "Unknown identifier: " ++ T.unpack name
simplify' ctx name (PEApply lhs rhs) = EApply (simplify' ctx name lhs) (simplify' ctx name rhs)
simplify' ctx name (PEFunc param body) = EFunc (T.intercalate "." name) $ simplify' (param:ctx) (name ++ ["fn"]) body
simplify' ctx name (PEDeclare decls body) = EDeclare decls' body'
  where
    ctx' = reverse (fmap fst decls) ++ ctx
    decls' = simplifyDecl <$> decls
    simplifyDecl (name', body) = simplify' ctx' (name ++ [name']) body 
    body' = simplify' ctx' name body
