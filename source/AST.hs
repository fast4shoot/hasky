{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module AST (Val(..), ParsedExpr(..), Expr(..)) where

import qualified Data.Text as T

data Val a =
    VAny a
    | VInt Integer
    | VBuiltin { vGetBuiltinName :: T.Text, vGetBuiltinBody :: Either T.Text (Val a) -> Either T.Text (Val a) }
    | VFunc T.Text [Either T.Text (Val a)] Expr

instance Show (Val a) where
    show (VAny _) = "VAny"
    show (VInt x) = "VInt " ++ show x
    show (VBuiltin name _) = "VBuiltin " ++ T.unpack name
    show (VFunc name _ _) = "VFunc " ++ T.unpack name

data ParsedExpr =
    PEInt Integer
    | PEName T.Text
    | PEApply ParsedExpr ParsedExpr
    | PEFunc { peFuncParamName :: T.Text, peFuncBody :: ParsedExpr }
    | PEDeclare { peDeclareDecls :: [(T.Text, ParsedExpr)], peDeclareBody :: ParsedExpr }
    deriving Show

data Expr = 
    EInt Integer
    | EName Int -- Maximálně 2 miliardy jmen ve scopu je trochu málo
    | EApply Expr Expr
    | EFunc { eFuncName :: T.Text, eFuncBody :: Expr }
    | EDeclare [Expr] Expr -- Zjednodušit to na EDeclare String Expr Expr asi nejde, protože by pak nefungovalo { a = f b ; b = g a in ... }
    deriving Show
