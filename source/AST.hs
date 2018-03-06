{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module AST where

import qualified Data.Text as T

data Val a =
    VAny a
    | VInt Integer
    | VBuiltin { vGetBuiltinName :: T.Text, vGetBuiltinBody :: Either T.Text (Val a) -> Either T.Text (Val a) }
    | VFunc T.Text [Either T.Text (Val a)] (Expr a)

instance Show a => Show (Val a) where
    show (VAny x) = "VAny (" ++ show x ++ ")"
    show (VInt x) = "VInt " ++ show x
    show (VBuiltin name _) = "VBuiltin " ++ T.unpack name
    show (VFunc name _ _) = "VFunc " ++ T.unpack name

data ParsedExpr a =
    PEVal (Val a)
    | PEName { peModule :: [T.Text], peIdentifier :: T.Text }
    | PEApply (ParsedExpr a) (ParsedExpr a)
    | PEFunc { peFuncParamName :: T.Text, peFuncBody :: (ParsedExpr a) }
    | PEDeclare { peDeclareDecls :: [(T.Text, ParsedExpr a)], peDeclareBody :: ParsedExpr a }
    deriving Show

data ParsedImportNames = 
    PINAll 
    | PINSpecific [T.Text]
    deriving Show

data ParsedImport =
    ParsedImport { piModule :: [T.Text], piAlias :: Maybe T.Text, piNames :: ParsedImportNames }
    deriving Show

data ParsedModule a =
    ParsedModule { pmImports :: [ParsedImport], pmDecls :: [(T.Text, ParsedExpr a)] }
    deriving Show

data Expr a = 
    EVal (Val a)
    | EName Int -- Maximálně 2 miliardy jmen ve scopu je trochu málo
    | EApply (Expr a) (Expr a)
    | EFunc { eFuncName :: T.Text, eFuncBody :: Expr a }
    | EDeclare [Expr a] (Expr a) -- Zjednodušit to na EDeclare String Expr Expr asi nejde, protože by pak nefungovalo { a = f b ; b = g a in ... }
    deriving Show
