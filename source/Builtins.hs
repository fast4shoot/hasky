{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Builtins(builtins) where

import Control.Arrow ((&&&))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import AST

builtinTrue = fn2 "true" $ \t _ -> t
builtinFalse = fn2 "false" $ \_ e -> e
builtinBool c = if c then builtinTrue else builtinFalse

intErrMsg name = Left $ T.concat ["function ", name, " is only callable on numbers"]

fn1ii name f = VBuiltin name $ \case
    Right (VInt a) -> pure $ VInt $ f a
    Right _ -> intErrMsg name
    x -> x

fn2 name f = VBuiltin name $ \a -> pure $ VBuiltin (T.append name ".fn") $ \b -> f a b
fn2ii name f = VBuiltin name $ \case
    Right (VInt a) -> pure $ VBuiltin (T.append name ".fn") $ \case
        Right (VInt b) -> pure $ f a b
        Right _ -> intErrMsg name
        x -> x
    Right _ -> intErrMsg name 
    x -> x
fn2iii name f = fn2ii name $ \a b -> VInt $ f a b
fn2iib name f = fn2ii name $ \a b -> builtinBool $ f a b

builtins :: Map.Map [T.Text] (ParsedModule a)
builtins = Map.singleton ["Builtin"] (ParsedModule [] decls)
    where
        decls = fmap (vGetBuiltinName &&& PEVal) [
            builtinTrue,
            builtinFalse,
            fn2iii "+" (+),
            fn2iii "-" (-),
            fn2iii "*" (*),
            fn2iii "/" div,
            fn2iii "%" mod,
            fn2iib "==" (==),
            fn2iib "!=" (/=),
            fn2iib "<" (<),
            fn2iib ">" (>),
            fn2iib "<=" (<=),
            fn2iib ">=" (>=),
            fn1ii "succ" succ,
            fn1ii "pred" pred
            ]
