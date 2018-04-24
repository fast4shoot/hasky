{-# LANGUAGE OverloadedStrings, LambdaCase, TypeApplications #-}
module Builtins(builtins) where

import Control.Arrow ((&&&))
import qualified Data.Map.Strict as Map
import Data.Semigroup ((<>))
import qualified Data.Text as T

import AST

class FromVal a where fromVal :: Val b -> Either T.Text a
class ToVal a where toVal :: T.Text -> a -> Val b

instance FromVal Integer where
    fromVal (VInt x) = Right x
    fromVal x = Left "can't convert Val to Integer"

instance ToVal Integer where toVal _ x = VInt x
instance ToVal Bool where toVal _ x = builtinBool x

instance (FromVal a, ToVal b) => ToVal (a -> b) where
    toVal name f = VBuiltin name $ \x -> do
        x' <- x
        val <- fromVal x'
        pure $ toVal (name <> ".fn") $ f val

fn2 name f = VBuiltin name $ \x -> pure $ VBuiltin (name <> ".fn") $ \y -> f x y
builtinTrue = fn2 "true" $ \t _ -> t
builtinFalse = fn2 "false" $ \_ e -> e
builtinBool c = if c then builtinTrue else builtinFalse

builtins :: Map.Map [T.Text] (ParsedModule a)
builtins = Map.singleton ["Builtin"] (ParsedModule [] decls)
    where
        decls = fmap (vGetBuiltinName &&& PEVal) [
            toVal "+" $ (+) @Integer,
            toVal "-" $ (-) @Integer,
            toVal "*" $ (*) @Integer,
            toVal "/" $ div @Integer,
            toVal "%" $ mod @Integer,
            toVal "==" $ (==) @Integer,
            toVal "!=" $ (/=) @Integer,
            toVal "<" $ (<) @Integer,
            toVal ">" $ (>) @Integer,
            toVal "<=" $ (<=) @Integer,
            toVal ">=" $ (>=) @Integer,
            toVal "succ" $ succ @Integer,
            toVal "pred" $ pred @Integer
            ]
