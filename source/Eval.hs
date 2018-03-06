{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Eval (eval, intro, showIntro) where

import Data.Foldable (forM_)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Random (randomIO)

import AST
import Builtins (builtins)

type VAnyHandler a = a -> Either T.Text (Val a) -> Either T.Text (Val a)

eval :: VAnyHandler a -> Expr a -> Either T.Text (Val a)
eval hAny = eval' hAny []

eval' :: VAnyHandler a -> [Either T.Text (Val a)] -> Expr a -> Either T.Text (Val a)
eval' _ _ (EVal x) = pure $ x
eval' _ s (EName x) = s !! x
eval' hAny s (EApply f x) = do
    f' <- eval' hAny s f
    let x' = eval' hAny s x
    case f' of
        VBuiltin _ f -> f x'
        VFunc _ s' f -> eval' hAny (x' : s') f
        VAny any -> hAny any x'
        VInt x -> Left $ "only functions are callable, not VInt " <> T.pack (show x)
eval' _ s (EFunc n body) = pure $ VFunc n s body
eval' hAny s (EDeclare decls body) = eval' hAny s' body
    where 
        s' = map (eval' hAny s') decls ++ s

data Intro = Intro Int [Val Intro] deriving Show

intro :: Val Intro -> Val Intro
intro = intro' 0

intro' :: Int -> Val Intro -> Val Intro
intro' _ (VAny (Intro tag vals)) = VAny $ Intro tag (intro <$> reverse vals)
intro' tag fn@(VFunc name s body) =
    case introEval (pure (VAny (Intro tag [])) : s) body of
        Left err -> fn
        Right val -> intro' (succ tag) val
intro' _ x = x

introEval = eval' introHandler
introHandler (Intro tag vals) arg = do
    arg' <- arg
    pure $ VAny $ Intro tag (arg' : vals)

showIntro (VInt x) = T.pack $ show x
showIntro (VBuiltin name _) = name
showIntro (VFunc name _ _) = name
showIntro (VAny (Intro tag vals)) = T.concat ["(", T.intercalate " " $ (T.pack $ show tag) : fmap showIntro vals, ")"]
