{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Eval (eval, intro) where

import Data.Foldable (forM_)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Random (randomIO)

import AST
import Builtins (builtins)

eval :: Expr a -> Either T.Text (Val a)
eval = eval' []

eval' :: [Either T.Text (Val a)] -> Expr a -> Either T.Text (Val a)
eval' _ (EVal x) = pure $ x
eval' s (EName x) = s !! x
eval' s (EApply f x) = do
    f' <- eval' s f
    case f' of
        VBuiltin _ f -> f $ eval' s x
        VFunc _ s' f -> eval' (eval' s x : s') f
        _ -> Left $ "only functions are callable, not " <> T.pack (show f')
eval' s (EFunc n body) = pure $ VFunc n s body
eval' s (EDeclare decls body) = eval' s' body
    where 
        s' = map (eval' s') decls ++ s

data Intro = Intro Char [Val Intro] deriving Show

intro :: Val Intro -> IO ()
intro v = intro' "" 'a' v

intro' s _ (VAny (Intro tag vals)) = do
    TIO.putStrLn $ T.concat [s, "tag ", T.pack $ show tag]
    forM_ (reverse vals) $ intro' ("  " <> s) 'a'
intro' s tag (VFunc name s' body) = do
    TIO.putStrLn $ T.concat [s, "introspecting fn ", name, " with tag ", T.pack $ show tag]
    let e = introEval (pure (VAny (Intro tag [])) : s') body
    case e of
        Left reason -> TIO.putStrLn $ s <> "...failed: " <> reason
        Right val -> intro' s (succ tag) val
intro' s tag x = TIO.putStrLn $ s <> T.pack (show x)

introEval :: [Either T.Text (Val Intro)] -> Expr Intro -> Either T.Text (Val Intro)
introEval _ (EVal x) = pure x
introEval s (EName x) = s !! x
introEval s (EApply f x) = do
    f' <- introEval s f
    let arg = introEval s x
    case f' of
        VBuiltin _ f -> f arg
        VFunc _ s' f -> do
            arg <- introEval s x
            introEval (pure arg : s') f
        VAny (Intro tag vals) -> do
            arg' <- arg
            pure $ VAny $ Intro tag (arg' : vals)
introEval s (EFunc n body) = pure $ VFunc n s body
introEval s (EDeclare decls body) = introEval s' body
    where 
        s' = map (introEval s') decls ++ s
