module Prettyprint(epretty) where

import AST
import Builtins (builtinPrettyCtx)
import Data.Maybe (maybe)
import Data.Text.Prettyprint.Doc

genId (depth, names) n = maybe defname pretty name
  where
    idx = depth - n - 1
    defname = pretty "id" <> pretty (idx - length names)
    name = lookup idx names

extendCtx (depth, names) newNames = (depth', names')
  where
    names' = zipWith (,) [depth..] newNames ++ names
    depth' = depth + length newNames

succCtx (depth, names) = (succ depth, names)

applyParensF ctx f@(EFunc _) = parens $ epretty' ctx f
applyParensF ctx f = epretty' ctx f

applyParensX ctx x@(EApply _ _) = parens $ epretty' ctx x
applyParensX ctx x = epretty' ctx x

funcBody ctx (EFunc body) = space <> genId ctx' 0 <> funcBody ctx' body
  where ctx' = succCtx ctx
funcBody ctx body = colon <+> epretty' ctx body

declareDecl ctx (name, expr) = pretty name <+> equals <+> epretty' ctx expr

declareBody ctx body = pretty "in" <+> epretty' ctx body

epretty' _ (EInt x) = pretty x
epretty' ctx (EName n) = genId ctx n
epretty' ctx (EApply f x) = applyParensF ctx f <+> applyParensX ctx x
epretty' ctx func@(EFunc _) = pretty "fn" <> funcBody ctx func
epretty' ctx (EDeclare [decl] body) = braces $ declareDecl ctx' decl <+> declareBody ctx' body
  where
    ctx' = extendCtx ctx [fst decl]
epretty' ctx (EDeclare decls body) = braces $ (indent 4 $ hcat [line, docDecls, line, docBody]) <> line
  where
    ctx' = extendCtx ctx $ map fst decls
    docDecls = vsep $ map (declareDecl ctx') decls
    docBody = declareBody ctx' body

epretty = epretty' builtinPrettyCtx
