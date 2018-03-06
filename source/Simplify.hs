{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Simplify where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.List (elemIndex, elemIndices)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))

import AST

simplify :: Map.Map [T.Text] (ParsedModule a) -> Either T.Text (Expr a)
simplify modules = do
    let (_, modulesWithOffset) = Map.mapAccum (\acc mdl -> (acc + (length $ pmDecls mdl), (acc, mdl))) 0 modules
    simplified <- sequence $ Map.elems $ Map.mapWithKey (simplify' modulesWithOffset) modules
    mainIdx <- lookupName modulesWithOffset ["<main>"] [] [] $ PEName ["<main>"] "main"
    return $ EDeclare (concat simplified) (EName mainIdx)

simplify' modules moduleName module_ = let
    imports = pmImports module_

    go _ _ (PEVal x) = Right $ EVal x
    go ctx _ name@(PEName{}) = EName <$> lookupName modules moduleName imports ctx name
    go ctx name (PEApply lhs rhs) = EApply <$> go ctx name lhs <*> go ctx name rhs
    go ctx name (PEFunc param body) = EFunc (T.intercalate "." (moduleName <> name)) <$> go (param:ctx) (name ++ ["fn"]) body
    go ctx name (PEDeclare decls body) = let
        ctx' = fmap fst decls ++ ctx
        decls' = simplifyDecl <$> decls
        simplifyDecl (name', body) = go ctx' (name ++ [name']) body 
        body' = go ctx' name body
        in EDeclare <$> sequence decls' <*> body'

    in sequence <$> fmap (go [] [] . snd) $ pmDecls module_

lookupName :: Map.Map [T.Text] (Int, ParsedModule a) -> [T.Text] -> [ParsedImport] -> [T.Text] -> ParsedExpr a -> Either T.Text Int
lookupName modules moduleName imports ctx (PEName module_ ident) = let
    idxInModule (offset, module_) = (+ (offset + length ctx)) <$> elemIndex ident (fst <$> pmDecls module_)

    -- if module_ is empty. the import has to either import all names or a name equal to ident
    -- otherwise, module_ has to match the full module name or the alias
    emptyMatcher ParsedImport{ piNames = PINAll } = True
    emptyMatcher ParsedImport{ piNames = (PINSpecific names) } = ident `elem` names

    nonemptyMatcher ParsedImport{ piModule, piAlias } =
        piModule == module_ || (pure <$> piAlias) == Just module_

    moduleMatcher = case module_ of
        [] -> emptyMatcher
        _ -> nonemptyMatcher

    self :: [[T.Text]] -> [[T.Text]]
    self = case module_ of
        [] -> (moduleName :) :: [[T.Text]] -> [[T.Text]]
        x -> if x == moduleName then (moduleName :) else id
    inCtx = case module_ of
        [] -> (elemIndices ident ctx ++)
        _ -> id

    candidateModules = self $ piModule <$> (filter moduleMatcher imports)
    candidateIds = inCtx $ mapMaybe (idxInModule . (modules Map.!)) candidateModules

    in case candidateIds of
        [idx] -> Right idx
        [] -> Left $ T.concat ["Identifier ", T.pack $ show $ module_ ++ pure ident, " not found, modules searched: ", T.pack $ show candidateModules, ", ctx: ", T.pack $ show ctx]
        _ -> Left $ T.concat ["Identifier ", T.pack $ show $ module_ ++ pure ident, " found multiple times, modules searched: ", T.pack $ show candidateModules, " ctx: ", T.pack $ show ctx]
