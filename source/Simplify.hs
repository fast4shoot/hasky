{-# LANGUAGE OverloadedStrings #-}
module Simplify where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.List (elemIndex)
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
        ctx' = reverse (fmap fst decls) ++ ctx
        decls' = simplifyDecl <$> decls
        simplifyDecl (name', body) = go ctx' (name ++ [name']) body 
        body' = go ctx' name body
        in EDeclare <$> sequence decls' <*> body'

    in sequence <$> fmap (go [] [] . snd) $ pmDecls module_

lookupName modules moduleName imports ctx name = let
    id = peIdentifier name
    module_ = peModule name

    idxInModule (offset, module_) = (+ (offset + length ctx)) <$> elemIndex id (fst <$> pmDecls module_)

    importMatches [] (ParsedImportAll importedModule) = Just importedModule
    importMatches [module_] (ParsedImportAliased importedModule alias) = if module_ == alias || [module_] == importedModule then Just importedModule else Nothing
    importMatches module_ import_ = if module_ == piModule import_ then Just module_ else Nothing

    self :: [[T.Text]] -> [[T.Text]]
    self = case module_ of
        [] -> (moduleName :) :: [[T.Text]] -> [[T.Text]]
        x -> if x == moduleName then (moduleName :) else (\x -> x)
    candidateModules = self $ mapMaybe (importMatches module_) imports
    candidateIds = mapMaybe (idxInModule . (modules Map.!)) candidateModules

    in case candidateIds of
        [idx] -> Right idx
        [] -> Left $ T.concat ["Identifier ", T.pack $ show name, " not found, modules searched: ", T.pack $ show candidateModules]
        _ -> Left $ T.concat ["Identifier ", T.pack $ show name, " found multiple times, modules searched: ", T.pack $ show candidateModules]
