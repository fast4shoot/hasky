{-# LANGUAGE OverloadedStrings #-}
module Load (loadProgram, LoadError) where

import Data.Attoparsec.Text (parseOnly, IResult(..))
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (Handle)

import AST (ParsedImport(..), ParsedModule(..))
import Builtins (builtins)
import Parse (pModule)

data LoadError =
    LoadError { leModuleName :: [T.Text], leError :: T.Text }
    deriving Show

xformParse moduleName (Left e) = Left $ LoadError moduleName $ T.pack e
xformParse moduleName (Right module_) = Right module_

loadProgram :: Map.Map [T.Text] (ParsedModule a) -> Handle -> IO (Either LoadError (Map.Map [T.Text] (ParsedModule a)))
loadProgram builtins h = do
    let moduleName = ["<main>"]
    content <- TIO.hGetContents h
    let parsed = xformParse moduleName $ parseOnly pModule content
    case parsed of
        Left e -> return $ Left e
        Right module_ -> loadModules (Map.insert moduleName module_ builtins) $ pmImports module_

loadModules loaded [] = pure $ Right loaded
loadModules loaded (import_:imports)
    | Map.member (piModule import_) loaded = loadModules loaded imports
    | otherwise = do
        let moduleName = piModule import_
            fileName = T.intercalate "/" moduleName <> ".hy"
        putStr "Loading "
        print fileName
        content <- TIO.readFile $ T.unpack fileName
        let parsed = xformParse moduleName $ parseOnly pModule content
        case parsed of
            Left e -> return $ Left e
            Right module_ ->
                let loaded' = Map.insert moduleName module_ loaded
                in loadModules loaded' $ imports <> pmImports module_

