module Main where

import Data.Attoparsec.Text (parse, IResult(..))
import Data.List (intercalate)
import Data.Map.Strict (keys)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (stdin)

import AST (vGetBuiltinName)
import Load (loadProgram)
import Simplify (simplify)
import Eval (eval, intro)
import Builtins (builtins)

{-
main = do
    prog <- TIO.getContents
    let parsed = xformParse $ parse pProgram prog
        simplified = simplify (fmap vGetBuiltinName builtins) <$> parsed
        evaluated = simplified >>= eval builtins
    print evaluated
    case evaluated of
        Right val -> intro val
        _ -> return ()
-}

main = do
    prog <- loadProgram builtins stdin
    case prog of
        Left e -> print e
        Right modules -> print $ simplify modules
