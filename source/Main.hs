module Main where

import Data.Attoparsec.Text (parseOnly)
import Data.Bifunctor
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import AST (vGetBuiltinName)
import Parse (pProgram)
import Simplify (simplify)
import Eval (eval, intro)
import Builtins (builtins)

main = do
    prog <- TIO.getContents
    let parsed = first T.pack $ parseOnly pProgram prog
        simplified = simplify (fmap vGetBuiltinName builtins) <$> parsed
        evaluated = simplified >>= eval builtins
    print evaluated
    case evaluated of
        Right val -> intro val
        _ -> return ()
