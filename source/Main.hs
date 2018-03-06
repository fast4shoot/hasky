module Main where

import Data.Attoparsec.Text (parse, IResult(..))
import Data.List (intercalate)
import Data.Map.Strict (keys)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (stdin, stderr)

import Load (loadProgram)
import Simplify (simplify)
import Eval (eval, intro, showIntro)
import Builtins (builtins)

main = do
    prog <- loadProgram builtins stdin
    case prog of
        Left e -> print e
        Right modules -> do
            let simplified = simplify modules
                evaluated = simplified >>= eval (\_ _ -> error "Unexpected VAny")
            case evaluated of
                Right val -> TIO.putStrLn $ showIntro $ intro val
                Left e -> TIO.hPutStrLn stderr e
            
