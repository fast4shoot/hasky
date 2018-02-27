{-# LANGUAGE OverloadedStrings #-}
module Parse (pModule) where

import Control.Applicative
import Control.Monad (guard, when)
import Data.Attoparsec.Text as P
import Data.Char (isAlpha, isAlphaNum, isPunctuation, isSymbol, isDigit, isSpace, isUpper)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import AST (Val(..), ParsedExpr(..), ParsedImport(..), ParsedModule(..), ParsedImportNames(..))

data WordType =
    WIdentifier { wiIdentifier :: !T.Text }
    | WKeyword { wkKeyword :: !Keyword }
    deriving (Show, Eq)

data Keyword = 
    KWColon
    | KWEquals
    | KWSeparator
    | KWFn
    | KWIn
    | KWImport
    deriving (Show, Eq)

classifyWord w = case w of
    ";" -> WKeyword KWSeparator
    ":" -> WKeyword KWColon
    "=" -> WKeyword KWEquals
    "fn" -> WKeyword KWFn
    "in" -> WKeyword KWIn
    "import" -> WKeyword KWImport
    w -> WIdentifier w

guarded pred parser = do
    result <- parser
    guard $ pred result
    pure result

skipHorizontalSpace = skipWhile isHorizontalSpace <?> "skipHorizontalSpace"
skipWhite = skipHorizontalSpace *> optional (char '\\' *> skipSpace *> skipEmptySpace) *> pure ()
skipEmptySpace = many tComment *> pure ()

tModulePart = guarded (not . T.null) scanner <* skipWhite
    where scanner = scan isUpper $ \p c -> if p c then Just isAlphaNum else Nothing
tModulePartSeparator = char '.' *> skipWhite
tModulePrefix = sepBy1' tModulePart tModulePartSeparator

tComment = do
    optional (char '`' *> skipWhile (not . isEndOfLine)) *> endOfLine *> skipHorizontalSpace

tAlphanumWord = do
    w <- scan isAlpha $ \p c -> if p c then Just isAlphaNum else Nothing
    guard $ not $ T.null w
    pure $ classifyWord w

tSymbolWord = do
    w <- takeWhile1 $ \c -> (isSymbol c || isPunctuation c) && notInClass "(){}`" c
    pure $ classifyWord w

tParenthesisOpen = char '(' <* skipWhite
tParenthesisClose = char ')' <* skipWhite
tBraceOpen = char '{' <* skipWhite
tBraceClose = char '}' <* skipWhite

tWord = (tAlphanumWord <|> tSymbolWord) <* skipWhite <?> "word"

tIdentifier = wiIdentifier <$> guarded (isId) tWord <?> "identifier"
    where isId (WIdentifier w) = True
          isId _ = False

tPrefixedIdentifier = (do
    prefix <- fromMaybe [] <$> optional (tModulePrefix <* tModulePartSeparator)
    id <- tIdentifier
    pure (prefix, id)) <?> "prefixed identifier"

tKeyword kw = wkKeyword <$> guarded (isKW) tWord <?> "keyword"
    where isKW (WKeyword kw') = kw == kw'
          isKW _ = False

tInt = decimal <* skipWhite :: Parser Integer

tSeparator = do
    tKeyword KWSeparator *> pure () <|> tComment
    skipEmptySpace

xformCalls (e:es) = xformCalls' e es
xformCalls' lhs (e:es) = xformCalls' (PEApply lhs e) es
xformCalls' lhs [] = lhs

xformFn [p] e = PEFunc (xformParam p) e
xformFn (p:ps) e = PEFunc (xformParam p) (xformFn ps e)

xformParam "_" = ""
xformParam p = p

pExpr = xformCalls <$> many1' pSubExpr <?> "expression"
pSubExpr =
    PEVal . VInt <$> tInt
    <|> uncurry PEName <$> tPrefixedIdentifier
    <|> pFunction
    <|> (tParenthesisOpen *> pExpr <* tParenthesisClose)
    <|> pDeclare
pFunction = (do
    tKeyword KWFn
    params <- many1' tIdentifier
    tKeyword KWColon
    body <- pExpr
    pure $ xformFn params body) <?> "function"
pDeclare = (do
    tBraceOpen
    skipEmptySpace
    decls <- pDeclareBody
    tKeyword KWIn
    body <- pExpr
    skipEmptySpace
    tBraceClose
    pure $ PEDeclare decls body) <?> "declarations"
pDeclareBody = do
    decls <- pDecl `sepBy1'` tSeparator
    -- TODO: failnout v případě, že blok deklaruje jedno jméno vícekrát
    tSeparator <|> pure ()
    pure decls
  where pDecl = do
        name <- tIdentifier
        tKeyword KWEquals
        body <- pExpr
        pure $ (name, body)
pImport = do
    tKeyword KWImport
    moduleName <- tModulePrefix
    alias <- optional tModulePart
    names <- optional $ tParenthesisOpen *> many' tIdentifier <* tParenthesisClose
    pure $ case (alias, names) of
        (_, Just names) -> ParsedImport moduleName alias $ PINSpecific names
        (Just _, Nothing) -> ParsedImport moduleName alias $ PINSpecific []
        (Nothing, Nothing) -> ParsedImport moduleName alias PINAll
pImports = pImport `sepBy'` tSeparator <?> "imports"
pModule = (do
    skipEmptySpace
    imports <- pImports
    when (not $ null imports) (tSeparator <|> endOfInput)
    body <- pDeclareBody
    endOfInput
    pure $ ParsedModule imports body) <?> "module"
