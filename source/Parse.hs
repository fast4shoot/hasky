{-# LANGUAGE OverloadedStrings #-}
module Parse (pProgram) where

import Control.Applicative
import Control.Monad (guard)
import Data.Attoparsec.Text as P
import Data.Char (isAlpha, isAlphaNum, isPunctuation, isSymbol, isDigit, isSpace)
import qualified Data.Text as T

import AST (ParsedExpr(..))

data WordType =
    WIdentifier { getIdentifier :: !T.Text }
    | WKeyword { getKeyword :: !Keyword }
    deriving (Show, Eq)

data Keyword = 
    KWColon
    | KWEquals
    | KWSeparator
    | KWFn
    | KWIn
    deriving (Show, Eq)

classifyWord w = case w of
    ";" -> WKeyword KWSeparator
    ":" -> WKeyword KWColon
    "=" -> WKeyword KWEquals
    "fn" -> WKeyword KWFn
    "in" -> WKeyword KWIn
    w -> WIdentifier w

guarded pred parser = do
    result <- parser
    guard $ pred result
    pure result

skipWhite = skipWhile isHorizontalSpace
skipEmptySpace = many (tComment *> skipWhite) *> pure ()

tComment = optional (char '`' *> skipWhile (not . isEndOfLine)) *> endOfLine *> skipWhite

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

tIdentifier = getIdentifier <$> guarded (isId) tWord <?> "identifier"
    where isId (WIdentifier w) = True
          isId _ = False

tKeyword kw = getKeyword <$> guarded (isKW) tWord <?> "keyword"
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
    PEInt <$> tInt
    <|> PEName <$> tIdentifier
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
pProgram = (do
    skipEmptySpace
    body <- pDeclareBody
    endOfInput
    pure $ PEDeclare body $ PEName "main") <?> "program"
