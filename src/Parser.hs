{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Parser (parseProgram, ParseResult) where

import Ast
import Data.Char (isSpace)
import Data.Function ((&))
import Data.Functor.Identity (Identity)
import Data.String.Interpolate (i)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read (readMaybe)

data SExp
  = SList [SExp]
  | SAtom String

instance Show SExp where
  show (SList exprs) = "(" ++ (unwords . map show $ exprs) ++ ")"
  show (SAtom sym) = sym

type Parser =
  Parsec
    Void
    String

type ParseResult a = Either String a

skipSpace :: Parser ()
skipSpace =
  L.space
    space1
    (L.skipLineComment ";")
    (L.skipBlockCommentNested "#|" "|#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

lsymbol :: String -> ParsecT Void String Identity String
lsymbol = L.symbol skipSpace

list :: Parser [SExp]
list =
  label "list" $
    lexeme $
      between (lsymbol "(") (lsymbol ")") (many sexp)
        <|> between (lsymbol "[") (lsymbol "]") (many sexp)
        <|> between (lsymbol "{") (lsymbol "}") (many sexp)

symbol :: Parser String
symbol =
  let iSAtomChar c = (not . isSpace $ c) && notElem c "()[]{}\",'`;|\\"
   in label "symbol" $
        lexeme $
          some $
            satisfy iSAtomChar

sexp :: Parser SExp
sexp =
  label "s-expression" $
    choice
      [ SList <$> list,
        SAtom <$> symbol
      ]

sexpsToProgram :: [SExp] -> ParseResult Program
sexpsToProgram sexps =
  case reverse sexps of
    (bodySexp : defsRevSexps) -> do
      defs <- mapM sexpToDefinition $ reverse defsRevSexps
      body <- sexpToExp bodySexp
      pure $ Program defs body
    _ -> Left "Empty program"

sexpToDefinition :: SExp -> ParseResult Definition
sexpToDefinition (SList (SAtom "struct" : SAtom name : fieldsSexp)) = do
  fields <- mapM sexpToStructField fieldsSexp
  pure $ StructDef name fields
sexpToDefinition (SList (SAtom "union" : SAtom name : casesSexp)) = do
  cases <- mapM sexpToUnionConstructor casesSexp
  pure $ UnionDef name cases
sexpToDefinition (SList (SAtom "define" : SAtom name : SList paramsSexp : SAtom "->" : retTypeSexp : bodySexp)) = do
  params <- mapM sexpToFunParam paramsSexp
  retType <- sexpToTypeRef retTypeSexp
  body <- sexpsToExp bodySexp
  pure $ FunDef name params retType body
sexpToDefinition _ = Left "Bad definition syntax"

sexpToTypeRef :: SExp -> ParseResult TypeRef
sexpToTypeRef (SAtom "Int") = pure Int
sexpToTypeRef (SAtom "Float") = pure Float
sexpToTypeRef (SAtom "Bool") = pure Bool
sexpToTypeRef (SList []) = pure Unit
sexpToTypeRef (SList [SAtom "Ptr", tSexp]) = TypePtr <$> sexpToTypeRef tSexp
sexpToTypeRef (SAtom sym) = pure $ TypeRef sym
sexpToTypeRef _ = Left "Bad type syntax"

sexpToStructField :: SExp -> ParseResult StructField
sexpToStructField (SList [SAtom name, typeSexp]) =
  do
    type' <- sexpToTypeRef typeSexp
    pure $ StructField name type'
sexpToStructField _ = Left "Bad field syntax"

sexpToUnionConstructor :: SExp -> ParseResult UnionConstructor
sexpToUnionConstructor (SList [SAtom name, typeSexp]) =
  do
    type' <- sexpToTypeRef typeSexp
    pure $ UnionConstructor name type'
sexpToUnionConstructor _ = Left "Bad case syntax"

sexpToFunParam :: SExp -> ParseResult FunParam
sexpToFunParam (SList [SAtom name, typeSexp]) =
  do
    type' <- sexpToTypeRef typeSexp
    pure $ FunParam name type'
sexpToFunParam _ = Left "Bad param syntax"

sexpsToExp :: [SExp] -> ParseResult Exp
sexpsToExp (SList [SAtom "define", SAtom binding, valueSexp] : nextSexp : restSexp) = do
  value <- sexpToExp valueSexp
  rest <- sexpsToExp (nextSexp : restSexp)
  pure $ Let binding value rest
sexpsToExp (firstSexp : nextSexp : restSexp) = do
  first <- sexpToExp firstSexp
  rest <- sexpsToExp (nextSexp : restSexp)
  pure $ Eseq first rest
sexpsToExp [expr] = sexpToExp expr
sexpsToExp [] = Left "Empty expression sequence"

sexpToExp :: SExp -> ParseResult Exp
sexpToExp (SList (SAtom "begin" : bodySexp)) = sexpsToExp bodySexp
sexpToExp (SList (SAtom "let" : SList [SAtom binding, valueSexp] : bodySexp)) = do
  value <- sexpToExp valueSexp
  body <- sexpsToExp bodySexp
  pure $ Let binding value body
sexpToExp (SAtom str) =
  case readMaybe str of
    Just v -> pure $ IntLiteral v
    Nothing -> case readMaybe str of
      Just f -> pure $ FloatLiteral f
      Nothing -> case str of
        "#true" -> pure $ BoolLiteral True
        "#false" -> pure $ BoolLiteral False
        _ -> pure $ VarRef str
sexpToExp (SList [SAtom "set!", SAtom binding, valueSexp]) = do
  value <- sexpToExp valueSexp
  pure $ Assign binding value
sexpToExp (SList [SAtom "set-ptr!", ptrSexp, valueSexp]) = do
  ptr <- sexpToExp ptrSexp
  value <- sexpToExp valueSexp
  pure $ SetPointer ptr value
sexpToExp (SList [SAtom "get-ptr", ptrSexp]) = GetPointer <$> sexpToExp ptrSexp
sexpToExp (SList [SAtom "alloc", valueSexp]) = Alloc <$> sexpToExp valueSexp
sexpToExp (SList [SAtom "dealloc", valueSexp]) = Dealloc <$> sexpToExp valueSexp
sexpToExp (SList [SAtom "if", condSexp, trueSexp, falseSexp]) = do
  cond <- sexpToExp condSexp
  true <- sexpToExp trueSexp
  false <- sexpToExp falseSexp
  pure $ Ite cond true false
sexpToExp (SList (SAtom "while" : condSexp : bodySexps)) = do
  cond <- sexpToExp condSexp
  body <- sexpsToExp bodySexps
  pure $ While cond body
sexpToExp (SList (SAtom "match" : valueSexp : casesSexps)) = do
  value <- sexpToExp valueSexp
  cases <-
    casesSexps
      & mapM
        ( \case
            SList (SList [SAtom constructor, SAtom binding] : bodySexps) -> do
              body <- sexpsToExp bodySexps
              pure $ MatchCase {matchedConstructor = constructor, matchedBinding = binding, caseBody = body}
            _ -> Left "Bad case syntax"
        )
  pure $ Match value cases
sexpToExp (SList (SAtom funName : argsSexps)) = do
  args <- mapM sexpToExp argsSexps
  pure $ Call funName args
sexpToExp (SList []) = pure UnitLiteral
sexpToExp expr = Left [i|Bad expression syntax #{expr}|]

parseSexps :: String -> String -> ParseResult [SExp]
parseSexps filename input =
  case parse (many sexp <* eof) filename input of
    Left err -> Left $ errorBundlePretty err
    Right output -> Right output

parseProgram :: String -> String -> ParseResult Program
parseProgram filename input = parseSexps filename input >>= sexpsToProgram
