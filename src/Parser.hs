module Parser where

import qualified Data.ByteString.Char8 as B
import Text.Parsec
import Text.Parsec.Combinator (between, sepBy1, chainr1)
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as P
import Data.List (elemIndex)
import Data.Map as M

import CoalTerm
--import CalderModule

data Info = Info { row :: Int, col :: Int } deriving (Show)

type CParser = Parsec String () 

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

     
binding :: CParser (String, Term)
binding =
  do name <- identifier
     symbol "="
     t <- term
     return (name, t)
  
gterm :: CParser Term
gterm =
  try (bind <|> protect)
  <|> try unit
  <|> try (first <|> second <|> pair)
  <|> (try injl <|> try injr <?> "boolean literal")
  <|> intLiteral
  <|> lam <|> parens term
  <|> try ite
  <|> (Var <$> identifier)

unaryPrim :: String -> (Term -> Term) -> CParser Term
unaryPrim s f =
  do symbol s
     t <- gterm
     return (f t)

first :: CParser Term
first = unaryPrim "fst" Fst

second :: CParser Term
second = unaryPrim "snd" Snd

pair :: CParser Term
pair = parens (do a <- term
                  P.comma lexer
                  b <- term
                  return (Pair a b)) <?> "a pair"


unit :: CParser Term
unit = symbol "()" >> return Unit


injl :: CParser Term
injl =  do reserved  "injl"
           e <- term
           reserved "as"
           ty <- sumty
           return (InjL e ty)


injr :: CParser Term
injr =  do reserved  "injr"
           e <- term
           reserved "as"
           ty <- sumty
           return (InjR e ty)

cond :: CParser Term
cond = injl <|> injr


ite :: CParser Term
ite = do reserved "case"
         c <- cond <?> "branch condition"
         reserved "inj1"
         x <- identifier
         e1 <- gterm
         reserved "inj2"
         y <- identifier
         e2 <- gterm
         return (Case c x e1 y e2)

intLiteral :: CParser Term
intLiteral = I <$> integer


term :: CParser Term
term =  (chainl1 gterm (return App))
       
dtype :: CParser Type
dtype =
  try (unitty <|> intty)
  <|> sumty <|> prodty
  <|> funty
  <|> saysty <|>  dtype

unitty :: CParser Type
unitty = do
  symbol "()"
  return UnitTy

intty :: CParser Type
intty = do
  symbol "int"
  return IntTy


sumty :: CParser Type
sumty = do
  symbol "+"
  ty1 <- dtype
  ty2 <- dtype
  return (SumTy ty1 ty2)

prodty :: CParser Type
prodty = do
  symbol "x"
  ty1 <- dtype
  ty2 <- dtype
  return (ProdTy ty1 ty2)

funty :: CParser Type
funty = do
  symbol "->"
  ty1 <- dtype
  ty2 <- dtype
  return (FunTy ty1 ty2)

saysty :: CParser Type
saysty = do
  l <- principal
  reserved "says"
  ty <- dtype
  return (SaysTy l ty)
  
lam :: CParser Term
lam = do symbol "\\"
         x <- identifier
         symbol ":"
         ty <- dtype
         symbol "["
         pc <- principal
         symbol ","
         theta <- principal 
         symbol "]"
         symbol "."
         t <- term
         return (Abs x ty t)

bind :: CParser Term
bind = do reserved "bind"
          x <- identifier
          symbol "=" 
          t1 <- term
          reserved "in"
          t2 <- term
          return (Bind x t1 t2)

dflatelabel :: CParser Label
dflatelabel  = do l <- identifier
                  return  (Prim (N l))
            
protect :: CParser Term
protect = do reserved "eta"
             l <- dflatelabel
             t <- term
             return (Protect l t)

principal :: CParser Principal
principal =
  top <|> bottom
  <|> primitive
--  <|> computation
--  <|> (Prim  ltype)

   
primitive :: CParser Principal
primitive = do l <- identifier
               return (Prim (N l))

top, bottom :: CParser Principal
top = symbol "top" >> return (Prim T)

bottom = symbol "bot" >> return (Prim B)


{-
computation :: CParser Principal
computation = do
  l <- identifier
  return (T l)
 -}
  
tee :: CParser Term
tee = do reserved "tee"
         t <- principal
         e <- term
         return (TEE t e)

parserprincipal :: CParser Principal
parserprincipal = do whiteSpace
                     t <- principal
                     eof
                     return t

parser :: CParser Term
parser = do whiteSpace
            t <- term
            eof
            return t
{-
 Using a Haskell lexer as a base
-}
lexer = P.makeTokenParser haskellDef
parens = P.parens lexer
braces = P.braces lexer
whiteSpace = P.whiteSpace lexer
identifier = P.identifier lexer
symbol = P.symbol lexer
integer = P.integer lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
