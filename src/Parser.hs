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
  try (bind <|> protect <|> sign)
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
       
ctype :: CParser Type
ctype =
  try (unitty <|> intty)
  <|> sumty <|> prodty
  <|> funty  <|> saysty

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
  ty1 <- ctype
  ty2 <- ctype
  return (SumTy ty1 ty2)

prodty :: CParser Type
prodty = do
  symbol "x"
  ty1 <- ctype
  ty2 <- ctype
  return (ProdTy ty1 ty2)

funty :: CParser Type
funty = do
  symbol "->"
  x <- identifier
  ty1 <- ctype
  ty2 <- ctype
  return (FunTy x ty1 ty2)

  
lam :: CParser Term
lam = do symbol "\\"
         x <- identifier
         symbol ":"
         ty <- ctype
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

principal :: CParser Principal
principal =
    primitive
   <|> computation


   
primitive :: CParser Principal
primitive = do l <- identifier
               return (N l)



computation :: CParser Principal
computation = do reserved "code{"
                 t <- term
                 symbol "}"
                 return (Code t)
 
saysty :: CParser Type
saysty = do
  p <- principal
  reserved "says"
  ty <- ctype
  return (SaysTy (PrinTy p) ty)

protect :: CParser Term
protect = do reserved "eta"
             p <- principal
             t <- term
             return (UnitM p t)

sign :: CParser Term
sign = do reserved "sign"
          p <- principal
          ty <- ctype
          return (Sign p ty)

tee :: CParser Term
tee = do reserved "mu"
         t <- identifier
         e <- term
         return (Mu t e)

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
