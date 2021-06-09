{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module CoalTerm where

import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

import Data.Map as M
import Data.Set as S
import Data.List as L

data Principal =
  N String
  | Code  Term 
  deriving (Generic, Typeable)

data Kind =
  Star
  | Prop
  | Prin
  | Comp
  | VarKind String Type Kind
  | TVarKind String Kind Kind
  deriving (Generic, Typeable)


data Type =
  UnitTy
  | IntTy
  | BoolTy
  | ListTy
  | TyVar String
  | SumTy Type Type
  | ProdTy Type Type
  | FunTy String Type  Type
  | TFunTy String Kind Type
  | PrinTy Principal
  | SaysTy Type Type
  | TyTeApp Type Term  -- Type Term application
  | TyTyApp Type Type  -- Type Type application
  | AST
  | Terminate          -- Pre-defined predicate
  deriving (Generic, Typeable)

data Term =
  Var String
  | Unit
  | I Integer
  | CTrue
  | CFalse
  | InjL Term Type   -- inl t as τ₁ + τ₂
  | InjR Term Type   -- inr t as τ₁ + τ₂
  | Abs String Type  Term
  | TAbs String Kind Term  -- \Lamba X. e
  | TApp Term Type
  | App Term Term
  | Case Term String Term String Term -- case injᵢ v of inj₁(x). e₁ | inj₂(x). e₂
  | Pair Term Term
  | Fst Term
  | Snd Term
  | Bind String Term Term
  | UnitM Principal Term
  | Sign Principal Type
  | Mu String  Term
  -- Extensions --
  | Represent Term
  | Inc Term
  | Nil
  | Cons Term Term
  | Letrec String Term Term
  | Match Term [(Term, Term)] -- pattern matching
  -- BPF Extensions --
  | EBPFAdd  
  | EBPFSub
  | EBPFMov
  deriving (Show, Typeable, Generic)

type  Policy = Type




instance Show Principal where
  show  (N p) = p
  show (Code t) = "code{" ++ (show t) ++ "}"

instance Show Kind where
  show Star = "*"
  show Prop = "Prop"
  show Prin = "Prin"
  show Comp = "Comp"
  show (VarKind x ty k) = (show x) ++ ":" ++ (show ty) ++ "->" ++ (show k)
  show (TVarKind y k1 k2) = (show y) ++ ":" ++ (show k1) ++ "->" ++ (show k2)

instance Show Type where
  show UnitTy = "()"
  show IntTy = "int"
  show (SumTy ty1 ty2) = (show ty1) ++ " + "  ++  (show ty2)
  show (ProdTy ty1 ty2) =  (show ty1) ++ "x" ++  (show ty2)
  show (FunTy x ty1  ty2 ) = (show x) ++ ":" ++ (show ty1) ++ "->" ++  (show ty2)
  show (SaysTy l ty) = (show l) ++ (show ty) 
  show (TFunTy y k ty) = (show y) ++ ":" ++ (show k) ++ "." ++ (show ty)
  show (PrinTy p) = show p
  show (TyTeApp ty t) = "(" ++ (show ty) ++ " " ++ (show t) ++ ")"
  show (TyTyApp ty1 ty2) = "(" ++ (show ty1) ++ " " ++ (show ty2) ++ ")"
  show AST = "AST"
  show Terminate = "(Predicate) Terminate"


-- Principal equivalences. Syntactic equivalence for now.
instance Eq Principal where
  (N s1) == (N s2) = s1 == s2
  (Code t1) == (Code t2) = t1 == t2
  _ == _ = False

-- Kind equivalences. Syntactic equivalence for now.  
instance Eq Kind where
  Star == Star = True
  Prop == Prop = True
  Prin == Prin = True
  Comp == Comp = True
  (VarKind x1 ty1 k1) == (VarKind x2 ty2 k2) = (x1 == x2) && (ty1 == ty2) && (k1 == k2)
  (TVarKind x1 k11 k12) == (TVarKind x2 k21 k22) = (x1 == x2) && (k11 == k21) && (k12 == k22)
  _ == _ = False

-- Type equivalences. Syntactic equivalence for now.  
instance Eq Type where
  UnitTy == UnitTy = True
  IntTy == IntTy = True
  (SumTy ty11 ty12) == (SumTy ty21 ty22) = (ty11 == ty21) && (ty12 == ty22) 
  (ProdTy ty11 ty12) == (ProdTy ty21 ty22) = (ty11 == ty21) && (ty12 == ty22)
  (FunTy x1 ty11 ty12) == (FunTy x2 ty21 ty22) = (x1 == x2) && (ty11 == ty21) && (ty12 == ty22)
  (SaysTy p1 ty1) == (SaysTy p2 ty2) = (p1 == p2) && (ty1 == ty2)
  (TFunTy y1 k1 ty1) == (TFunTy y2 k2 ty2) = (y1 == y2) && (k1 == k2) && (ty1 == ty2)
  (PrinTy p1) == (PrinTy p2) = (p1 == p2)
  (TyTeApp ty1 t1) == (TyTeApp ty2 t2) = (ty1 == ty2) && (t1 == t2)
  (TyTyApp ty11 ty12) == (TyTyApp ty21 ty22) = (ty11 == ty21) && (ty12 == ty22)
  AST == AST = True
  Terminate == Terminate = True
  _ == _ = False
  
-- Term equivalences. Syntactic equivalence for now.
instance Eq Term where
   (Var s1) == (Var s2) = s1 == s2
   Unit == Unit = True
   (I i1) == (I i2) = i1 == i2
   CTrue == CTrue = True
   CFalse == CFalse = True
   (InjL t1 ty1) == (InjL t2 ty2) = (t1 == t2) && (ty1 == ty2)   -- inl t as τ₁ + τ₂
   (InjR t1 ty1) == (InjR t2 ty2) = (t1 == t2) && (ty1 == ty2)  -- inr t as τ₁ + τ₂
   (Abs x1 ty1 t1) == (Abs x2 ty2 t2) = (x1 == x2) && (ty1 == ty2) && (t1 == t2)
   (TAbs x1 k1 t1) == (TAbs x2 k2 t2) = (x1 == x2) && (k1 == k2) && (t1 == t2) 
   (TApp t1 ty1) == (TApp t2 ty2) = (t1 == t2) && (ty1 == ty2) 
   (App t11 t12) == (App t21 t22) = (t11 == t21) && (t12 == t22)
   (Case t1 x1 tt1 y1 tf1) == (Case t2 x2 tt2 y2 tf2) = (t1 == t2) && (x1 == x2) && (tt1 == tt2) && (tf1 == tf2) -- case injᵢ v of inj₁(x). e₁ | inj₂(x). e₂
   (Pair t11 t12) == (Pair t21 t22) = (t11 == t21) && (t12 == t22)
   (Fst t1) == (Fst t2) = (t1 == t2)
   (Snd t1) == (Snd t2) = (t1 == t2)
   (Bind x1 t11 t12) == (Bind x2 t21 t22)  = (x1 == x2) && (t11 == t21) && (t12 == t22)
   (UnitM p1 t1) == (UnitM p2 t2) = (p1 == p2) && (t1 == t2)
   (Sign p1 ty1) == (Sign p2 ty2) = (p1 == p2) && (ty1 == ty2)
   (Mu p1 t1) == (Mu p2 t2) = (p1 == p2) && (t1 == t2)
   (Represent t1) == (Represent t2) = (t1 == t2)
   _ == _ = False
