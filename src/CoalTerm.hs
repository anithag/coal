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

-- primitive labels
data L =
   N String   -- primitive strings
  |T   -- Top
  |B   -- Bot
  deriving (Eq, Ord, Generic, Typeable)

instance Show L where
  show (N s) = s
  show T = "⊤"
  show B = "⊥"
  
data Principal =
  Prim L
  | Tee String  --computation principals
  deriving (Ord, Generic, Typeable)

instance Eq Principal where
  (Prim (N s1)) == (Prim (N s2)) = s1 == s2
  (Prim T) == (Prim T) = True
  (Prim B) == (Prim B) = True
  _ == _ = False

{-
instance Ord Principal where
  compare (Prim s1) (Prim s2) = compare s1 s2
  compare  ((:→) p)  ((:→) q) = compare p q
  compare  ((:←) p)  ((:←) q) = compare p q
  compare  (p₁ :∧ q₁) (p₂ :∧ q₂) = if (p₁ == q₂) && (p₂ == q₁) then EQ
    else if (compare p₁ p₂) == EQ then compare q₁ q₂
    else compare p₁ p₂
  compare  (p₁ :∨ q₁) (p₂ :∨ q₂) = if (p₁ == q₂) && (p₂ == q₁) then EQ
    else if (compare p₁ p₂) == EQ then compare q₁ q₂
    else compare p₁ p₂
-}


instance Show Principal where
  show (Prim  (N p)) = p
  show (Prim T) = "⊤"
  show (Prim B) = "⊥"
  
type Label = Principal
type PC = Principal



data Type =
  Principal :> Principal
  | UnitTy
  | IntTy
  | BoolTy
  | TyVar String
  | SumTy Type Type
  | ProdTy Type Type
  | FunTy Type  Type
  | SaysTy Label Type
  deriving (Eq, Generic, Typeable)


instance Show Type where
  show UnitTy = "()"
  show IntTy = " int "
  show (SumTy ty2 ty1) = (show ty1) ++ " + "  ++  (show ty1)
  show (ProdTy ty1 ty2) =  (show ty1) ++ "x" ++  (show ty2)
  show (FunTy ty1  ty2 ) = (show ty1) ++  (show ty2)
  show (SaysTy l ty) = (show l) ++ (show ty) 
  

--type TyEnv = M.Map String Type

data Term =
  Var String
  | Unit
  | I Integer
  | InjL Term Type   -- inl t as τ₁ + τ₂
  | InjR Term Type   -- inr t as τ₁ + τ₂
  | Abs String Type  Term
  | App Term Term
  | Case Term String Term String Term -- case injᵢ v of inj₁(x). e₁ | inj₂(x). e₂
  | Pair Term Term
  | Fst Term
  | Snd Term
  | Bind String Term Term
  | Protect Label Term
  | TEE Label  Term -- ensure no recursive TEE
  deriving (Eq, Show, Typeable, Generic)






