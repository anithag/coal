module Main where

import CoalTerm
import TypeChecker
import Simulator
import qualified Data.Map as M
import qualified Data.List as L
import Text.Parsec
import qualified Parser as P

import System.Directory
import Control.Exception
import Control.Monad

testFile :: FilePath -> IO ()
testFile fp =
  do s <- readFile fp
     putStrLn $ "Source file: " ++ fp
     putStrLn $ "\nContents: \n" ++ s
     case parse P.parser fp s of
       Left err -> fail (show err)
       Right t ->
         do putStrLn $ "\nParsed AST:"
            print t
            putStrLn ""
            typeCheckWith basicEnv t
            putStrLn "\nEvaluation:"
            cfg <- (eval $  Cfg {Simulator.term = t, env = M.empty})
            putStrLn $ "Done"
              

data TestResult =
    TestOK
  | TestFailed
  deriving (Eq, Show)

drive :: (FilePath -> IO ()) -> FilePath -> IO TestResult
drive f fp =
  catch (f fp >> return TestOK)
    (\(SomeException e) ->
       do putStrLn "* Test failed:"
          print e
          return TestFailed)

testsDirectory :: FilePath
testsDirectory = "src/tests"


main :: IO ()
main =
  do fs <- filter ((=="cal").extName) <$> listDirectory testsDirectory
     rs <- withCurrentDirectory testsDirectory $
        forM (zip [0..] fs) $ \(n,fp) ->
                                do putStrLn $ "* Test " ++ show n
                                   drive testFile fp
     putStrLn $ "* Summary: (" ++ show (length rs) ++ " test(s) in total)"
     let (ok, failed) = L.partition (==TestOK) rs
     putStrLn $ show (length ok) ++ " test(s) passed, "
       ++ show (length failed) ++ " test(s) failed."
     return ()
       where extName = reverse . takeWhile ((/=) '.') . reverse


-- Complex  ebpfVerifier
-- Counts number of ebpf instructions
cebpfVerifier :: Term
cebpfVerifier =
  (TAbs "Z" Comp
    (Abs "x" (TyTyApp AST (TyVar "Z"))
       (Letrec "loop"
               (Abs "counter" IntTy
                  (Abs "prog" ListTy
                    (Match (Var "prog") [(Nil, (Var "counter")), (Cons (Var "t") (Var "tail"), 
                           (Match (Var "t") [(EBPFAdd,  (App (App (Var "loop") (Inc (Var "counter")) )  (Var "tail"))), (EBPFSub,  (App (App (Var "loop") (Inc (Var "counter")) )  (Var "tail")) ), (EBPFMov,  (App (App (Var "loop") (Inc (Var "counter")) )  (Var "tail")) ) ]) )
                    ]) -- end of outer match
                  )
               )
               (App (App (Var "loop") (I 0)) (Var "x")) 
       ) -- end of letrec
    )
   ) 


-- Trivial ebpfVerifier 
ebpfVerifier :: Term
ebpfVerifier =
  -- Type of ebpfVerifier is ΛZ::Comp.AST Z -> ebpfVerifier says (Terminate Z)
  (TAbs "Z" Comp
    (Abs "x" (TyTyApp AST (TyVar "Z")) 
         (Sign (Code ebpfVerifier) (TyTyApp Terminate (TyVar "Z"))) -- FIXME: Recursion bomb during printing
    )
   ) 

-- get the reflection of the term
-- ΛX::*.X -> AST X
quote :: Term
quote  = (TAbs "X" Star
           (Abs "x" (TyVar "X") (Represent (Var "x")))
          )

--Invoke ebpfVerifier
--  ebpfVerifier (Code u) (AST (Code u)) 
runebpfVerifier :: Term -> IO Term
runebpfVerifier (Mu "t" e)  = do
  let tprin = Code (Mu "t" e)
  return (App (TApp ebpfVerifier (PrinTy tprin)) (App (TApp quote (PrinTy tprin)) (Mu "t" e))
         )


{- Takes a policy and a proof term
- returns if the proof term satisfies/helps with enforcing the policy
- NOTE: p is hard-coded to be
           (forall U. ebpfVerifier says (Terminate U) -> U speaks-for Kernel
-}
ebpfAuthEngine :: Policy -> Term -> IO Bool
ebpfAuthEngine pol u = do
    cfg <- constructProof u
    case (term cfg) of
      {- Ideally, the  proof term should be type checked. That is the essence of Curry-Howard Isomorphism
       - but here we are optimizing it by directly matching with the required term. This is because we already know
       - the shape of the policy.
      -}
      Sign (Code v) (TyTyApp Terminate (PrinTy (Code u))) -> return (v == ebpfVerifier) -- discharges (U speaks-for Kernel)
      _ -> return False

-- Construct proof for "ebpfVerifier says (Terminate (code u))"
constructProof :: Term -> IO ThreadCfg
constructProof u = do
  t <- runebpfVerifier u
  (eval  Cfg{term = t, env = M.empty})
 
