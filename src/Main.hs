module Main where

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
            case (eval $  Cfg {Simulator.term = t, env = M.empty}) of 
              Just r ->             putStrLn "Done"
              Nothing -> putStrLn "Failed"
              

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

  
