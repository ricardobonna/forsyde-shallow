-- Main test module, it must be run from the root directory of the project
module Main (main) where

import Install

import Control.Monad (liftM)
import System.Environment (getArgs, getEnv)
import System.FilePath (splitDirectories)


main :: IO ()
main = do
 needed <- testNeeded
 if needed 
   then testInstall
   else putStrLn "There is no need to run the test suite."


-- Check if we need to do the tests, This will be true unless an
-- automatic test is done from darcs, in which case we only need
-- to perform the tests if the affected files are ForSyDe.cabal
-- Setup.hs or any file under src/, tests/ or lib/
testNeeded :: IO Bool
testNeeded = do
  mDARCS_FILES <- catch (liftM Just $ getEnv "DARCS_FILES")
                        (\_ -> return Nothing)
  case mDARCS_FILES of
    Nothing -> return True
    Just multiLine -> (return.checkAffected) multiLine
  where checkAffected str = any affected (lines str) 
        affected file = file `elem` ["Setup.hs", "ForSyDe.cabal"] ||
                        firstDir file `elem` ["src", "tests", "lib"]         
        firstDir filePath = 
               let sDirName = splitDirectories filePath
               in case sDirName of
                  [_]     -> ""
                  (x:xs)  -> x

                                