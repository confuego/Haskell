{-

to compile this file:
     ghc --make -o theExecutableName CompilableExamples.hs

th run that executable:
     ./theExecutableName a s d f args go here

-}

-- if you choose to make this a module, it must be named Main 
-- in order to generate an executable from it.
module Main where

import System.Environment  -- getArgs
import System.IO           -- openFile, hClose, hPutStrLn

main :: IO ()
main = do 
  (x:xs)  <- getArgs
  theFile <- openFile "ex.txt" WriteMode 
  loopPutter theFile (x:xs)
  hClose theFile
  putStrLn $ "first arg: " ++ (show x)
  putStr ( "hello world!\n")
         

loopPutter :: Handle -> [String] -> IO ()
loopPutter h (x:xs) = do
  hPutStrLn h (show x)
  loopPutter h xs
loopPutter h [] = do
  hPutStr h "all done!"
  return ()
