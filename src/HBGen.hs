{-# LANGUAGE LambdaCase #-}
module HBGen
  ( main )
    where

import System.Directory
import System.Environment
import HBGen.Convert

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action =
  cond >>= \result ->
    if result
      then action
      else pure ()

confirm :: IO Bool
confirm =
  putStrLn "Output file already exist, do you want to overwrite it? (y/n)" *>
    getLine >>= \case
        "y" -> pure True
        "n" -> pure False
        _ ->
          putStrLn "Invalid response. use y or n" *>
            confirm

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- No args
    [] -> do
      contents <- getContents
      putStrLn (process "" contents)
    -- with input and output
    [ifName, ofName] -> do
      exist <- doesFileExist ofName
      content <- readFile ifName
      let writeResult = writeFile ofName (process ifName content)
      if exist
         then whenIO confirm writeResult
         else writeResult
    _  ->
        putStrLn "Usage: runghc Main.hs [-- <input-file> <output-file>]"
