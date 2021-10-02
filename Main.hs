{-# LANGUAGE LambdaCase #-}
module Main where

import System.Directory
import System.Environment
import Convert

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
main =
  getArgs >>= \case
      [] ->
        getContents >>= (putStrLn . process "")
      [ifName, ofName] ->
        doesFileExist ofName >>= \exist ->
          let
            writeResult = readFile ifName >>= \content ->
              writeFile ofName (process ifName content)
          in
          if exist
           then whenIO confirm writeResult
           else writeResult
      _ ->
        putStrLn "Usage: runghc Main.hs [-- <input-file> <output-file>]"
