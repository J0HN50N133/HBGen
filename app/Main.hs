{-# LANGUAGE LambdaCase #-}
module Main where

import qualified HBGen
import OptParse

import System.Directory
import System.IO
import System.Exit

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

main :: IO()
main = do
  options <- parse
  case options of
    ConvertDir input output ->
      HBGen.convertDirectory input output

    ConvertSingle input output -> do
      (title, inputHandle) <-
        case input of
          Stdin -> pure ("", stdin)
          InputFile file ->
            (,) file <$> openFile file ReadMode
      outputHandle <-
        case output of
          Stdout -> pure stdout
          OutputFile file -> do
            exists <- doesFileExist file
            shouldOpenFile <-
              if exists
                then confirm
                else pure True
            if shouldOpenFile
              then
                openFile file WriteMode
              else
                exitFailure

      HBGen.convertSingle title inputHandle outputHandle
      hClose inputHandle
      hClose outputHandle
