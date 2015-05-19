module Main where

import           Control.Applicative
import           Data.Foldable (forM_)
import           Data.List (isPrefixOf, intercalate)
import           Data.Maybe
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.FilePath.Find
import           System.Process
import           Test.DocTest

main :: IO ()
main = do
    files <- find always (extension ==? ".hs") "src"
    cabalMacrosFile <- getCabalMacrosFile
    doctest $ [ "-isrc"
              , "-optP-include"
              , "-optP" ++ cabalMacrosFile
              , "-XOverloadedStrings"
              , "-XFlexibleInstances"
              , "-XMultiParamTypeClasses"
              ] ++ files

getCabalMacrosFile :: IO FilePath
getCabalMacrosFile = do
  contents <- getDirectoryContents "dist"
  let rest = "build" </> "autogen" </> "cabal_macros.h"
  return $ case filter ("dist-sandbox-" `isPrefixOf`) contents of
    [x] -> "dist" </> x </> rest
    _ -> "dist" </> rest
