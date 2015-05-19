#!/usr/bin/env runhaskell

import           Control.Monad
import           Data.Foldable
import           Data.List
import           System.Directory
import           System.FilePath
import           System.Process

main :: IO ()
main = do
  servantPackages <- lines <$> readFile "sources.txt"
  createGlobalSandbox servantPackages
  buildAndTest servantPackages
  dists servantPackages

createGlobalSandbox servantPackages = do
  sh "cabal sandbox init"
  sh ("cabal install --only-dep --enable-tests " ++ unwords servantPackages)

buildAndTest servantPackages = do
  forM_ servantPackages $ \ servantPackage -> do
    setCurrentDirectory servantPackage
    exists <- doesDirectoryExist ".cabal-sandbox"
    when (not exists) $
      sh "stack-sandbox --parent .."
    let deps = internalDependencies servantPackage
    forM_ deps $ \ dep -> do
      sh ("cabal sandbox add-source " ++ (".." </> dep))
    when (not (null deps)) $
      sh ("cabal install " ++ unwords deps)
    sh "cabal configure --enable-tests --ghc-options='-Wall'"
    sh "cabal build"
    sh "cabal test"
    sh "cabal clean"
    setCurrentDirectory ".."

dists servantPackages = do
  distFiles <- forM servantPackages $ \ servantPackage -> do
    setCurrentDirectory servantPackage
    sh "cabal sdist"
    [distFile] <-
      filter (".tar.gz" `isSuffixOf`) <$>
      getDirectoryContents "dist"
    distFileAbsolute <- canonicalizePath ("dist" </> distFile)
    setCurrentDirectory ".."
    return distFileAbsolute
  createDirectory "dist-tests"
  setCurrentDirectory "dist-tests"
  sh "stack-sandbox --parent .."
  sh ("cabal install --enable-tests " ++ unwords distFiles)
  setCurrentDirectory ".."
  sh "rm -rf dist-tests"

internalDependencies p = case p of
  "servant" -> []
  "servant-client" -> ["servant", "servant-server"]
  "servant-docs" -> ["servant"]
  "servant-jquery" -> ["servant", "servant-server"]
  "servant-server" -> ["servant"]
  "servant-examples" ->
    "servant" :
    "servant-client" :
    "servant-docs" :
    "servant-jquery" :
    "servant-lucid" :
    "servant-server" :
    []
  "servant-blaze" -> ["servant"]
  "servant-lucid" -> ["servant"]
  _ -> []

sh :: String -> IO ()
sh command = do
  dir <- getCurrentDirectory
  putStrLn ("in: " ++ dir)
  putStrLn ("==> (" ++ dir ++ ") " ++ command)
  -- putStr "confirmed?" >> getLine
  callCommand command
