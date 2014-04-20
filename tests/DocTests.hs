-- I don't know who originally wrote this, but I picked it up from
-- Edward Kmett's folds package, and subsequently modified it.

module Main where

import Build_doctest (deps)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (filterM)
import Data.List (isPrefixOf, isSuffixOf)
import System.Directory
import System.FilePath ((</>))
import Test.DocTest (doctest)

main :: IO ()
main = do
  srcs <- getSources
  dist <- getDistDir
  doctest $ [ "-i."
            , "-i" ++ dist ++ "/build/autogen"
            , "-optP-include"
            , "-optP" ++ dist ++ "/build/autogen/cabal_macros.h"
            , "-hide-all-packages"
            ] ++ map ("-package="++) deps ++ srcs

getSources :: IO [FilePath]
getSources = filter (isSuffixOf ".hs") <$> go "Network"
  where
    go dir = do
      (dirs, files) <- getFilesAndDirectories dir
      (files ++) . concat <$> mapM go dirs

getFilesAndDirectories :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories dir = do
  c <- map (dir </>) . filter (`notElem` ["..", "."]) <$>
       getDirectoryContents dir
  (,) <$> filterM doesDirectoryExist c <*> filterM doesFileExist c

getDistDir :: IO FilePath
getDistDir = do
  names <- getDirectoryContents "dist"
  return $ case filter ("dist-sandbox-" `isPrefixOf`) names of
             (d:_) -> "dist/" ++ d
             _     -> "dist"
