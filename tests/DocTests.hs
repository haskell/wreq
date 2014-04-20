module Main where

import Control.Monad
import System.Directory
import Data.List
import System.FilePath
import Control.Applicative
import Build_doctest (deps)
import Test.DocTest

main :: IO ()
main = do
  srcs <- getSources
  doctest $ [ "-i."
            , "-idist/build/autogen"
            , "-optP-include"
            , "-optPdist/build/autogen/cabal_macros.h"
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
