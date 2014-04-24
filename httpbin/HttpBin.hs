module Main (main) where

import HttpBin.Server (serve)
import Snap.Http.Server.Config (commandLineConfig)

main :: IO ()
main = serve commandLineConfig
