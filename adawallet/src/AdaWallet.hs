{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- HLINT ignore "Use let" -}

module AdaWallet (main) where

import Control.Monad
import Options.Applicative
import System.Directory
import Prelude

main :: IO ()
main = join $ execParser (info opts idm)

foo :: IO ()
foo = putStrLn "Foo"

data CLI = CLI
  { quiet :: Bool
  }

opts :: Parser (IO ())
opts =
  hsubparser
    ( command "wipe" (info (pure wipeCommand) (progDesc "wipe all state"))
    )

wipeCommand :: IO ()
wipeCommand = do
  dir <- stateDir
  System.Directory.removeDirectoryRecursive dir

stateDir :: IO FilePath
stateDir =
  System.Directory.getXdgDirectory System.Directory.XdgData "adawallet"
