{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

import Interface.Interaction
import Interface.CommandLine

import Options.Applicative


main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> parser) fullDesc
