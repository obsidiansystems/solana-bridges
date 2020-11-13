{-# LANGUAGE TemplateHaskell #-}

module Ethereum.Contracts.TH where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Instances.TH.Lift ()
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Process

compileSolidity :: Q Exp
compileSolidity = do
  let contract = "SolanaClient"
      client = "solana-client"
      source = client <> "/" <> contract <> ".sol"
      dist = client <> "/dist"

  -- https://gitlab.haskell.org/ghc/ghc/-/issues/18330
  addDependentFile source

  (abi, bin) <- runIO $ do
    callProcess "solc"
      [ "--optimize"
      , "--abi", "--bin"
      , source
      , "-o", dist, "--overwrite"
      ]
    abi <- BS.readFile $ dist <> "/" <> contract <> ".abi"
    bin <- BS.readFile $ dist <> "/" <> contract <> ".bin"
    pure (abi, bin)

  [| (T.unpack (T.decodeUtf8 abi), bin) |]
