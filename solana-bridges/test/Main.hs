{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.Except (runExceptT)
import Crypto.Hash (Digest, SHA256)
import Data.Bool (bool)
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Solidity.Prim.Address (Address)
import Data.Tree (Tree (..))
import GHC.Word (Word64)
import Network.Web3.Provider (Provider)
import Test.Hspec (it, describe, hspec, shouldBe)

import Ethereum.Contracts
import Solana.Relayer
import Solana.Utils

main :: IO ()
main = pure ()
