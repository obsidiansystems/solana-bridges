{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TypeApplications #-}

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (runExceptT)
import Crypto.Hash (SHA256, hash)
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Default
import Data.Functor (void)
import Data.Solidity.Prim.Address (Address)
import Network.Web3.Provider (Provider)
import Test.Hspec (Spec, it, describe, hspec, shouldBe)

import Ethereum.Contracts
import Solana.Relayer

main :: IO ()
main = pure ()

testWithRunningNode :: Provider -> IO ()
testWithRunningNode node = do
  contract <- deploySolanaClientContract node
  testMerkle16 node contract

testMerkle16 :: Provider -> Address -> IO ()
testMerkle16 node contract = hspec $ describe "16-ary merkle proof of inclusions verification" $ do
  let
    verify expected proof root leaf offset = do
      res <- runExceptT (verifyMerkleProof node contract proof root leaf offset)
      res `shouldBe` Right expected

    sha256 = hash @ByteString @SHA256
    layer2Hash = sha256 . BS.concat
    word2hash =  sha256 . BS.singleton
    hash2bytes = ByteArray.convert
    height0, height1 :: [ByteString]
    height0 = fmap hash2bytes $ fmap word2hash [0..15]
    height1 = fmap hash2bytes $ layer2Hash height0 : fmap word2hash [1..15]

  it "does not access offset when proof is empty " $ do
    verify True  [] (sha256 $ BS.singleton 0) (BS.singleton 0) 99999
  it "verifies proof with length 1 " $ do
    verify True [height0] (layer2Hash height0) (BS.singleton 1) 1
  it "verifies proof with length 2 " $ do
    verify True [height0, height1] (layer2Hash height1) (BS.singleton 2) 2

  it "rejects invalid proofs" $ do
    verify False [height0] (layer2Hash height0) (BS.singleton 0) 1
    verify False [height0, height1] (layer2Hash height1) (BS.singleton 1) 2
    verify False [height0, height0] (layer2Hash height1) (BS.singleton 2) 2
