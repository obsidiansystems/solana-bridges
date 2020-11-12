{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.Except (runExceptT)
import Crypto.Hash (Digest, SHA256, hash)
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

    height1, height2 :: [Digest SHA256]
    height1 = fmap (sha256 . BS.singleton) [0..15]
    height2 = merkleParent height1 : fmap (sha256 . BS.singleton) [1..15]

  it "does not access offset when proof is empty " $ do
    verify True  [] (sha256 $ BS.singleton 0) (BS.singleton 0) 99999
  it "verifies proof with length 1 " $ do
    verify True [height1] (merkleParent height1) (BS.singleton 1) 1
  it "verifies proof with length 2 " $ do
    verify True [height1, height2] (merkleParent height2) (BS.singleton 2) 2

  it "rejects invalid proofs" $ do
    verify False [height1] (merkleParent height1) (BS.singleton 0) 1
    verify False [height1, height2] (merkleParent height2) (BS.singleton 1) 2
    verify False [height1, height1] (merkleParent height2) (BS.singleton 2) 2

sha256 :: ByteString -> Digest SHA256
sha256 = hash

merkleParent :: [Digest SHA256] -> Digest SHA256
merkleParent = sha256 . BS.concat . fmap ByteArray.convert

verifyProof
  :: Provider
  -> Address
  -> [[Digest SHA256]]
  -> Digest SHA256
  -> ByteString
  -> Word64
  -> IO ()
verifyProof node contract proof root leaf offset = do
  putStrLn $ "Verifying inclusion proof"
  putStrLn $ unlines $ fmap ("  " <>) $
    [ "of length: " <> show (length proof)
    , "for value: " <> show leaf
    , "at offset: " <> show offset
    , "of merkle tree with root: " <> show root
    , "on contract with address: " <> show contract
    ]
  runExceptT (verifyMerkleProof node contract proof root leaf offset) >>= \case
    Left err -> putStrLn $ "Error: " <> show err
    Right res -> putStrLn $ "Verification " <> bool "failed" "succeeded" res

labels :: [Char]
labels = "abcdefghijklmnop"

leaves :: ByteString -> [Digest SHA256]
leaves x = fmap (sha256 . (x <>) . BSC.singleton) labels

branches :: [Digest SHA256]
branches = fmap (merkleParent . leaves . BSC.singleton) labels

mkTree :: [Char] -> Tree String
mkTree ls = Node "" $ flip fmap ls $ \l ->
  Node [] $ flip fmap ls $ \s ->
    Node [l, s] []
