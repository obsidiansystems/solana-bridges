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
  contract <- deploySolanaClientContract node defaultSolanaRPCConfig
  testMerkle16 node contract

testMerkle16 :: Provider -> Address -> IO ()
testMerkle16 node contract = hspec $ describe "16-ary merkle proof of inclusions verification" $ do
  let
    verify expected proof root leaf offset = do
      res <- runExceptT (verifyMerkleProof node contract proof root leaf offset)
      res `shouldBe` Right expected

  it "does not access offset when proof is empty " $ do
    verify True [] (sha256 $ BS.singleton 0) (BS.singleton 0) 99999

  it "accepts valid proofs" $ do
    verify True [leaves "a"] (merkleParent $ leaves "a") "aa" 0x0

    verify True [leaves "a", branches] (merkleParent branches) "aa" 0x00
    verify True [leaves "a", branches] (merkleParent branches) "ap" 0x0f
    verify True [leaves "b", branches] (merkleParent branches) "bc" 0x12
    verify True [leaves "p", branches] (merkleParent branches) "pa" 0xf0
    verify True [leaves "p", branches] (merkleParent branches) "pp" 0xff

  it "rejects invalid proofs" $ do
    verify False [leaves "a"] (merkleParent $ leaves "a") "aa" 0x1

    verify False [leaves "a", branches] (merkleParent branches) "aa" 0x01
    verify False [leaves "a", branches] (merkleParent branches) "ap" 0x00
    verify False [leaves "a", branches] (merkleParent branches) "pp" 0xff

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
