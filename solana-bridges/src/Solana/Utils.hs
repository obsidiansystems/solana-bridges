{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solana.Utils where

import Control.Lens ((&))
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Crypto.Random.Types
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import Data.Foldable
import qualified Data.Sequence as Seq

import Control.Monad.Except (runExceptT)
import Test.Hspec (it, describe, hspec, shouldBe)

import Ethereum.Contracts
import Solana.Relayer
import Solana.Types

mkSignature :: MonadRandom m => BS.ByteString -> m Ed25519.Signature
mkSignature msg = do
  sk <- Ed25519.generateSecretKey
  pure $ Ed25519.sign sk (Ed25519.toPublic sk) msg

-- curl base64
txnBase64 :: BS.ByteString
txnBase64 = "AZVKAuMoJbnV4PtPG7mMZCBEanoBomhtUHls0FuOa1QgthOeVMVj6VeAUVkn++w9NW1VF2POQ4YaeOEY6T2L0gsBAAMF01L0Cxouih7U0W+rqrqFKJDpZV3XeSpuqaXgRbrAeAqgAVVnFM8Agxw18NmD9pMJiEV64Q4iw2HyJ+BVmNG3+Qan1RcZLwqvxvJl4/t3zHragsUp0L47E24tAFUgAAAABqfVFxjHdMkoVmOYaR1etoteuKObS21cc1VbIQAAAAAHYUgdNXR0u3xNdiTr072z2DVec9EQQ/wNo1OAAAAAAOXoKdOObGil/U5GuEw/MRcdyHgKqsLIJgZiCDePh+iGAQQEAQIDADUCAAAAAQAAAAAAAAAdAAAAAAAAAGjgBRtbPax5AAeZ2uXW052RZQ4g+VXwFZO6iUZSVsuGAA=="

txnBinary :: BS.ByteString
Right txnBinary = Base64.decode txnBase64

txnParsed :: SolanaTxn
txnParsed = Data.Binary.decode $ LBS.fromStrict txnBinary

txnMessage :: SolanaTxnMessage
txnMessage = Data.Binary.decode $ LBS.fromStrict $ BS.drop 65 $ txnBinary

roundtrip :: forall b. Binary b => b -> b
roundtrip = dec . enc
  where
    dec = Data.Binary.decode . LBS.fromStrict
    enc = LBS.toStrict . Data.Binary.encode

verifySigs :: SolanaTxn -> Seq.Seq Bool
verifySigs txn = flip fmap (Seq.zip pks sigs) $ \(pk, sig) -> Ed25519.verify pk msg' sig
  where
    msg = _solanaTxn_message txn
    msg' = LBS.toStrict $ Data.Binary.encode msg

    pks :: Seq.Seq Ed25519.PublicKey
    pks = unCompactArray $ _solanaTxnMessage_addresses msg

    sigs :: Seq.Seq Ed25519.Signature
    sigs = unCompactArray (_solanaTxn_signatures txn)

testParsing :: IO ()
testParsing = do
  let node = def
  contract <- deploySolanaClientContract node defaultSolanaRPCConfig
  hspec $ describe "Parser " $ do
    let roundtrips x = roundtrip x `shouldBe` x

    it "parses compact-u16" $ do
      let parses buffer cursor expected@(w16, _) = do
            roundtrips w16
            res <- runExceptT $ parseCompactWord16 node contract buffer cursor
            res `shouldBe` Right expected

      parses "\x7f" 0 (0x7f, 1)
      parses "\x83\x02" 0 (0x103, 2)
      parses "\xFF\xFF\x03" 0 (0xFFFF, 3)
      parses "\x00\x00\x03" 2 (0x03, 3)

    it "parses compact array of bytes" $ do
      let parses bytes = do
            let buffer = LBS.toStrict $ Data.Binary.encode (CompactByteArray bytes)
            res <- runExceptT $ parseBytes node contract buffer 0
            res `shouldBe` Right (LBS.toStrict bytes, 1 + fromIntegral (LBS.length bytes))

      parses ""
      parses "abc"
      parses "abcdef"

    it "parses instructions" $ do
      for_ (txnMessage & _solanaTxnMessage_instructions) $ \i -> do
        roundtrips i
        let buffer = LBS.toStrict $ Data.Binary.encode i
        res <- runExceptT $ parseInstruction node contract buffer 0
        res `shouldBe` Right (i, fromIntegral (BS.length buffer))

    it "parses bytes32" $ do
      let hash = txnMessage & _solanaTxnMessage_recentBlockHash
      roundtrips hash
      let buffer = LBS.toStrict $ Data.Binary.encode hash
      res <- runExceptT $ parseBytes32 node contract buffer 0
      res `shouldBe` Right (sha256ToBytes32 hash, 32)

    it "parses uint64" $ do
      let uint = Word64LE 0x1122334455667788
      roundtrips uint
      let buffer = LBS.toStrict $ Data.Binary.encode (uint, uint)
      res <- runExceptT $ parseUint64LE node contract buffer 8
      res `shouldBe` Right (fromIntegral uint, 16)

    it "parses votes" $ do
      let [i] = txnMessage
            & _solanaTxnMessage_instructions
            & fmap _solanaTxnInstruction_data
            & fmap unCompactByteArray
            & unCompactArray
            & toList
          vi = Data.Binary.decode i :: SolanaVoteInstruction
          SolanaVoteInstruction_Vote v = vi

          v' = v { _solanaVote_timestamp = Just 0x1122334455667788 }
          vi' = SolanaVoteInstruction_Vote v'

      let parse vote = do
            let buffer = LBS.toStrict $ Data.Binary.encode vote
            res <- runExceptT $ parseVote node contract buffer 0
            res `shouldBe` Right (vote, fromIntegral (BS.length buffer))

      roundtrips vi
      roundtrips vi'

      parse v
      parse v'

{-
    it "parses messages" $ do
      roundtrips txnMessage
      let buffer = LBS.toStrict $ Data.Binary.encode txnMessage
      res <- runExceptT $ parseSolanaMessage node contract buffer
      res `shouldBe` Right txnMessage
-}
