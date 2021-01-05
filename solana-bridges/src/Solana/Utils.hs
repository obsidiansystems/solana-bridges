{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solana.Utils where

import Control.Lens (mapped, (&), (.~))
import Control.Monad.Except (MonadError, MonadIO, runExceptT)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Crypto.Random.Types
import Data.Binary
import Data.Bool (bool)
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import Data.Foldable
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Solidity.Prim.Address as Solidity
import Data.Tree (Tree (..))
import qualified Network.Ethereum.Api.Types as Eth
import Network.JsonRpc.TinyClient (JsonRpcException)
import qualified Network.Web3.Provider as Eth
import Test.Hspec (it, describe, hspec, shouldBe, shouldNotBe, shouldThrow)

import Ethereum.Contracts
import Solana.Relayer
import Solana.Types

testWithRunningNode :: Eth.Provider -> IO ()
testWithRunningNode node = do
  (contract, _slot) <- deploySolanaClientContract node defaultSolanaRPCConfig
  testInclusionProofVerification node contract

testInclusionProofVerification :: Eth.Provider -> Solidity.Address -> IO ()
testInclusionProofVerification node contract = hspec $ describe "16-ary merkle tree" $ do
  let
    verify expected proof blockMerkle value index = do
      let stubAccountHash = sha256 "qwerty"
          stubBankHash = sha256 $ ByteArray.convert stubAccountHash <> ByteArray.convert blockMerkle
      res <- runExceptT (verifyTransactionInclusionProof node contract stubAccountHash blockMerkle proof stubBankHash value index)
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

merkleParent :: [Digest SHA256] -> Digest SHA256
merkleParent = sha256 . BS.concat . fmap ByteArray.convert

verifyProof
  :: Eth.Provider
  -> Solidity.Address
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

mkSignature :: MonadRandom m => BS.ByteString -> m Ed25519.Signature
mkSignature msg = do
  sk <- Ed25519.generateSecretKey
  pure $ Ed25519.sign sk (Ed25519.toPublic sk) msg

getVoteCount :: (MonadError String m, MonadIO m) => Eth.Provider -> Solidity.Address -> Word64 -> m Word64
getVoteCount node contract slot = fmap _contractSlot_voteCounts $ getSlot node contract slot

getTransaction :: (MonadError String m, MonadIO m) => Eth.Provider -> Solidity.Address -> Word64 -> Word64 -> m (Maybe SolanaTxn)
getTransaction node contract slot txIdx = runMaybeT $ do
  sigs <- MaybeT $ getSignatures node contract slot txIdx
  msg <- MaybeT $ getMessage node contract slot txIdx
  pure $ Data.Binary.decode $ LBS.fromStrict $ sigs <> msg


-- curl getConfirmedBlock [30, "base64"]
txnBase64 :: BS.ByteString
txnBase64 = "AZVKAuMoJbnV4PtPG7mMZCBEanoBomhtUHls0FuOa1QgthOeVMVj6VeAUVkn++w9NW1VF2POQ4YaeOEY6T2L0gsBAAMF01L0Cxouih7U0W+rqrqFKJDpZV3XeSpuqaXgRbrAeAqgAVVnFM8Agxw18NmD9pMJiEV64Q4iw2HyJ+BVmNG3+Qan1RcZLwqvxvJl4/t3zHragsUp0L47E24tAFUgAAAABqfVFxjHdMkoVmOYaR1etoteuKObS21cc1VbIQAAAAAHYUgdNXR0u3xNdiTr072z2DVec9EQQ/wNo1OAAAAAAOXoKdOObGil/U5GuEw/MRcdyHgKqsLIJgZiCDePh+iGAQQEAQIDADUCAAAAAQAAAAAAAAAdAAAAAAAAAGjgBRtbPax5AAeZ2uXW052RZQ4g+VXwFZO6iUZSVsuGAA=="

txnBinary :: BS.ByteString
Right txnBinary = Base64.decode txnBase64

txnParsed :: SolanaTxn
txnParsed = Data.Binary.decode $ LBS.fromStrict txnBinary

roundtrip :: forall b. Binary b => b -> b
roundtrip = dec . enc
  where
    dec = Data.Binary.decode . LBS.fromStrict
    enc = LBS.toStrict . Data.Binary.encode

dummyHash :: Base58ByteString
dummyHash = Base58ByteString $ ByteArray.convert solanaVoteProgram

submitChildBlock :: (MonadError String m, MonadIO m) => Eth.Provider -> Solidity.Address -> [SolanaTxn] -> m Eth.TxReceipt
submitChildBlock node contract txs = do
  s <- getLastSlot node contract
  slot <- getSlot node contract s
  let block = SolanaCommittedBlock
        { _solanaCommittedBlock_blockhash = dummyHash
        , _solanaCommittedBlock_previousBlockhash = _contractSlot_blockHash slot
        , _solanaCommittedBlock_parentSlot = s
        , _solanaCommittedBlock_transactions = SolanaTxnWithMeta <$> txs
        , _solanaCommittedBlock_rewards = []
        , _solanaCommittedBlock_blockTime = Nothing
        }
  addBlocks node contract $ pure (s+1, block, dummyHash)

txnSigningInfo :: SolanaTxn -> (BS.ByteString, Seq.Seq (Ed25519.PublicKey, Ed25519.Signature))
txnSigningInfo txn = (LBS.toStrict $ Data.Binary.encode msg, Seq.zip pks sigs)
  where
    msg = _solanaTxn_message txn

    pks :: Seq.Seq Ed25519.PublicKey
    pks = unCompactArray $ _solanaTxnMessage_accountKeys msg

    sigs :: Seq.Seq Ed25519.Signature
    sigs = unCompactArray (_solanaTxn_signatures txn)

sha256 :: BS.ByteString -> Digest SHA256
sha256 = hash

verifySigs :: SolanaTxn -> Seq.Seq Bool
verifySigs txn = flip fmap ed25519 $ \(pk, sig) -> Ed25519.verify pk msg sig
  where
    (msg, ed25519) = txnSigningInfo txn

testSolanaClient :: IO ()
testSolanaClient = do
  let node = def

      [vi] = txnParsed
        & _solanaTxn_message
        & _solanaTxnMessage_instructions
        & fmap _solanaTxnInstruction_data
        & fmap unCompactByteArray
        & unCompactArray
        & toList
        & fmap Data.Binary.decode
      SolanaVoteInstruction_Vote v = vi

      [votedOnSlot] = v & _solanaVote_slots & toList
      initializeSlot = 1 + fromIntegral votedOnSlot
      relayStartingSlot = 1 + initializeSlot

      -- VoteSwitch's Hash is of a switching proof which is currently unused since optimistic confirmation proofs are not yet implemented
      madeUpHash = sha256 "0x1234567890"
      vsi = SolanaVoteInstruction_VoteSwitch v madeUpHash

      notAVote = SolanaVoteInstruction_UpdateValidatorIdentity

      txnTamperedMessageHeader = txnParsed
        & solanaTxn_message . solanaTxnMessage_header . solanaTxnHeader_numRequiredSignatures .~ 111

      txnTamperedVoteSwitch = txnParsed
        & solanaTxn_message . solanaTxnMessage_instructions . mapped . solanaTxnInstruction_data .~ CompactByteArray (Data.Binary.encode vsi)

      txnTamperedNoVotes = txnParsed
        & solanaTxn_message . solanaTxnMessage_instructions . mapped . solanaTxnInstruction_data .~ CompactByteArray (Data.Binary.encode notAVote)

      txnTamperedInvalidProgram = txnParsed
        & solanaTxn_message . solanaTxnMessage_instructions . mapped . solanaTxnInstruction_programIdIndex .~ 255

  contract <- deploySolanaClientContractImpl node
  putStrLn $ "Contract deployed at address " <> show contract

  hspec $ describe "Solana client" $ do
    let
      dummyEpochSchedule = SolanaEpochSchedule
        { _solanaEpochSchedule_warmup = False
        , _solanaEpochSchedule_firstNormalEpoch = 0
        , _solanaEpochSchedule_leaderScheduleSlotOffset = 0
        , _solanaEpochSchedule_firstNormalSlot = 0
        , _solanaEpochSchedule_slotsPerEpoch = 8096
        }
      dummyBlock slot txs = SolanaCommittedBlock
        { _solanaCommittedBlock_blockhash = dummyHash
        , _solanaCommittedBlock_previousBlockhash = dummyHash
        , _solanaCommittedBlock_parentSlot = slot - 1
        , _solanaCommittedBlock_transactions = SolanaTxnWithMeta <$> txs
        , _solanaCommittedBlock_rewards = []
        , _solanaCommittedBlock_blockTime = Nothing
        }

    it "can initialize contract" $ do
      res <- runExceptT $ initialize node contract initializeSlot (dummyBlock (initializeSlot - 1) []) dummyHash dummyEpochSchedule
      res `shouldBe` Right ()

    let roundtrips x = roundtrip x `shouldBe` x
        verify expected txn = do
          let (msg, ed25519) = txnSigningInfo txn
          let verifies (pk,sig) = do
                Ed25519.verify pk msg sig `shouldBe` expected
                res <- runExceptT $ test_ed25519_verify node contract (ByteArray.convert sig) msg (Base58ByteString $ ByteArray.convert pk)
                res `shouldBe` Right expected
          for_ ed25519 verifies

    it "has enough balance for challenge payout" $ do
      res <- runExceptT $ getBalance node contract
      res `shouldBe` Right challengePayout

    it "parses instructions" $ do
      for_ (txnParsed & _solanaTxn_message & _solanaTxnMessage_instructions) $ \i -> do
        roundtrips i
        let buffer = LBS.toStrict $ Data.Binary.encode i
        res <- runExceptT $ parseInstruction node contract buffer 0
        res `shouldBe` Right (i, fromIntegral (BS.length buffer))

    it "accepts valid sigs" $ do
      verify True txnParsed

    it "rejects invalid sigs" $ do
      verify False txnTamperedMessageHeader
      verify False txnTamperedVoteSwitch
      verify False txnTamperedNoVotes

    it "verifies vote transactions" $ do
      let sigs = LBS.toStrict $ Data.Binary.encode $ txnParsed & _solanaTxn_signatures
          msg = LBS.toStrict $ Data.Binary.encode $ txnParsed & _solanaTxn_message

          test expected s m i = do
            res <- runExceptT $ verifyVote node contract s m i
            res `shouldBe` Right expected

      test True sigs msg 0

    let
      submitChallenge slot tx instr = do
        res <- runExceptT $ challengeVote node contract slot tx instr
        fmap Eth.receiptStatus res `shouldBe` Right (Just 1)

      submitBlocks blocks = do
        res <- runExceptT $ addBlocks node contract blocks
        fmap Eth.receiptStatus res `shouldBe` Right (Just 1)

      expectTx slot txIdx expectedTx = do
        tx <- runExceptT $ getTransaction node contract slot txIdx
        tx `shouldBe` Right expectedTx

      expectContractAlive expectedAlive = do
        res <- runExceptT $ getCode node contract
        (bool shouldBe shouldNotBe expectedAlive) res (Right "")


    do
      let
        txsPerSlot :: Num a => a
        txsPerSlot = 10

        txCopies :: Word64
        txCopies = 200

        slots = txCopies `div` txsPerSlot --TODO: roundup when fractional
        tamperedVoteSlot = relayStartingSlot + slots
        tamperedNoVotesSlot = tamperedVoteSlot + 1

        copies = flip fmap [0..slots-1] $ \i ->
          (relayStartingSlot + i, replicate txsPerSlot txnParsed)

        voteTxs = copies <> [(tamperedVoteSlot, [txnTamperedVoteSwitch])]
        allTxs = voteTxs <> [(tamperedNoVotesSlot, [txnTamperedInvalidProgram, txnTamperedNoVotes])]

      it "can submit a chunk of slots with votes" $ do
        let
          blocks = fromMaybe (error "Blocks must be non-empty") $ nonEmpty
            $ flip fmap allTxs $ \(s, txs) -> (s, dummyBlock s txs, dummyHash)

        submitBlocks blocks

        for_ (_solanaVote_slots v) $ \s -> do
          res <- runExceptT $ getVoteCount node contract (fromIntegral s)
          res `shouldBe` Right (fromIntegral $ length copies * txsPerSlot + 1)

        expectTx relayStartingSlot        0                (Just txnParsed)
        expectTx relayStartingSlot        (txsPerSlot - 1) (Just txnParsed)
        expectTx (relayStartingSlot + 1)  0                (Just txnParsed)
        expectTx (relayStartingSlot + 2)  (txsPerSlot - 1) (Just txnParsed)
        expectTx tamperedNoVotesSlot      0                Nothing
        expectTx tamperedNoVotesSlot      1                (Just txnTamperedNoVotes)

      it "survives challenge of valid vote signatures" $ do
        submitChallenge relayStartingSlot 0 0
        submitChallenge relayStartingSlot 1 0
        submitChallenge (relayStartingSlot + 1) 0 0
        submitChallenge (relayStartingSlot + 1) 1 0

        expectTx relayStartingSlot 0 (Just txnParsed)

      it "survives challenge of non-relayed transactions" $ do
        runExceptT (challengeVote node contract tamperedNoVotesSlot 0 0) `shouldThrow` (\(_ :: JsonRpcException) -> True)
        expectContractAlive True

      it "self-destructs on challenge of invalid vote signatures" $ do
        submitChallenge tamperedNoVotesSlot 1 0
        expectContractAlive False
