{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE EmptyCase #-}

module Ethereum.Contracts where

import Control.Lens hiding (index)
import Control.Monad
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class
import Crypto.Error (CryptoFailable(..))
import Crypto.Hash (Digest, SHA256, digestFromByteString)
import Data.ByteArray.HexString (HexString)
import Data.ByteArray.Sized (unSizedByteArray, unsafeSizedByteArray)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.Functor.Compose
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import Data.Solidity.Prim.Address (Address)
import Data.Word
import GHC.Exts (fromList)
import Network.Web3.Provider (runWeb3')
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.Binary as Binary
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Sequence
import qualified Data.Solidity.Prim.Bytes as Solidity
import qualified Data.Solidity.Prim.Int as Solidity
import qualified Network.Ethereum.Account as Eth
import qualified Network.Ethereum.Api.Eth as Eth (getBalance, getCode)
import qualified Network.Ethereum.Api.Types as Eth (TxReceipt(..), DefaultBlock(Latest))
import qualified Network.Ethereum.Unit as Eth
import qualified Network.Web3.Provider as Eth

import Solana.Types
import qualified Ethereum.Contracts.Bindings as Contracts

test_sha512 :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> BS.ByteString -> m BS.ByteString
test_sha512 node ca a = bytesFromSol <$> simulate node ca "test_sha512" (Contracts.test_sha512 (bytesToSol a))

test_sha512_gas :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> BS.ByteString -> m Eth.TxReceipt
test_sha512_gas node ca a = simulate node ca "test_sha512" (Eth.send $ Contracts.Test_sha512Data (bytesToSol a))

test_ed25519_verify
  :: (MonadError String m, MonadIO m)
  => Eth.Provider -> Address
  -> BS.ByteString -> BS.ByteString -> Base58ByteString
  -> m Bool
test_ed25519_verify node ca sig msg pk = simulate node ca "test_ed25519_verify" (Contracts.test_ed25519_verify (bytesToSol sig) (bytesToSol msg) (unsafeBytes32ToSol pk))

test_ed25519_verify_gas
  :: (MonadError String m, MonadIO m)
  => Eth.Provider -> Address
  -> BS.ByteString -> BS.ByteString -> Base58ByteString
  -> m Eth.TxReceipt
test_ed25519_verify_gas node ca sig msg pk = simulate node ca "" (Eth.send $ Contracts.Test_ed25519_verifyData (bytesToSol sig) (bytesToSol msg) (unsafeBytes32ToSol pk))

getInitialized :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> m Bool
getInitialized node ca = simulate node ca "initialized" Contracts.initialized

getLastSlot :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> m Word64
getLastSlot node ca = word64FromSol <$> simulate node ca "lastSlot" Contracts.lastSlot

getLastHash :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> m Base58ByteString
getLastHash node ca = Base58ByteString . ByteArray.convert . unSizedByteArray <$> simulate node ca "lastHash" Contracts.lastHash

getSignatures :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> Word64 -> Word64 -> m (Maybe ByteString)
getSignatures node ca slot tx = fmap convert $ simulate node ca "getSignatures"
  $ Contracts.getSignatures (fromIntegral slot) (fromIntegral tx)
  where
    convert (hasTx, signatures) = if hasTx then Just (ByteArray.convert signatures) else Nothing

getMessage :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> Word64 -> Word64 -> m (Maybe ByteString)
getMessage node ca slot tx = fmap convert $ simulate node ca "getMessage"
  $ Contracts.getMessage (fromIntegral slot) (fromIntegral tx)
  where
    convert (hasTx, message) = if hasTx then Just (ByteArray.convert message) else Nothing


data ContractSlot = ContractSlot
  { _contractSlot_hasBlock :: Bool
  , _contractSlot_blockHash :: Base58ByteString
  , _contractSlot_leaderPublicKey :: Base58ByteString
  , _contractSlot_voteCounts :: Word64
  } deriving (Eq, Ord, Show)

getSlot :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> Word64 -> m ContractSlot
getSlot node ca = fmap convert . simulate node ca "slots" . Contracts.getSlot_ . fromIntegral
  where
    convert (hasBlock, hash, leader, votes) =
      ContractSlot
      hasBlock
      (bytes32FromSol hash)
      (bytes32FromSol leader)
      (fromIntegral votes)

initialize :: (MonadIO m, MonadError String m)
  => Eth.Provider
  -> Address
  -> Word64
  -> SolanaCommittedBlock
  -> Base58ByteString
  -> SolanaEpochSchedule
  -> m ()
initialize node ca slot root leader schedule = void $ submit node ca "initialize" $ Contracts.initialize
  (word64ToSol slot)-- uint64 slot,
  (unsafeBytes32ToSol $ _solanaCommittedBlock_blockhash root)-- bytes32 blockHash,
  (unsafeBytes32ToSol $ leader)-- bytes32 leader,
  (_solanaEpochSchedule_warmup schedule)-- bool scheduleWarmup,
  (word64ToSol $ _solanaEpochSchedule_firstNormalEpoch schedule)-- uint64 scheduleFirstNormalEpoch,
  (word64ToSol $ _solanaEpochSchedule_leaderScheduleSlotOffset schedule)-- uint64 scheduleLeaderScheduleSlotOffset,
  (word64ToSol $ _solanaEpochSchedule_firstNormalSlot schedule)-- uint64 scheduleFirstNormalSlot,
  (word64ToSol $ _solanaEpochSchedule_slotsPerEpoch schedule)-- uint64 scheduleSlotsPerEpoch
  (blockTransactions root)

addBlocks
  :: (MonadIO m, MonadError String m)
  => Eth.Provider
  -> Address
  -> NonEmpty (Word64, SolanaCommittedBlock, Base58ByteString)
  -> m Eth.TxReceipt
addBlocks node ca blocks = submit node ca "addBlocks" $ Contracts.addBlocks
  ( word64ToSol $ _solanaCommittedBlock_parentSlot parent
  , unsafeBytes32ToSol $ _solanaCommittedBlock_previousBlockhash parent
  )
  (toList $ flip fmap blocks $ \(slot,block,leader) ->
      ( word64ToSol slot
      , unsafeBytes32ToSol $ _solanaCommittedBlock_blockhash block
      , unsafeBytes32ToSol leader
      )
  )
  (toList $ flip fmap blocks $ \(_, b, _) -> blockTransactions b)
  where
    (_, parent, _) = NonEmpty.head blocks

mergeSchedules :: Map Word64 SolanaLeaderSchedule -> SolanaEpochSchedule -> Map Word64 Base58ByteString
mergeSchedules leaderSchedule epochSchedule = flip ifoldMap (Compose leaderSchedule) $ \(epoch, leaderPk) slotIndices ->
  foldMap (\slotIndex -> Map.singleton (firstSlotInEpoch epochSchedule epoch + slotIndex) leaderPk) slotIndices

blockTransactions :: SolanaCommittedBlock -> [(Bool, Solidity.Bytes, Solidity.Bytes)]
blockTransactions b =
  flip fmap (fmap _solanaTxnWithMeta_transaction $ _solanaCommittedBlock_transactions b) $ \tx ->
    if isVoteTxn tx
    then
      ( True
      , ByteArray.convert $ LBS.toStrict $ Binary.encode $ tx & _solanaTxn_signatures
      , ByteArray.convert $ LBS.toStrict $ Binary.encode $ tx & _solanaTxn_message
      )
    else
      ( False
      , ""
      , ""
      )


challengeVote
  :: (MonadIO m, MonadError String m)
  => Eth.Provider
  -> Address
  -> Word64
  -> Word64
  -> Word64
  -> m Eth.TxReceipt
challengeVote node ca slot tx instruction = submit node ca "challengeVote"
  $ Contracts.challengeVote (fromIntegral slot) (fromIntegral tx) (fromIntegral instruction)

verifyVote_gas
  :: (MonadError String m, MonadIO m)
  => Eth.Provider
  -> Address
  -> ByteString
  -> ByteString
  -> Word64
  -> m Eth.TxReceipt
verifyVote_gas node ca sigs msg idx = simulate node ca "verifyVote"
  $ Eth.send $ Contracts.VerifyVoteData (ByteArray.convert sigs) (ByteArray.convert msg) (fromIntegral idx)

verifyVote
  :: (MonadError String m, MonadIO m)
  => Eth.Provider
  -> Address
  -> ByteString
  -> ByteString
  -> Word64
  -> m Bool
verifyVote node ca sigs msg idx = simulate node ca "verifyVote"
  $ Contracts.verifyVote (ByteArray.convert sigs) (ByteArray.convert msg) (fromIntegral idx)

-- TODO: misbehaving bindings - parsing is fixed in https://github.com/obsidiansystems/hs-web3/tree/tupple-array but encoding seems broken
parseSolanaMessage
  :: (MonadError String m, MonadIO m)
  => Eth.Provider
  -> Address
  -> ByteString
  -> m SolanaTxnMessage
parseSolanaMessage node ca msg = fmap convert $ simulate node ca "parseSolanaMessage_" $ Contracts.parseSolanaMessage_ (ByteArray.convert msg)
  where
--    bytes = CompactByteArray . LBS.fromStrict . ByteArray.convert
    convert (requiredSignatures, readOnlySignatures, readOnlyUnsigned, addresses, recentBlockHash) --, _instructions)
      = SolanaTxnMessage
        (SolanaTxnHeader (fromIntegral requiredSignatures)  (fromIntegral readOnlySignatures) (fromIntegral readOnlyUnsigned))
        (LengthPrefixedArray . Sequence.fromList . fmap unsafeBytes32ToPublicKey $ addresses)
        (bytes32ToSha256 recentBlockHash)
        (LengthPrefixedArray $ Sequence.fromList [])
{-
        (LengthPrefixedArray $ Sequence.fromList $ flip fmap instructions $
         \(programId, accounts, data') -> SolanaTxnInstruction
                                          (fromIntegral programId)
                                          (bytes accounts)
                                          (bytes data'))
-}

parseInstruction
  :: (MonadError String m, MonadIO m)
  => Eth.Provider
  -> Address
  -> ByteString
  -> Word64
  -> m (SolanaTxnInstruction, Word64)
parseInstruction node ca buffer offset = fmap convert $ simulate node ca "parseInstruction_"
  $ Contracts.parseInstruction_ (ByteArray.convert buffer) (fromIntegral offset)
  where
    bytes = CompactByteArray . LBS.fromStrict . ByteArray.convert
    convert (programId, accounts, data', consumed) =
      (SolanaTxnInstruction (fromIntegral programId) (bytes accounts) (bytes data')
      , fromIntegral consumed)

getSeenBlocks :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> m Word64
getSeenBlocks node ca = word64FromSol <$> simulate node ca "seenBlocks" Contracts.seenBlocks

verifyTransactionInclusionProof
  :: (MonadError String m, MonadIO m)
  => Eth.Provider
  -> Address
  -> Digest SHA256
  -> Digest SHA256
  -> [[Digest SHA256]]
  -> Digest SHA256
  -> ByteString
  -> Word64
  -> m Bool
verifyTransactionInclusionProof node ca accountsHash blockMerkle subProof bankHashMerkleRoot value transactionIndex =
  simulate node ca "verifyTransactionInclusionProof" $ Contracts.verifyTransactionInclusionProof
    (sha256ToBytes32 accountsHash)
    (sha256ToBytes32 blockMerkle)
    (fmap (fromList . fmap (unsafeSizedByteArray . ByteArray.convert . sha256ToBytes32)) subProof)
    (sha256ToBytes32 bankHashMerkleRoot)
    (ByteArray.convert value)
    (word64ToSol transactionIndex)

verifyMerkleProof :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> [[Digest SHA256]] -> Digest SHA256 -> ByteString -> Word64 -> m Bool
verifyMerkleProof node ca proof root value index = simulate node ca "verifyMerkleProof" $
  Contracts.verifyMerkleProof
    (fmap (fromList . fmap (unsafeSizedByteArray . ByteArray.convert . sha256ToBytes32)) proof)
    (sha256ToBytes32 root)
    (ByteArray.convert value)
    (word64ToSol index)

-- implementation details

sha256ToBytes32 :: Digest SHA256 -> Solidity.BytesN 32
sha256ToBytes32 = unsafeSizedByteArray . ByteArray.convert

bytes32ToSha256 :: Solidity.BytesN 32 -> Digest SHA256
bytes32ToSha256 = maybe (error "bytes32ToSha256: digestFromByteString failed") id . digestFromByteString . unSizedByteArray

word64ToSol :: Word64 -> Solidity.UIntN 64
word64ToSol = fromInteger . toInteger

word64FromSol :: Solidity.UIntN 64 -> Word64
word64FromSol = fromInteger . toInteger

unsafeBytes32ToSol :: Base58ByteString -> Solidity.BytesN 32
unsafeBytes32ToSol = unsafeSizedByteArray . ByteArray.convert . unBase58ByteString

bytes32FromSol :: Solidity.BytesN 32 -> Base58ByteString
bytes32FromSol = Base58ByteString . ByteArray.convert . unSizedByteArray

bytesToSol :: BS.ByteString -> Solidity.Bytes
bytesToSol = ByteArray.convert

bytesFromSol :: Solidity.Bytes -> BS.ByteString
bytesFromSol = ByteArray.convert

unsafeBytes32ToPublicKey :: Solidity.BytesN 32 -> Ed25519.PublicKey
unsafeBytes32ToPublicKey bytes32 = case Ed25519.publicKey (ByteArray.convert bytes32 :: ByteString) of
  CryptoPassed good -> good
  CryptoFailed bad -> error $ "unsafeBytes32ToPublicKey: publicKey failed: " <> show bad


invokeContract :: Address
               -> Eth.DefaultAccount Eth.Web3 a
               -> Eth.Web3 a
invokeContract a = Eth.withAccount ()
                   . Eth.withParam (Eth.to .~ a)
                   . Eth.withParam (Eth.gasPrice .~ (1 :: Eth.Wei))

simulate :: (MonadIO m, MonadError String m, Show a) => Eth.Provider -> Address -> String -> Eth.DefaultAccount Eth.Web3 a -> m a
simulate node ca name x = do
  let qname = "'" <> name <> "'"
  runWeb3' node (invokeContract ca x) >>= \case
    Left err -> throwError $ "Failed " <> qname <> ": " <> show err
    Right r -> pure r

submit :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> String -> Eth.DefaultAccount Eth.Web3 Eth.TxReceipt -> m Eth.TxReceipt
submit node ca name x = do
  let qname = "'" <> name <> "'"
  runWeb3' node (invokeContract ca x) >>= \case
    Left err -> throwError $ "Failed " <> qname <> ": " <> show err
    Right r -> do
      when (Just 1 /= Eth.receiptStatus r) $ throwError "Contract execution reported failure"
      pure r

getCode :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> m HexString
getCode node ca = do
  runWeb3' node (Eth.getCode ca Eth.Latest) >>= \case
    Left err -> throwError $ "Failed getCode@" <> show ca <> ": " <> show err
    Right r -> pure r

getBalance :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> m Integer
getBalance node ca = do
  runWeb3' node (Eth.getBalance ca Eth.Latest) >>= \case
    Left err -> throwError $ "Failed getBalance@" <> show ca <> ": " <> show err
    Right r -> pure (fromIntegral r)
