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
import Crypto.Hash (Digest, SHA256)
import Data.ByteArray.Sized (unSizedByteArray, unsafeSizedByteArray)
import Data.ByteString (ByteString)
import GHC.Exts (fromList)
import Data.Functor.Compose
import Data.Map (Map)
import Data.Solidity.Prim.Address (Address)
import Data.Word
import Network.Web3.Provider (runWeb3')
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Solidity.Prim.Bytes
import qualified Data.Solidity.Prim.Int
import qualified Network.Ethereum.Account as Eth
import qualified Network.Ethereum.Api.Types as Eth (TxReceipt(..))
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

-- getSlotLeader :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> Word64 -> m Base58ByteString
-- getSlotLeader node ca s = bytes32FromSol <$> simulate node ca "getSlotLeader" (Contracts.getSlotLeader $ word64ToSol s)


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

addBlocks :: (MonadIO m, MonadError String m) => Eth.Provider -> Address -> [(Word64, SolanaCommittedBlock)] -> Map Word64 SolanaLeaderSchedule -> SolanaEpochSchedule -> m ()
addBlocks node ca blocks leaderSchedule epochSchedule = void $ submit node ca "addBlocks" $ Contracts.addBlocks
  (word64ToSol . fst <$> blocks)
  (unsafeBytes32ToSol . _solanaCommittedBlock_blockhash . snd <$> blocks)
  (word64ToSol . _solanaCommittedBlock_parentSlot . snd <$> blocks)
  (unsafeBytes32ToSol . _solanaCommittedBlock_previousBlockhash . snd <$> blocks)
  (unsafeBytes32ToSol . (mergedSchedules Map.!) . fst <$> blocks)
  where
    mergedSchedules = ifoldMap (\(epoch, leaderPk) slotIndices ->
      foldMap (\slotIndex -> Map.singleton (firstSlotInEpoch epochSchedule epoch + slotIndex) leaderPk) slotIndices)
      $ Compose leaderSchedule

parseVoteMessage
  :: (MonadError String m, MonadIO m)
  => Eth.Provider
  -> Address
  -> Data.Solidity.Prim.Bytes.Bytes
  -> m ( Data.Solidity.Prim.Bytes.BytesN 8
       , Data.Solidity.Prim.Bytes.BytesN 8
       , Data.Solidity.Prim.Bytes.BytesN 8
       , [Data.Solidity.Prim.Bytes.BytesN 32]
       , Data.Solidity.Prim.Bytes.BytesN 32
       , [Data.Solidity.Prim.Bytes.Bytes]
       )
parseVoteMessage node ca msg = simulate node ca "parseVoteMessage" $ Contracts.parseVoteMessage msg

verifyTransactionSignature
  :: (MonadError String m, MonadIO m)
  => Eth.Provider
  -> Address
  -> Data.Solidity.Prim.Int.UIntN 64
  -> Data.Solidity.Prim.Int.UIntN 64
  -> Data.Solidity.Prim.Int.UIntN 64
  -> m Bool
verifyTransactionSignature node ca slot transactionIndex addressIndex =
  simulate node ca "verifyTransactionSignature"
  $ Contracts.verifyTransactionSignature slot transactionIndex addressIndex

challengeTransactionSignature
  :: (MonadError String m, MonadIO m)
  => Eth.Provider
  -> Address
  -> Data.Solidity.Prim.Int.UIntN 64
  -> Data.Solidity.Prim.Int.UIntN 64
  -> Data.Solidity.Prim.Int.UIntN 64
  -> m ()
challengeTransactionSignature node ca slot transactionIndex addressIndex =
  submit node ca "challengeTransactionSignature"
  $ Contracts.challengeTransactionSignature slot transactionIndex addressIndex

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

sha256ToBytes32 :: Digest SHA256 -> Data.Solidity.Prim.Bytes.BytesN 32
sha256ToBytes32 = unsafeSizedByteArray . ByteArray.convert

word64ToSol :: Word64 -> Data.Solidity.Prim.Int.UIntN 64
word64ToSol = fromInteger . toInteger

word64FromSol :: Data.Solidity.Prim.Int.UIntN 64 -> Word64
word64FromSol = fromInteger . toInteger

unsafeBytes32ToSol :: Base58ByteString -> Data.Solidity.Prim.Bytes.BytesN 32
unsafeBytes32ToSol = unsafeSizedByteArray . ByteArray.convert . unBase58ByteString

bytes32FromSol :: Data.Solidity.Prim.Bytes.BytesN 32 -> Base58ByteString
bytes32FromSol = Base58ByteString . ByteArray.convert . unSizedByteArray

bytesToSol :: BS.ByteString -> Data.Solidity.Prim.Bytes.Bytes
bytesToSol = ByteArray.convert

bytesFromSol :: Data.Solidity.Prim.Bytes.Bytes -> BS.ByteString
bytesFromSol = ByteArray.convert


invokeContract :: Address
               -> Eth.DefaultAccount Eth.Web3 a
               -> Eth.Web3 a
invokeContract a = Eth.withAccount ()
                   . Eth.withParam (Eth.to .~ a)
                   . Eth.withParam (Eth.gasLimit .~ 99e6) -- TODO: estimate gas before call instead of using default value of --rpc.gascap
                   . Eth.withParam (Eth.gasPrice .~ (1 :: Eth.Wei))

simulate :: (MonadIO m, MonadError String m, Show a) => Eth.Provider -> Address -> String -> Eth.DefaultAccount Eth.Web3 a -> m a
simulate node ca name x = do
  let qname = "'" <> name <> "'"
  runWeb3' node (invokeContract ca x) >>= \case
    Left err -> throwError $ "Failed " <> qname <> ": " <> show err
    Right r -> pure r

submit :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> String -> Eth.DefaultAccount Eth.Web3 Eth.TxReceipt -> m ()
submit node ca name x = do
  let qname = "'" <> name <> "'"
  runWeb3' node (invokeContract ca x) >>= \case
    Left err -> throwError $ "Failed " <> qname <> ": " <> show err
    Right r -> when (null $ Eth.receiptLogs r) $ throwError "Contract execution did not finish"
