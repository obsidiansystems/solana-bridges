{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Ethereum.Contracts where

import Control.Lens
import Control.Monad
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class
import Data.ByteArray.Sized (unSizedByteArray, unsafeSizedByteArray)
import Data.Solidity.Prim.Address (Address)
import Data.Word
import Network.Web3.Provider (runWeb3')
import qualified Data.ByteArray as ByteArray
import qualified Data.Solidity.Prim.Bytes
import qualified Data.Solidity.Prim.Int
import qualified Network.Ethereum.Account as Eth
import qualified Network.Ethereum.Api.Types as Eth (TxReceipt(..))
import qualified Network.Ethereum.Unit as Eth
import qualified Network.Web3.Provider as Eth

import Solana.Types
import qualified Ethereum.Contracts.Abi as Contracts


getInitialized :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> m Bool
getInitialized node ca = simulate node ca "initialized" Contracts.initialized

getEpoch :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> m Word64
getEpoch node ca = word64FromSol <$> simulate node ca "epoch" Contracts.epoch

getLastSlot :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> m Word64
getLastSlot node ca = word64FromSol <$> simulate node ca "lastSlot" Contracts.lastSlot


getLastHash :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> m Base58ByteString
getLastHash node ca = Base58ByteString . ByteArray.convert . unSizedByteArray <$> simulate node ca "lastHash" Contracts.lastHash

getSlotLeader :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> Word64 -> m (Data.Solidity.Prim.Int.UIntN 256)
getSlotLeader node ca s = simulate node ca "getSlotLeader" $ Contracts.getSlotLeader $ word64ToSol s

setEpoch :: (MonadIO m, MonadError String m) => Eth.Provider -> Address -> Word64 -> SolanaLeaderSchedule -> m ()
setEpoch node ca e _ldrSched = void $ submit node ca "setEpoch" $ Contracts.setEpoch (word64ToSol e) [] []
  -- todo: leader schedule handling

addBlocks :: (MonadIO m, MonadError String m) => Eth.Provider -> Address -> [(Word64, SolanaCommittedBlock)] -> m ()
addBlocks node ca blocks = void $ submit node ca "addBlocks" $ Contracts.addBlocks
  (word64ToSol . fst <$> blocks)
  (unsafeBytes32ToSol . _solanaCommittedBlock_blockhash . snd <$> blocks)
  (word64ToSol . _solanaCommittedBlock_parentSlot . snd <$> blocks)
  (unsafeBytes32ToSol . _solanaCommittedBlock_previousBlockhash . snd <$> blocks)


getSeenBlocks :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> m Word64
getSeenBlocks node ca = word64FromSol <$> simulate node ca "seenBlocks" Contracts.seenBlocks

-- implementation details

word64ToSol :: Word64 -> Data.Solidity.Prim.Int.UIntN 64
word64ToSol = fromInteger . toInteger

word64FromSol :: Data.Solidity.Prim.Int.UIntN 64 -> Word64
word64FromSol = fromInteger . toInteger

unsafeBytes32ToSol :: Base58ByteString -> Data.Solidity.Prim.Bytes.BytesN 32
unsafeBytes32ToSol = unsafeSizedByteArray . ByteArray.convert . unBase58ByteString

bytes32FromSol :: Data.Solidity.Prim.Bytes.BytesN 32 -> Base58ByteString
bytes32FromSol = Base58ByteString . ByteArray.convert . unSizedByteArray

invokeContract :: Address
               -> Eth.DefaultAccount Eth.Web3 a
               -> Eth.Web3 a
invokeContract a = Eth.withAccount ()
                   . Eth.withParam (Eth.to .~ a)
                   . Eth.withParam (Eth.gasLimit .~ 1e8)
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
