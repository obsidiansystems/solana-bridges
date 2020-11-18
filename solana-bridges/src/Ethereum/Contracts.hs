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

import Control.Lens
import Control.Monad
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class
import Data.ByteArray.Sized (unSizedByteArray, unsafeSizedByteArray)
import Data.Functor.Compose
import Data.Map (Map)
import Data.Solidity.Prim.Address (Address)
import Data.Word
import Network.Web3.Provider (runWeb3')
import qualified Data.ByteArray as ByteArray
import qualified Data.Map.Strict as Map
import qualified Data.Solidity.Prim.Bytes
import qualified Data.Solidity.Prim.Int
import qualified Network.Ethereum.Account as Eth
import qualified Network.Ethereum.Api.Types as Eth (TxReceipt(..))
import qualified Network.Ethereum.Unit as Eth
import qualified Network.Web3.Provider as Eth
import qualified Data.ByteString as BS
import Data.ByteArray.HexString
import Data.Aeson
import Data.Text (Text)
import Data.Constraint

import Solana.Types
import qualified Ethereum.Contracts.Bindings as Contracts


-- data TestCrypto a b where
--   TestCrypto_scalarmult :: TestCrypto ((Integer, Integer), Integer) (Integer, Integer)
--   TestCrypto_decodepoint :: TestCrypto HexString (Integer, Integer)
--   -- TestCrypto_Hint :: () -> TestCrypto _
--   TestCrypto_isoncurve :: TestCrypto (Integer, Integer) Bool
--   -- TestCrypto_inv :: TestCrypto Integer Integer
--   TestCrypto_edwards :: TestCrypto ((Integer, Integer), (Integer, Integer)) (Integer, Integer)
--   -- TestCrypto_decodeint :: () -> TestCrypto _
--   -- TestCrypto_encodeint :: () -> TestCrypto _
--   TestCrypto_xrecover :: TestCrypto Integer Integer
--   TestCrypto_encodepoint :: TestCrypto (Integer, Integer) HexString
-- 
-- data TestCryptCase where
--   TestCryptCase :: forall a b. TestCrypto a b -> a -> b -> TestCryptCase
-- 
-- testCaseHasIn
--   :: forall c a b.
--   ( c ((Integer, Integer), (Integer, Integer))
--   , c ((Integer, Integer), Integer)
--   , c (Integer, Integer)
--   , c HexString
--   , c Integer
--   )
--   => TestCrypto a b
--   -> Dict (c a)
-- testCaseHasIn = \case
--   TestCrypto_scalarmult -> Dict
--   TestCrypto_decodepoint -> Dict
--   TestCrypto_isoncurve -> Dict
--   TestCrypto_edwards -> Dict
--   TestCrypto_xrecover -> Dict
--   TestCrypto_encodepoint -> Dict
-- 
-- testCaseHasOut
--   :: forall c a b.
--   ( c (Integer, Integer)
--   , c Bool
--   , c HexString
--   , c Integer
--   )
--   => TestCrypto a b
--   -> Dict (c b)
-- testCaseHasOut = \case
--   TestCrypto_scalarmult -> Dict
--   TestCrypto_decodepoint -> Dict
--   TestCrypto_isoncurve -> Dict
--   TestCrypto_edwards -> Dict
--   TestCrypto_xrecover -> Dict
--   TestCrypto_encodepoint -> Dict

-- newtype OneElementArray a = OneElementArray { unOneElementArray :: a }
-- instance FromJSON a => FromJSON (OneElementArray a) where
--   parseJSON v = do
--     xs :: [a] <- parseJSON v
--     case xs of
--       [x] -> pure $ OneElementArray x
--       _ -> fail "Wrong number of elements"
-- 
-- instance FromJSON TestCryptCase where
--   parseJSON = withObject "TestCryptCase" $ \v -> do
--     fn :: Text <- v .: "fn"
--     case fn of
--       "scalarmult"  -> TestCryptCase TestCrypto_scalarmult  <$> (                      v .: "args") <*> v .: "result"
--       "decodepoint" -> TestCryptCase TestCrypto_decodepoint <$> (unOneElementArray <$> v .: "args") <*> v .: "result"
--       "isoncurve"   -> TestCryptCase TestCrypto_isoncurve   <$> (unOneElementArray <$> v .: "args") <*> v .: "result"
--       "edwards"     -> TestCryptCase TestCrypto_edwards     <$> (                      v .: "args") <*> v .: "result"
--       "xrecover"    -> TestCryptCase TestCrypto_xrecover    <$> (unOneElementArray <$> v .: "args") <*> v .: "result"
--       "encodepoint" -> TestCryptCase TestCrypto_encodepoint <$> (unOneElementArray <$> v .: "args") <*> v .: "result"
--       _ -> fail "nah"
-- 
-- test_impl
--   :: (MonadError String m, MonadIO m)
--   => Eth.Provider -> Address
--   -> TestCrypto a b
--   -> a
--   -> m b
-- test_impl node ca = \case
--   TestCrypto_scalarmult -> \(p, e) -> test_scalarmult node ca p e
--   TestCrypto_decodepoint -> \(HexString x) -> test_decodepoint node ca (Base58ByteString x)
--   TestCrypto_isoncurve -> \p -> (== 0) <$> test_curvedistance node ca p
--   TestCrypto_edwards -> \(x, y) -> test_edwards node ca x y
--   TestCrypto_xrecover -> \y -> test_xrecover node ca y
--   TestCrypto_encodepoint -> \p -> HexString <$> test_encodepoint node ca p
--   -- TestCrypto_inv -> \a@() -> test_inv

-- test_encodepoint
--   :: (MonadError String m, MonadIO m)
--   => Eth.Provider -> Address
--   -> (Integer, Integer)
--   -> m BS.ByteString
-- test_encodepoint node ca p = ByteArray.convert <$> simulate node ca "encodepoint" (Contracts.test_encodepoint [fromInteger $ fst p, fromInteger $ snd p])

-- test_edwards
--   :: (MonadError String m, MonadIO m)
--   => Eth.Provider -> Address
--   -> (Integer, Integer)
--   -> (Integer, Integer)
--   -> m (Integer, Integer)
-- test_edwards node ca x y = (\[xx, yy] -> (toInteger xx, toInteger yy)) <$> simulate node ca "edwareds" (Contracts.test_edwards [fromInteger $ fst x, fromInteger $ snd x] [fromInteger $ fst y, fromInteger $ snd y])

-- test_curvedistance
--   :: (MonadError String m, MonadIO m)
--   => Eth.Provider -> Address
--   -> (Integer, Integer)
--   -> m Integer
-- test_curvedistance node ca (x, y) = toInteger <$> simulate node ca "test_curvedistance" (Contracts.test_curvedistance (fromInteger x) (fromInteger y))

-- test_scalarmult
--   :: (MonadError String m, MonadIO m)
--   => Eth.Provider -> Address
--   -> (Integer, Integer)
--   -> Integer
--   -> m (Integer, Integer)
-- test_scalarmult node ca (x, y) z = bimap toInteger toInteger <$> simulate node ca "test_scalarmult" (Contracts.scalarmult (fromInteger x, fromInteger y) (fromInteger z))

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


-- test_xrecover
--   :: (MonadError String m, MonadIO m)
--   => Eth.Provider -> Address
--   -> Integer -> m Integer
-- test_xrecover node ca x = toInteger <$> simulate node ca "test_xrecover" (Contracts.xrecover $ fromInteger x)

-- test_decodepoint
--   :: (MonadError String m, MonadIO m)
--   => Eth.Provider -> Address
--   -> Base58ByteString
--   -> m (Integer, Integer)
-- test_decodepoint node ca x = bimap toInteger toInteger <$> simulate node ca "test_decodepoint" (Contracts.decodepoint $ unsafeBytes32ToSol x)

test_decodeint
  :: (MonadError String m, MonadIO m)
  => Eth.Provider -> Address
  -> Base58ByteString
  -> m Integer
test_decodeint node ca x = toInteger <$> simulate node ca "test_decodeint" (Contracts.decodeint $ unsafeBytes32ToSol x)

test_packMessage
  :: (MonadError String m, MonadIO m)
  => Eth.Provider -> Address
  -> Base58ByteString -> Base58ByteString -> BS.ByteString
  -> m BS.ByteString
test_packMessage node ca x y z = bytesFromSol <$> simulate node ca "test_decodeint" (Contracts.packMessage (unsafeBytes32ToSol x) (unsafeBytes32ToSol y) (bytesToSol z))

-- test_sha512 :: (MonadError String m, MonadIO m) => Eth.Provider -> Address -> BS.ByteString -> m Eth.TxReceipt
-- test_sha512 node ca a = simulate node ca "test_sha512" (Contracts.test_sha512 (bytesToSol a))


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
