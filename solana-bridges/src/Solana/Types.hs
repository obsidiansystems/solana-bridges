{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Solana.Types where

import Control.Applicative
import Control.Lens (makeLenses, (<&>))
import Control.Monad ((<=<))
import Crypto.Error (CryptoFailable(..))
import Crypto.Hash (Digest, HashAlgorithm, hashDigestSize, digestFromByteString)
import Crypto.Hash.Algorithms (SHA256)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (toJSONKeyText)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Foldable
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base58 as Base58
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data JsonRpcVersion = JsonRpcVersion
instance ToJSON JsonRpcVersion where toJSON _ = toJSON ("2.0" :: T.Text)
instance FromJSON JsonRpcVersion where parseJSON _ = pure JsonRpcVersion

type SolanaRpcRequest = SolanaRpcRequestF Value
data SolanaRpcRequestF a = SolanaRpcRequest
  { _solanaRpcRequest_jsonrpc :: !JsonRpcVersion
  , _solanaRpcRequest_id :: !Int
  , _solanaRpcRequest_method :: !T.Text
  , _solanaRpcRequest_params :: !(Maybe a)
  }

type SolanaRpcResult = SolanaRpcResultF Value
data SolanaRpcResultF a = SolanaRpcResult
  { _solanaRpcResult_jsonrpc :: !JsonRpcVersion
  , _solanaRpcResult_id :: !Int
  , _solanaRpcResult_result :: !a
  }

data SolanaRpcError = SolanaRpcError
  { _solanaRpcError_jsonrpc :: !JsonRpcVersion
  , _solanaRpcError_id :: !Int
  , _solanaRpcError_error :: !Value
  }

type SolanaRpcNotification = SolanaRpcNotificationF Value
data SolanaRpcNotificationF a = SolanaRpcNotification
  { _solanaRpcNotification_jsonrpc :: !JsonRpcVersion
  , _solanaRpcNotification_method :: !T.Text
  , _solanaRpcNotification_params :: !(SolanaRpcNotificationParamsF a)
  }

type SolanaRpcNotificationParams = SolanaRpcNotificationParamsF Value
data SolanaRpcNotificationParamsF a = SolanaRpcNotificationParams
  { _solanaRpcNotificationParams_result :: !a
  , _solanaRpcNotificationParams_subscription :: !Int
  }

newtype Base58ByteString = Base58ByteString { unBase58ByteString :: BS.ByteString }
  deriving (Eq, Ord, Show)

data SolanaEpochInfo = SolanaEpochInfo
  { _solanaEpochInfo_slotsInEpoch :: !Word64
  , _solanaEpochInfo_slotIndex :: !Word64
  , _solanaEpochInfo_absoluteSlot :: !Word64
  , _solanaEpochInfo_blockHeight :: !Word64
  , _solanaEpochInfo_epoch :: !Word64
  } deriving Show

data SolanaEpochSchedule = SolanaEpochSchedule
  { _solanaEpochSchedule_warmup :: !Bool
  , _solanaEpochSchedule_firstNormalEpoch :: !Word64
  , _solanaEpochSchedule_leaderScheduleSlotOffset :: !Word64
  , _solanaEpochSchedule_firstNormalSlot :: !Word64
  , _solanaEpochSchedule_slotsPerEpoch :: !Word64
  } deriving Show


--- Wellknown addresses
wellKnownAddress :: BS.ByteString -> Ed25519.PublicKey
wellKnownAddress x = case Base58.decodeBase58 Base58.bitcoinAlphabet x of
  Nothing -> error "b58"
  Just y -> case Ed25519.publicKey y of
      CryptoPassed good -> good
      CryptoFailed bad -> error $ show bad

wellKnownPrograms :: [BS.ByteString]
wellKnownPrograms = [ "11111111111111111111111111111111"
                    , "BPFLoader1111111111111111111111111111111111"
                    , "Config1111111111111111111111111111111111111"
                    , "Stake11111111111111111111111111111111111111"
                    , "KeccakSecp256k11111111111111111111111111111"
                    , "Vote111111111111111111111111111111111111111"
                    ]

solanaVoteProgram :: Ed25519.PublicKey
solanaVoteProgram = wellKnownAddress "Vote111111111111111111111111111111111111111"

findWellKnownProgram :: Ed25519.PublicKey -> Maybe BS.ByteString
findWellKnownProgram pk = find (\p -> wellKnownAddress p == pk) wellKnownPrograms

newtype CompactWord16 = CompactWord16 Word16
  deriving (Real, Integral, Num, Show, Eq, Ord, Enum)

instance Binary CompactWord16 where
  put (CompactWord16 x)
    | x < 1 `shiftL` 7  = putWord8 (fromIntegral x         )
    | x < 1 `shiftL` 14 = putWord8 (fromIntegral x .|. 0x80) <> putWord8 (fromIntegral (x `shiftR` 7)         )
    | otherwise         = putWord8 (fromIntegral x .|. 0x80) <> putWord8 (fromIntegral (x `shiftR` 7) .|. 0x80) <> putWord8 (fromIntegral (x `shiftR` 14))

  get = getWord8 >>= \x0 -> CompactWord16 <$> if x0 < 1 `shiftL` 7
    then pure $ fromIntegral x0
    else getWord8 >>= \x1 -> if x1 < 1 `shiftL` 7
      then pure $ fromIntegral (x0 .&. 0x7f) .|. shiftL (fromIntegral x1) 7
      else getWord8 >>= \x2 -> if x2 < 1 `shiftL` 2
        then pure $ fromIntegral (x0 .&. 0x7f) .|. shiftL (fromIntegral (x1 .&. 0x7f)) 7 .|. shiftL (fromIntegral x2) 14
        else fail "too big"

newtype LengthPrefixedArray sz a = LengthPrefixedArray { unCompactArray :: Seq a}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (Integral sz, Num sz, Binary sz, Binary a) => Binary (LengthPrefixedArray sz a) where
  put (LengthPrefixedArray xs)
    | length xs /= fromIntegral (fromIntegral (length xs) :: sz) = error "bad LengthPrefixedArray length"
    | otherwise = put (fromIntegral (length xs) :: sz) <> traverse_ put xs

  get = do
    numXs :: sz <- get
    LengthPrefixedArray <$> Seq.replicateM (fromIntegral numXs) get

type CompactArray = LengthPrefixedArray CompactWord16

newtype CompactByteArray = CompactByteArray { unCompactByteArray :: LBS.ByteString }
  deriving (Eq, Ord, Show)

instance Binary CompactByteArray where
  put (CompactByteArray xs)
    | LBS.length xs /= fromIntegral (fromIntegral (LBS.length xs) :: Word16) = error "bad CompactByteArray length"
    | otherwise = put (CompactWord16 $ fromIntegral $ LBS.length xs) <> putByteString (LBS.toStrict xs)

  get = do
    CompactWord16 numXs <- get
    CompactByteArray <$> getLazyByteString (fromIntegral numXs)


instance ToJSON a => ToJSON (LengthPrefixedArray sz a) where
  toJSON = toJSON . unCompactArray
instance FromJSON a => FromJSON (LengthPrefixedArray sz a) where
  parseJSON = fmap LengthPrefixedArray . parseJSON

instance Binary Ed25519.Signature where
  put = putByteString . ByteArray.convert
  get = mkEd25519Signature =<< getByteString (Ed25519.signatureSize)
instance Binary Ed25519.PublicKey where
  put = putByteString . ByteArray.convert
  get = mkEd25519PublicKey =<< getByteString (Ed25519.publicKeySize)
instance HashAlgorithm a => Binary (Digest a) where
  put = putByteString . ByteArray.convert
  get = mkDigest =<< getByteString (hashDigestSize (undefined :: a))

instance ToJSON Ed25519.Signature where
  toJSON = toJSON . Base58ByteString . ByteArray.convert
instance ToJSON Ed25519.PublicKey where
  toJSON = toJSON . Base58ByteString . ByteArray.convert
instance ToJSON (Digest SHA256) where
  toJSON = toJSON . Base58ByteString . ByteArray.convert

instance FromJSON Ed25519.Signature where
  parseJSON = mkEd25519Signature . unBase58ByteString <=< parseJSON
instance FromJSON Ed25519.PublicKey where
  parseJSON = mkEd25519PublicKey . unBase58ByteString <=< parseJSON
instance FromJSON (Digest SHA256) where
  parseJSON = mkDigest . unBase58ByteString <=< parseJSON


mkEd25519PublicKey :: MonadFail m => BS.ByteString -> m Ed25519.PublicKey
mkEd25519PublicKey bs = case Ed25519.publicKey bs of
  CryptoPassed good -> pure good
  CryptoFailed bad -> fail $ show bad

mkEd25519Signature :: MonadFail m => BS.ByteString -> m Ed25519.Signature
mkEd25519Signature bs = case Ed25519.signature bs of
  CryptoPassed good -> pure good
  CryptoFailed bad -> fail $ show bad

mkDigest :: (MonadFail m, HashAlgorithm ha) => BS.ByteString -> m (Digest ha)
mkDigest bs = case digestFromByteString bs of
  Just good -> pure good
  Nothing -> fail "bad Digest size"

-- https://docs.solana.com/transaction#transaction-format
data SolanaTxn = SolanaTxn
  { _solanaTxn_signatures :: CompactArray Ed25519.Signature
  , _solanaTxn_message :: SolanaTxnMessage
  } deriving (Eq, Generic, Show)
instance Binary SolanaTxn

data SolanaTxnMessage = SolanaTxnMessage
  { _solanaTxnMessage_header :: SolanaTxnHeader
  , _solanaTxnMessage_accountKeys :: CompactArray Ed25519.PublicKey
  , _solanaTxnMessage_recentBlockhash :: Digest SHA256
  , _solanaTxnMessage_instructions :: CompactArray SolanaTxnInstruction
  } deriving (Eq, Generic, Show)
instance Binary SolanaTxnMessage

data SolanaTxnHeader = SolanaTxnHeader
  { _solanaTxnHeader_numRequiredSignatures :: Word8
  , _solanaTxnHeader_numReadonlySignedAccounts :: Word8
  , _solanaTxnHeader_numReadonlyUnsignedAccounts :: Word8
  } deriving (Eq, Ord, Generic, Show)
instance Binary SolanaTxnHeader

-- https://github.com/solana-labs/solana/blob/master/sdk/program/src/instruction.rs#L227
data SolanaTxnInstruction = SolanaTxnInstruction
  { _solanaTxnInstruction_programIdIndex :: Word8
  , _solanaTxnInstruction_accounts :: CompactByteArray
  , _solanaTxnInstruction_data :: CompactByteArray
  } deriving (Eq, Generic, Show)
instance Binary SolanaTxnInstruction

instance ToJSON SolanaTxnInstruction where
  toJSON tx = object
    [ "programIdIndex" .= _solanaTxnInstruction_programIdIndex tx
    , "accounts" .= LBS.unpack (unCompactByteArray $ _solanaTxnInstruction_accounts tx)
    , "data" .= base58ByteStringToText (Base58ByteString $ LBS.toStrict $ unCompactByteArray $ _solanaTxnInstruction_data tx)
    ]

instance FromJSON SolanaTxnInstruction where
  parseJSON = withObject "SolanaTxnInstruction" $ \v -> SolanaTxnInstruction
    <$> v .: "programIdIndex"
    <*> (v .: "accounts" <&> (CompactByteArray . LBS.pack))
    <*> ((v .: "data") >>= parseData)
    where
      parseData txt = case parseBase58ByteString txt of
        Nothing -> fail "invalid base58"
        Just bs -> pure $ CompactByteArray $ LBS.fromStrict $ unBase58ByteString bs

isVoteTxn :: SolanaTxn -> Bool
isVoteTxn txn = any isVoteInstr (_solanaTxnMessage_instructions $ _solanaTxn_message txn)
  where
    isVoteInstr :: SolanaTxnInstruction -> Bool
    isVoteInstr instr
      = Just solanaVoteProgram == Seq.lookup (fromIntegral $ _solanaTxnInstruction_programIdIndex instr) (unCompactArray $ _solanaTxnMessage_accountKeys $ _solanaTxn_message txn)


data VoteAuthorize
   = VoteAuthorize_Voter
   | VoteAuthorize_Withdrawer
   deriving (Generic, Eq, Show)
instance Binary VoteAuthorize

newtype Word64LE = Word64LE { unWord64LE :: Word64 }
  deriving (Integral, Num, Eq, Ord, Real, Enum, Bounded, Show)

instance Binary Word64LE where
  get = Word64LE <$> getWord64le
  put = putWord64le . unWord64LE


data SolanaVote = SolanaVote
  { _solanaVote_slots :: LengthPrefixedArray Word64LE Word64LE -- ^ A stack of votes starting with the oldest vote
  , _solanaVote_hash :: Digest SHA256-- ^  signature of the bank's state at the last slot
  , _solanaVote_timestamp :: Maybe Word64LE -- ^  processing timestamp of last slot
  } deriving (Generic, Eq, Show)
instance Binary SolanaVote

data SolanaVoteInitialize = SolanaVoteInitialize
  { _solanaVoteInitialize_nodePubkey :: Ed25519.PublicKey
  , _solanaVoteInitialize_authorizedVoter :: Ed25519.PublicKey
  , _solanaVoteInitialize_authorizedWithdrawer :: Ed25519.PublicKey
  , _solanaVoteInitialize_commission :: Word8
  } deriving (Generic, Eq, Show)
instance Binary SolanaVoteInitialize

data SolanaVoteInstruction
  = SolanaVoteInstruction_InitializeAccount SolanaVoteInitialize
  | SolanaVoteInstruction_Authorize Ed25519.PublicKey VoteAuthorize
  | SolanaVoteInstruction_Vote SolanaVote
  | SolanaVoteInstruction_Withdraw Word64LE
  | SolanaVoteInstruction_UpdateValidatorIdentity
  | SolanaVoteInstruction_UpdateCommission Word8
  | SolanaVoteInstruction_VoteSwitch SolanaVote (Digest SHA256)
  deriving (Eq, Show)

instance Binary SolanaVoteInstruction where
  get = getWord32le >>= \case
    0 -> SolanaVoteInstruction_InitializeAccount <$> get
    1 -> SolanaVoteInstruction_Authorize <$> get <*> get
    2 -> SolanaVoteInstruction_Vote <$> get
    3 -> SolanaVoteInstruction_Withdraw <$> get
    4 -> pure SolanaVoteInstruction_UpdateValidatorIdentity
    5 -> SolanaVoteInstruction_UpdateCommission <$> get
    6 -> SolanaVoteInstruction_VoteSwitch <$> get <*> get
    bad -> fail $ "bad tag for SolanaVoteInstruction: " <> show bad
  put = \case
    SolanaVoteInstruction_InitializeAccount x     -> putWord32le 0 <> put x
    SolanaVoteInstruction_Authorize x y           -> putWord32le 1 <> put x <> put y
    SolanaVoteInstruction_Vote x                  -> putWord32le 2 <> put x
    SolanaVoteInstruction_Withdraw x              -> putWord32le 3 <> put x
    SolanaVoteInstruction_UpdateValidatorIdentity -> putWord32le 4
    SolanaVoteInstruction_UpdateCommission x      -> putWord32le 5 <> put x
    SolanaVoteInstruction_VoteSwitch x y          -> putWord32le 6 <> put x <> put y


firstSlotInEpoch :: SolanaEpochSchedule -> Word64 -> Word64
firstSlotInEpoch schedule =
  let
    warmup0 = (\x -> div (_solanaEpochSchedule_slotsPerEpoch schedule) $ 2 ^ x)
      <$> reverse [1.._solanaEpochSchedule_firstNormalEpoch schedule]
    warmup = Map.fromList $ zip [0..] $ scanl (+) 0 warmup0
  in \epoch -> case Map.lookup epoch warmup of
    Nothing -> _solanaEpochSchedule_firstNormalSlot schedule + (satsub epoch $ _solanaEpochSchedule_firstNormalEpoch schedule) * _solanaEpochSchedule_slotsPerEpoch schedule
    Just slot -> slot


epochFromSlot :: SolanaEpochSchedule -> Word64 -> SolanaEpochInfo
epochFromSlot schedule =
  let
    warmup0 = (\x -> div (_solanaEpochSchedule_slotsPerEpoch schedule) $ 2 ^ x)
      <$> reverse [1.._solanaEpochSchedule_firstNormalEpoch schedule]
    warmup = zip [0..] $ zip warmup0 $ drop 1 $ scanl (+) 0 warmup0
  in \absoluteSlot ->
    let
      (epoch, (slotsInEpoch, firstSlotInEpoch0)) = if absoluteSlot >= _solanaEpochSchedule_firstNormalSlot schedule
        then
          (_solanaEpochSchedule_firstNormalEpoch schedule + (absoluteSlot - _solanaEpochSchedule_firstNormalSlot schedule) `div` _solanaEpochSchedule_slotsPerEpoch schedule
          , ( _solanaEpochSchedule_slotsPerEpoch schedule
            , _solanaEpochSchedule_firstNormalSlot schedule + (epoch - _solanaEpochSchedule_firstNormalEpoch schedule) * _solanaEpochSchedule_slotsPerEpoch schedule
            )
          )
        else
          case reverse $ filter (\(_, (_, firstSlotInEpoch')) -> firstSlotInEpoch' <= absoluteSlot) warmup of
            [] -> error "Network too recent: epoch info is not available yet"
            (x:_) -> x
    in SolanaEpochInfo
      { _solanaEpochInfo_slotsInEpoch = slotsInEpoch
      , _solanaEpochInfo_slotIndex = absoluteSlot - firstSlotInEpoch0
      , _solanaEpochInfo_absoluteSlot = absoluteSlot
      , _solanaEpochInfo_blockHeight = 0 -- or maybe this should be undefined?
      , _solanaEpochInfo_epoch = epoch
      }


parseBase58ByteString :: Text -> Maybe Base58ByteString
parseBase58ByteString pk58Text = Base58ByteString <$> Base58.decodeBase58 Base58.bitcoinAlphabet (T.encodeUtf8 pk58Text)
base58ByteStringToText :: Base58ByteString -> Text
base58ByteStringToText = T.decodeLatin1 . Base58.encodeBase58 Base58.bitcoinAlphabet . unBase58ByteString

instance FromJSON Base58ByteString where
  parseJSON = withText "Base58ByteString" $ maybe (fail "invalid base58") pure . parseBase58ByteString
instance ToJSON Base58ByteString where
  toJSON = toJSON . base58ByteStringToText

instance ToJSONKey Base58ByteString where
  toJSONKey = toJSONKeyText base58ByteStringToText
instance FromJSONKey Base58ByteString where
  fromJSONKey = FromJSONKeyTextParser (maybe (fail "invalid base58") pure . parseBase58ByteString)

data SolanaCommitment
   = SolanaCommitment_Max
   | SolanaCommitment_Root
   | SolanaCommitment_SingleGossip
   | SolanaCommitment_Recent
   deriving (Eq, Ord, Show, Enum)

instance ToJSON SolanaCommitment where
  toJSON x = object ["commitment" Data.Aeson..= case x of
      SolanaCommitment_Max -> "max" :: Text
      SolanaCommitment_Root -> "root"
      SolanaCommitment_SingleGossip -> "singleGossip"
      SolanaCommitment_Recent -> "recent"
    ]

data SolanaCommittedBlock = SolanaCommittedBlock
  { _solanaCommittedBlock_blockhash :: !Base58ByteString
  , _solanaCommittedBlock_previousBlockhash :: !Base58ByteString
  , _solanaCommittedBlock_parentSlot :: !Word64
  , _solanaCommittedBlock_transactions :: ![SolanaTxnWithMeta]
  , _solanaCommittedBlock_rewards :: ![Value]
  , _solanaCommittedBlock_blockTime :: !(Maybe Word64)
  } deriving Show

data SolanaTxnWithMeta = SolanaTxnWithMeta
  { _solanaTxnWithMeta_transaction :: !SolanaTxn
  } deriving (Eq, Show, Generic)

type SolanaLeaderSchedule = Map Base58ByteString [Word64]

newtype SolanaRpcErrorOrResult a = SolanaRpcErrorOrResult { unSolanaRpcErrorOrResult :: Either SolanaRpcError (SolanaRpcResultF a) }

instance FromJSON a => FromJSON (SolanaRpcErrorOrResult a) where
  parseJSON x = (SolanaRpcErrorOrResult . Left <$> parseJSON x)
            <|> (SolanaRpcErrorOrResult . Right <$> parseJSON x)


data SolanaSlotNotification = SolanaSlotNotification
  { _solanaSlotNotification_parent :: !Word64
  , _solanaSlotNotification_root :: !Word64
  , _solanaSlotNotification_slot :: !Word64
  } deriving Show

data SolanaBlockCommitment = SolanaBlockCommitment
  { _solanaBlockCommitment_totalStake :: !Word64
  , _solanaBlockCommitment_commitment :: ![Word64]
  } deriving Show

satsub :: Word64 -> Word64 -> Word64
satsub x y
  | x <= y = 0
  | otherwise = x - y


do
  let x = (defaultOptions { fieldLabelModifier = dropWhile ('_' ==) . dropWhile ('_' /=) . dropWhile ('_' ==) })
  concat <$> traverse (deriveJSON x)
    [ ''SolanaRpcResultF
    , ''SolanaRpcError
    , ''SolanaRpcRequestF
    , ''SolanaRpcNotificationF
    , ''SolanaRpcNotificationParamsF
    , ''SolanaEpochInfo
    , ''SolanaEpochSchedule
    , ''SolanaSlotNotification
    -- , ''SolanaVote
    , ''SolanaBlockCommitment
    , ''SolanaCommittedBlock
    , ''SolanaTxnWithMeta
    , ''SolanaTxn
    , ''SolanaTxnMessage
    , ''SolanaTxnHeader
    ]

concat <$> traverse (makeLenses)
  [ ''SolanaTxn
  , ''SolanaTxnMessage
  , ''SolanaTxnHeader
  , ''SolanaTxnInstruction
  ]
