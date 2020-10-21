{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Solana.Types where

import           Data.Aeson
import           Data.Aeson.Types (toJSONKeyText)
import           Data.Aeson.TH
import qualified Data.Text as T
import qualified Data.ByteString as BS
import           Data.Word
import Data.Time
import           Data.Text (Text)
import qualified Data.ByteString.Base58 as Base58
import qualified Data.Text.Encoding as T
import           Control.Applicative
import Data.Map (Map)

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

epochFromSlot :: SolanaEpochSchedule -> Word64 -> SolanaEpochInfo
epochFromSlot schedule =
  let
    warmup0 = (\x -> div (_solanaEpochSchedule_slotsPerEpoch schedule) $ 2 ^ x)
      <$> reverse [1.._solanaEpochSchedule_firstNormalEpoch schedule]
    warmup = zip [0..] $ zip warmup0 $ drop 1 $ scanl (+) 0 warmup0
  in \absoluteSlot ->
    let
      (epoch, (slotsInEpoch, firstSlotInEpoch)) = if absoluteSlot >= _solanaEpochSchedule_firstNormalSlot schedule
        then
          ((absoluteSlot - _solanaEpochSchedule_firstNormalSlot schedule) `div` _solanaEpochSchedule_slotsPerEpoch schedule
          , ( _solanaEpochSchedule_slotsPerEpoch schedule
            , _solanaEpochSchedule_firstNormalSlot schedule + (epoch - _solanaEpochSchedule_firstNormalEpoch schedule) * _solanaEpochSchedule_slotsPerEpoch schedule
            )
          )
        else
          head $ dropWhile (\(_, (_, firstSlotInEpoch')) -> firstSlotInEpoch' < absoluteSlot) warmup
    in SolanaEpochInfo
      { _solanaEpochInfo_slotsInEpoch = slotsInEpoch
      , _solanaEpochInfo_slotIndex = absoluteSlot - (firstSlotInEpoch - slotsInEpoch)
      , _solanaEpochInfo_absoluteSlot = absoluteSlot
      , _solanaEpochInfo_blockHeight = 0 -- or maybe this should be undefined?
      , _solanaEpochInfo_epoch = epoch
      }

data SolanaVote = SolanaVote
  { _solanaVote_slots :: ![Word64]
  , _solanaVote_hash :: !Base58ByteString -- TODO different type
  , _solanaVote_timestamp :: !(Maybe UTCTime)
  } deriving Show


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
  -- , _solanaCommittedBlock_transactions :: ![Value]
  -- , _solanaCommittedBlock_rewards :: ![Value]
  , _solanaCommittedBlock_blockTime :: !(Maybe Word64)
  } deriving Show

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
    , ''SolanaVote
    , ''SolanaBlockCommitment
    , ''SolanaCommittedBlock
    ]
