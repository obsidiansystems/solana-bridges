{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Solana.RPC where

import Solana.Types
import Data.Foldable
import Control.Lens (view, ix, _Left)
import Control.Concurrent.MVar
import Data.IORef
import Data.Bifunctor
import Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Network.WebSockets as WebSockets
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.HTTP.Types.Status as HTTP
import Network.Socket(withSocketsDo)
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap (IntMap)
import Control.Concurrent (forkIO, killThread)
import Control.Monad.Catch (finally)
import Data.Void
import Data.Word


getEpochSchedule :: SolanaRpcM IO SolanaEpochSchedule
getEpochSchedule = rpcWebRequest @SolanaEpochSchedule "getEpochSchedule"

getEpochInfo :: SolanaRpcM IO SolanaEpochInfo
getEpochInfo = rpcWebRequest "getEpochInfo"

getConfirmedBlock :: Word64 -> SolanaRpcM IO (Either Value (Maybe SolanaCommittedBlock))
getConfirmedBlock slot = rpcWebRequest'' @_ @(Maybe SolanaCommittedBlock) "getConfirmedBlock" $ Just [slot]

getConfirmedBlocks :: Word64 -> Word64 -> SolanaRpcM IO [Word64]
getConfirmedBlocks startSlot endSlot = rpcWebRequest' "getConfirmedBlocks" $ Just (startSlot, endSlot)

-- TODO: the real RPC doesn't work as advertised
getConfirmedBlocksWithLimit :: Word64 -> Word64 -> SolanaRpcM IO [Word64]
-- getConfirmedBlocksWithLimit startSlot endSlot = rpcWebRequest' "getConfirmedBlocksWithLimit" $ Just (startSlot, endSlot)
getConfirmedBlocksWithLimit startSlot limit = getConfirmedBlocks startSlot (startSlot + limit)

getLeaderSchedule :: Word64 -> SolanaRpcM IO SolanaLeaderSchedule
getLeaderSchedule slot = rpcWebRequest' @[Word64] @SolanaLeaderSchedule "getLeaderSchedule" $ Just [slot]


rootSubscribe :: (Either String Word64 -> IO ()) -> SolanaRpcM IO ()
rootSubscribe = sendRPCSubscription @Void @Word64 "rootSubscribe" (Nothing :: Maybe Void)

slotSubscribe :: (Either String SolanaSlotNotification -> IO ()) -> SolanaRpcM IO ()
slotSubscribe = sendRPCSubscription @Void @SolanaSlotNotification "slotSubscribe" Nothing


withSolanaWebSocket :: SolanaRpcConfig -> SolanaRpcM IO a -> IO a
withSolanaWebSocket cfg (SolanaRpcM go) = do

  req0 <- (HTTPClient.parseRequest "http://127.0.0.1:8899")
  httpMgr <- newTlsManager

  requestId <- newIORef 0
  requestHandlers :: IORef (IntMap (Value -> IO ())) <- newIORef IntMap.empty
  notifyHandlers :: IORef (IntMap (Either [Value] (Value -> IO ()))) <- newIORef IntMap.empty
  rpcCriticalSection <- newMVar ()

  withSocketsDo $ WebSockets.runClient "127.0.0.1" 8900 "/" $ \conn -> do

    threadId <- forkIO $ forever $ do
      msgOrNotify <- either error pure . decodeMsg =<< WebSockets.receiveDataMessage conn
      case msgOrNotify of
        Left msg -> join $ atomicModifyIORef' notifyHandlers $ \oldState ->
          case IntMap.lookup (_solanaRpcNotificationParams_subscription $ _solanaRpcNotification_params msg) oldState of
            Nothing -> (IntMap.insert (_solanaRpcNotificationParams_subscription $ _solanaRpcNotification_params msg) (Left $ [_solanaRpcNotificationParams_result $ _solanaRpcNotification_params msg]) oldState, pure ())
            Just (Right handler) -> (oldState, handler $ _solanaRpcNotificationParams_result $ _solanaRpcNotification_params msg)
            Just (Left stillPending) -> (IntMap.insert (_solanaRpcNotificationParams_subscription $ _solanaRpcNotification_params msg) (Left $ stillPending <> [_solanaRpcNotificationParams_result $ _solanaRpcNotification_params msg]) oldState, pure ())
        Right msg -> join $ atomicModifyIORef' requestHandlers $ \oldState ->
          case IntMap.lookup (_solanaRpcResult_id msg) oldState of
            Nothing -> (,) oldState $ pure ()
            Just handler -> (,) (IntMap.delete (_solanaRpcResult_id msg) oldState) $
              handler (_solanaRpcResult_result msg)



    flip finally (killThread threadId) $ runReaderT go $ SolanaRpcContext
      { _solanaRpcContext_baseRpcRequest = req0
        { HTTPClient.host = _solanaRpcConfig_host cfg
        }
      , _solanaRpcContext_httpMgr = httpMgr
      , _solanaRpcContext_wsConn = conn
      , _solanaRpcContext_requestHandlers = requestHandlers
      , _solanaRpcContext_notifyHandlers = notifyHandlers
      , _solanaRpcContext_rpcCriticalSection = rpcCriticalSection
      , _solanaRpcContext_requestId = requestId
      }

unliftSolanaRpcM :: SolanaRpcM IO (SolanaRpcM IO a -> IO a)
unliftSolanaRpcM = SolanaRpcM $ do
  ctx <- ask
  pure $ ($ctx) . runReaderT . runSolanaRpcM

-- The low level internals follow:

decodeMsg :: WebSockets.DataMessage -> Either String (Either SolanaRpcNotification SolanaRpcResult)
decodeMsg = \case
    WebSockets.Text x _ -> go x
    WebSockets.Binary x -> go x
  where
    go x = first (<> show x) $ case decode' x of
      Just rpcresult -> Right (Right rpcresult)
      Nothing -> Left <$> eitherDecode' x


rpcWebRequest'' :: forall a b. (Show a, ToJSON a, FromJSON b) => T.Text -> Maybe a -> SolanaRpcM IO (Either Value b)
rpcWebRequest'' method params = do
  req0 <- SolanaRpcM $ asks _solanaRpcContext_baseRpcRequest -- (HTTPClient.parseRequest "http://127.0.0.1:8899")
  httpMgr <- SolanaRpcM $ asks _solanaRpcContext_httpMgr
  let req = req0
        { HTTPClient.method = "POST"
        , HTTPClient.requestBody = HTTPClient.RequestBodyLBS $ encode $ SolanaRpcRequest
          { _solanaRpcRequest_jsonrpc = JsonRpcVersion
          , _solanaRpcRequest_id = 1
          , _solanaRpcRequest_method = method
          , _solanaRpcRequest_params = params
          }
        , HTTPClient.requestHeaders = ("Content-Type", "application/json") : HTTPClient.requestHeaders req0
        }

  resultLBS <- liftIO $ HTTPClient.httpLbs req httpMgr
  unless (HTTP.statusIsSuccessful $ HTTPClient.responseStatus resultLBS) $
    error $ show $ HTTPClient.responseStatus resultLBS

  either (error . (\bad -> unlines [bad, show (method, params), show (HTTPClient.responseBody resultLBS)])) (pure . bimap _solanaRpcError_error _solanaRpcResult_result . unSolanaRpcErrorOrResult)
    $ eitherDecode' @(SolanaRpcErrorOrResult b) $ HTTPClient.responseBody resultLBS

rpcWebRequest :: FromJSON b => T.Text -> SolanaRpcM IO b
rpcWebRequest method = rpcWebRequest' method $ (Nothing :: Maybe Void)

rpcWebRequest' :: (Show a, ToJSON a, FromJSON b) => T.Text -> Maybe a -> SolanaRpcM IO b
rpcWebRequest' method args = rpcWebRequest'' method args >>= either (error . show) pure


sendRPCSubscription :: forall a b. (ToJSON a, FromJSON b) => T.Text -> Maybe a -> (Either String b -> IO ()) -> SolanaRpcM IO ()
sendRPCSubscription method params handler = do
  rpcCriticalSection <- SolanaRpcM $ asks _solanaRpcContext_rpcCriticalSection
  requestId <- SolanaRpcM $ asks _solanaRpcContext_requestId
  requestHandlers <- SolanaRpcM $ asks _solanaRpcContext_requestHandlers
  conn <- SolanaRpcM $ asks _solanaRpcContext_wsConn
  -- let
  --   rpcWSRequest :: IO Value
  result <- liftIO $ newEmptyMVar
  newId <- liftIO $ atomicModifyIORef' requestId $ \x -> (x, succ x)
  subIdJSON <- SolanaRpcM $ lift $ withMVar rpcCriticalSection $ \() -> do
      join $ atomicModifyIORef' requestHandlers $ \oldState -> (,) (IntMap.insert newId (putMVar result) oldState) $ do
        WebSockets.sendBinaryData conn $ Data.Aeson.encode $ SolanaRpcRequest
          { _solanaRpcRequest_jsonrpc = JsonRpcVersion
          , _solanaRpcRequest_id = newId
          , _solanaRpcRequest_method = method
          , _solanaRpcRequest_params = params
          }
      readMVar result
  -- sendRPCRequest method params $ \subIdJSON -> do
  subId <- case fromJSON subIdJSON of
    Success x -> pure x
    Error x -> error $ x <> ": " <> show subIdJSON
  notifyHandlers <- SolanaRpcM $ asks _solanaRpcContext_notifyHandlers
  lift $ join $ atomicModifyIORef' notifyHandlers $ \oldState ->
    let
      handler' :: Value -> IO ()
      handler' x = handler $ case fromJSON x of
        Success x' -> Right x'
        Error x' -> Left $ x' <> ": " <> show subIdJSON
      newState = IntMap.insert subId (Right handler') oldState
      pendingNotifications = traverse_ handler' $ view (ix subId . _Left) oldState
    in (newState, pendingNotifications)

newtype SolanaRpcM m a = SolanaRpcM { runSolanaRpcM :: ReaderT SolanaRpcContext m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadFail)

-- TODO: tls
data SolanaRpcConfig = SolanaRpcConfig
  { _solanaRpcConfig_host :: BS.ByteString
  , _solanaRpcConfig_rpcPort :: Int
  , _solanaRpcConfig_wsPort :: Int
  }

instance FromJSON SolanaRpcConfig where
  parseJSON v = (\(a, b, c) -> SolanaRpcConfig (T.encodeUtf8 a) b c) <$> parseJSON v
instance ToJSON SolanaRpcConfig where
  toJSON (SolanaRpcConfig a b c) = toJSON (T.decodeUtf8 a, b, c)

data SolanaRpcContext = SolanaRpcContext
  { _solanaRpcContext_baseRpcRequest :: HTTPClient.Request
  , _solanaRpcContext_httpMgr :: !HTTPClient.Manager
  , _solanaRpcContext_wsConn :: !WebSockets.Connection
  , _solanaRpcContext_requestHandlers :: !(IORef (IntMap (Value -> IO ())))
  , _solanaRpcContext_notifyHandlers :: !(IORef (IntMap (Either [Value] (Value -> IO ()))))
  , _solanaRpcContext_requestId :: !(IORef Int)
  , _solanaRpcContext_rpcCriticalSection :: !(MVar ())
  }
