{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Solana.Relayer where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_, withAsync)
import Control.Concurrent.MVar
import Control.Exception (SomeException, handle)
import Control.Lens
import Control.Monad
import Control.Monad.Catch (catch)
import Control.Monad.Catch (finally)
import Control.Monad.Except (runExceptT, throwError, ExceptT(..), MonadError)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans (lift)
import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (SHA256(..), SHA512(..))
import Data.Aeson
import Data.Aeson.Lens (_String, key, nth)
import Data.Aeson.TH
import Data.Aeson.Types (Parser, explicitParseField, listParser)
import Data.Bits
import Data.ByteArray.HexString
import Data.Default (def)
import Data.FileEmbed (embedFile)
import Data.Foldable
import Data.Bifunctor
import Data.Functor.Compose
import Data.List (intercalate, unfoldr)
import Data.List.Split (chunksOf)
import Data.List.NonEmpty (nonEmpty)
import Data.Map (Map)
import Data.Maybe(fromMaybe, isJust)
import Data.Semigroup (stimesMonoid)
import Data.Solidity.Prim.Address (Address)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Data.Word
import GHC.Generics (Generic)
import Network.Ethereum.Api.Types (Call(..))
import Network.JsonRpc.TinyClient as Eth
import Network.URI (URI(..), uriToString, parseURI)
import Network.Web3.Provider (runWeb3, runWeb3')
import System.Directory (canonicalizePath, copyFile, createDirectory, createDirectoryIfMissing, getCurrentDirectory, removeDirectoryRecursive, removeFile)
import System.Environment
import System.Exit
import System.IO (BufferMode(LineBuffering), stdout, stderr, hFlush, hPutStrLn, hSetBuffering)
import System.IO.Error (isAlreadyExistsError, isDoesNotExistError)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Posix.Files (createSymbolicLink)
import System.Process (CreateProcess(..), createProcess, proc, readProcess, readCreateProcessWithExitCode, spawnProcess, terminateProcess, waitForProcess)
import System.Which (staticWhich)
import Text.Read (readEither)
import qualified Blockchain.Data.RLP as RLP
import qualified Crypto.Error
import qualified Crypto.PubKey.Ed25519
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Network.Ethereum.Api.Debug as Eth
import qualified Network.Ethereum.Api.Eth as Eth (getBlockByNumber)
import qualified Network.Ethereum.Api.Eth as Eth (getTransactionReceipt)
import qualified Network.Ethereum.Api.Eth as Eth (sendTransaction)
import qualified Network.Ethereum.Api.Types as Eth
import qualified Network.Web3.Provider as Eth
import qualified System.Process.ByteString.Lazy

import Ethereum.Contracts as Contracts
import Ethereum.Contracts.Dist (solanaClientContractBin)
import Solana.RPC
import Solana.Types

ethashElementsPerInstruction :: Int
ethashElementsPerInstruction = 8

mainRelayEthereumToSolana :: IO ()
mainRelayEthereumToSolana = do
  getArgs >>= \case
    configFile:[] -> do
      configData <- BS.readFile configFile
      config :: ContractConfig <- case eitherDecodeStrict' configData of
          Right c -> pure c
          Left e -> fail $ show e

      relayEthereumToSolana configFile config

    _ -> do
      progName <- getProgName
      hPutStrLn stderr $ "USAGE: " <> progName <> " CONFIGFILE.json"

type SolanaToEthereumConfig = (Eth.Provider, Address, SolanaRpcConfig)

mainRelaySolanaToEthereum :: IO ()
mainRelaySolanaToEthereum = do
  getArgs >>= \case
    configFile:[] -> do
      configData <- BS.readFile configFile
      (node, address, solanaConfig) :: SolanaToEthereumConfig <- case eitherDecodeStrict' configData of
          Right c -> pure c
          Left e -> fail $ show e

      relaySolanaToEthereum node solanaConfig address

    _ -> do
      progName <- getProgName
      hPutStrLn stderr $ "USAGE: " <> progName <> " CONFIGFILE.json"


uriToProvider :: URI -> Either Text Eth.Provider
uriToProvider uri = case uriScheme uri of
  "http:" -> httpProvider
  "https:" -> httpProvider

  invalidSchema -> Left $ "invalid schema: " <> T.pack invalidSchema
  where
    httpProvider = Right $ Eth.HttpProvider $ uriToString id uri ""

defaultEthereumRPCConfig :: Eth.Provider
defaultEthereumRPCConfig = def

defaultSolanaRPCConfig :: SolanaRpcConfig
defaultSolanaRPCConfig = SolanaRpcConfig "127.0.0.1" 8899 8900

mainDeploySolanaClientContract :: IO ()
mainDeploySolanaClientContract = do
  mProvider <- getArgs <&> \case
    [] -> Right (defaultEthereumRPCConfig, defaultSolanaRPCConfig)
    ethUrl:solHost:solPort:solWs:[] -> do
      ethProvider <- case parseURI ethUrl of
        Just ethUrl' -> uriToProvider ethUrl'
        Nothing -> Left "invalid uri"
      solPort' <- first T.pack $ readEither solPort
      solWs' <- first T.pack $ readEither solWs
      pure $ (,) ethProvider $ SolanaRpcConfig (T.encodeUtf8 $ T.pack solHost) solPort' solWs'
    _ -> Left ""

  case mProvider of
    Left err -> do
      progName <- getProgName
      T.hPutStrLn stderr $ T.unlines
        ["USAGE: " <> T.pack progName <> " [ETH-PROVIDER SOLANA-RPC-HOST SOLANA-PORT SOLANA-WEBSOCKET]"
        , "\tETH-PROVIDER\tethereum provider url"
        , "\t\thttp://host[:port]"
        -- , "\t\tws://host:port"
        , "\tSOLANA-RPC-HOST\tsolana validator host"
        , "\t\thost"
        , "\tSOLANA-PORT\tsolana validator rpc port"
        , "\t\tport"
        , "\tSOLANA-WEBSOCKET\tsolana validator websocket port"
        , "\t\tport"
        , err
        ]
    Right (provider, solanaConfig) -> do
      (ca, _slot0) <- deploySolanaClientContract provider solanaConfig
      let config :: SolanaToEthereumConfig = (provider, ca, solanaConfig)

      LBS.putStr $ encode config
      putStrLn ""

runEthereumTestnet :: IO ()
runEthereumTestnet = do
  currentDir <- getCurrentDirectory
  runDir <- canonicalizePath =<< createTempDirectory currentDir ".run"

  setupEth currentDir runDir
  withGeth runDir $ forever $ threadDelay 1e6

deployAndRunSolanaRelayer :: IO ()
deployAndRunSolanaRelayer = do
  let solanaConfig = (SolanaRpcConfig "127.0.0.1" 8899 8900)
  (ca, _slot0) <- deploySolanaClientContract def solanaConfig
  relaySolanaToEthereum def solanaConfig ca

runSolanaTestnet :: IO ()
runSolanaTestnet = do
  getArgs >>= \case
    [genesisArchive] -> setupSolana genesisArchive

    _ -> do
      progName <- getProgName
      hPutStrLn stderr $ "USAGE: " <> progName <> " <GENESIS>.tar.bz2"

data SolanaSpecialPaths = SolanaSpecialPaths
  { _solanaSpecialPaths_splToken :: !FilePath
  , _solanaSpecialPaths_splMemo :: !FilePath
  } deriving (Eq, Show)

data SolanaKeypairFiles = SolanaKeypairFiles
  { _solanaKeypairFiles_faucet :: !FilePath
  , _solanaKeypairFiles_bootstrapValidator :: !FilePath
  , _solanaKeypairFiles_voteAccount :: !FilePath
  , _solanaKeypairFiles_stakeAccount :: !FilePath
  }

makeSolanaKeypairFiles :: IO SolanaKeypairFiles
makeSolanaKeypairFiles = do
  tmp <- getCanonicalTemporaryDirectory
  keypairsDir <- createTempDirectory tmp "solana-keypairs"
  let
    bootstrapDir = keypairsDir <> "/bootstrap-validator"
    files = SolanaKeypairFiles
      { _solanaKeypairFiles_faucet = keypairsDir <> "/faucet.json"
      , _solanaKeypairFiles_bootstrapValidator = bootstrapDir <> "/identity.json"
      , _solanaKeypairFiles_voteAccount = bootstrapDir <> "/vote-account.json"
      , _solanaKeypairFiles_stakeAccount = bootstrapDir <> "/stake-account.json"
      }

  createDirectoryIfMissing True bootstrapDir
  T.writeFile (_solanaKeypairFiles_bootstrapValidator files) solanaBootstrapValidatorIdentityKeypair
  T.writeFile (_solanaKeypairFiles_voteAccount files) voteAccountKeypair
  T.writeFile (_solanaKeypairFiles_stakeAccount files) stakeAccountKeypair
  T.writeFile (_solanaKeypairFiles_faucet files) solanaFaucetKeypair

  pure files

makeGenesisArchive :: IO FilePath
makeGenesisArchive = do
  splPaths <- SolanaSpecialPaths <$> getEnv "SPL_TOKEN" <*> getEnv "SPL_MEMO"
  keypairs <- makeSolanaKeypairFiles
  tmp <- getCanonicalTemporaryDirectory
  ledgerPath <- createTempDirectory tmp "solana-genesis"

  let
    genesisArchivePath = ledgerPath <> "/genesis.tar.bz2"

    genArgs :: [String]
    genArgs =
      [ "--max-genesis-archive-unpacked-size", "1073741824"
      , "--enable-warmup-epochs"
      , "--bootstrap-validator"
      , _solanaKeypairFiles_bootstrapValidator keypairs
      , _solanaKeypairFiles_voteAccount keypairs
      , _solanaKeypairFiles_stakeAccount keypairs
      , "--bpf-program", "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA", "BPFLoader1111111111111111111111111111111111", _solanaSpecialPaths_splToken splPaths
      , "--bpf-program", "Memo1UhkJRfHyvLMcVucJwxXeuD728EqVDDwQDxFMNo", "BPFLoader1111111111111111111111111111111111", _solanaSpecialPaths_splMemo splPaths
      , "--ledger", ledgerPath
      , "--faucet-pubkey"
      , _solanaKeypairFiles_faucet keypairs
      , "--faucet-lamports", "500000000000000000"
      , "--hashes-per-tick", "sleep"
      , "--cluster-type", "development"
      ]
    p = proc solanaGenesisPath genArgs
    go n = do
      readCreateProcessWithExitCode p "" >>= \case
        good@(ExitSuccess, _, _) -> hPutStrLn stderr $ show good
        (ExitFailure 1,"",bad@"Error: IO(Custom { kind: Other, error: \"Error checking to unpack genesis archive: Archive error: extra entry found: \\\"genesis.bin\\\"\" })\n") -> do
          hPutStrLn stderr $ "Failed attempt " <> show n <> " at generating genesis file: " <> show bad
          removeDirectoryRecursive ledgerPath
          go (n+1)
        bad -> error $ "Unexpected failure solana-genesis\n\t" <> show bad

  hPutStrLn stderr $ unwords $ solanaGenesisPath:genArgs
  go (1 :: Int)

  pure genesisArchivePath

setupSolana :: FilePath -> IO ()
setupSolana genesisArchive = do
  currentDir <- getCurrentDirectory
  solanaConfigDir <- canonicalizePath =<< createTempDirectory currentDir ".run"

  putStrLn solanaConfigDir

  keypairs <- makeSolanaKeypairFiles

  let
    ledgerPath = solanaConfigDir <> "/ledger"
    genesisPath = ledgerPath <> "/genesis.tar.bz2"
    bootstrapValidator = (proc solanaValidatorPath
      [ "--ledger", ledgerPath
      , "--rpc-port", "8899"
      , "--identity", _solanaKeypairFiles_bootstrapValidator keypairs
      , "--vote-account" , _solanaKeypairFiles_voteAccount keypairs
      , "--rpc-faucet-address", "127.0.0.1:9900"
      , "--bind-address", "127.0.0.1"
      , "--enable-rpc-exit"
      , "--enable-rpc-transaction-history"
      , "--log", "-"
      ])
        { env = Just
          [("RUST_LOG", intercalate ","
            [ "info"
            , "solana_core::replay_stage=error"
            , "solana_metrics=error"
            , "solana_ledger::blockstore=error"
            , "solana_core::poh_recorder=error"
            , "solana_runtime::bank=error"
            ])
          ]
        }

  createDirectoryIfMissing True ledgerPath
  copyFile genesisArchive genesisPath

  faucet <- spawnProcess solanaFaucetPath
    ["--keypair", _solanaKeypairFiles_faucet keypairs]

  (_, _, _, validator) <- createProcess bootstrapValidator
  _ <- finally (waitForProcess validator) (terminateProcess faucet)
  pure ()


setupEth :: FilePath -> FilePath -> IO ()
setupEth currentDir runDir = do
  let latestSymlinks = currentDir <> "/.run-latest"
  let logsSymlink = logsSubdir latestSymlinks
  createDirectory latestSymlinks & allow isAlreadyExistsError
  putStrLn $ "Placing run data in " <> runDir
  removeFile logsSymlink & allow isDoesNotExistError
  createSymbolicLink (logsSubdir runDir) logsSymlink
  putStrLn $ "Made symlink to latest logs: " <> logsSymlink
  where
    allow predicate = handle $ \err ->
      unless (predicate err) $ error (show err)

challengePayout :: Num a => a
challengePayout = 1e7

createContract :: Address -> HexString -> Call
createContract fromAddr hex = Eth.Call
    { callFrom = Just fromAddr
    , callTo = Nothing
    , callGas = Just 25e6
    , callGasPrice = Just 1
    , callValue = Just challengePayout
    , callData = Just hex
    , callNonce = Nothing
    }

data SolanaClientState = SolanaClientState
  { _solanaClientState_height :: Word64
  , _solanaClientState_offset :: Word64
  , _solanaClientState_full :: Bool
  , _solanaClientState_missingElementsBitmask :: Word16
  } deriving (Eq, Ord, Show, Generic)

relayEthereumToSolana :: FilePath -> ContractConfig -> IO ()
relayEthereumToSolana configFile config = do
  hSetBuffering stdout LineBuffering
  let solanaAccountLookupArgs = proc solanaPath $ T.unpack <$>
        [ "account"
        , _contractConfig_accountId config
        , "--output", "json"
        ]

      getSolanaClientState = SolanaClientState
        <$> Binary.getWord64le
        <*> Binary.getWord64le
        <*> getFull
        <*> Binary.getWord16le
        where
          -- 'bool' is aligned to 16 bits for some reason, but only one bit can be non-zero so we don't care about endianness
          getFull = fmap (/= 0) Binary.getWord16le

      fetchClientState = System.Process.ByteString.Lazy.readCreateProcessWithExitCode solanaAccountLookupArgs "" >>= \case
        (ExitSuccess, accountData, _) -> either (error . ("bad: " <>) ) pure $ do
          x :: Value <- eitherDecode' accountData
          x1 <- maybe (Left "missing account data") pure $ preview (key "account" . key "data" . nth 0 . _String) x
          () <- maybe (Left "invalid encoding") pure $ preview (key "account" . key "data" . nth 1 . _String . only "base64") x
          x2 <- maybe (Left "failed to decode") pure $ preview _Right $ Base64.decode $ T.encodeUtf8 x1
          bimap (view _3) (view _3) $ Binary.runGetOrFail getSolanaClientState $ LBS.fromStrict x2

        bad -> error $ show bad


  SolanaClientState highestBlock nextBlockOffset isFull missingElementsBitmask <- fetchClientState
  let contractState = case (highestBlock, nextBlockOffset, isFull) of
        (0, 0, False) -> Nothing
        _ -> Just $ succ highestBlock

  let
    bridgeToolProc command args =
      proc solanaBridgeToolPath $ fmap T.unpack $
           [ command
           , "--config", T.pack configFile
           -- , "--payer", "/dev/null"
           ] <> args

    printTxn txn = do
      putStrLn ("Submitted transaction: " <> show txn)
      hFlush stdout

    relayEthashElements :: Word64 -> IO ()
    relayEthashElements height = do
      elems <- getEthashElements height >>= \case
        Left err -> error (T.unpack err)
        Right ee -> pure ee

      let
        chunkSize = ethashElementsPerInstruction
        chunks = chunksOf chunkSize elems

      forConcurrently_ (zip [0..] chunks) $ \(idx, chunk) -> do
          let
            offset = chunkSize * idx;
            raw = LBS.toStrict (Binary.runPut $ Binary.putWord64le height)
              : BS.singleton (fromIntegral idx)
              : fmap unHexString chunk
            dataHex = T.concat $ T.decodeLatin1 . B16.encode <$> raw
            p = bridgeToolProc "provide-ethash-element"
              ["--element", dataHex]

          putStrLn $ "Relaying ethash elements " <> show offset <> " through " <> show (offset + chunkSize - 1) <> ":"
          readCreateProcessWithExitCode p "" >>= \case
            (ExitSuccess, txn, _) -> printTxn txn
            bad -> error $ show bad

  let relayingStart = fromMaybe 1 $ _contractConfig_loopStart config
  let loop :: Word64 -> IO (Either Eth.Web3Error Void)
      loop n = do
        client <- fetchClientState
        print client

        let doEth :: forall resp. Eth.Web3 resp -> IO resp
            doEth m = do
              res :: Either Eth.Web3Error a <- catch (runWeb3 m)
                (\case
                    ParsingException msg -> fail msg
                    CallException _ -> do
                      T.putStrLn $ "No new block, waiting (" <> T.pack (show n) <> ")"
                      threadDelay 5e6
                      (fmap . fmap) (\case) $ loop n)
              case res of
                Left e -> fail $ show e
                Right res' -> pure res'


        mTotalDifficulty <- case n == relayingStart of
          False -> pure Nothing
          True -> fmap (Just . Eth.blockTotalDifficulty) $
            doEth $ Eth.getBlockByNumber $ Eth.Quantity $ toInteger n
        rlp <- doEth $ Eth.getBlockRlp n
        let blockHeader = blockToHeader rlp
        let instructionData = case mTotalDifficulty of
              Nothing -> blockHeader
              Just (Eth.Quantity totalDifficulty) -> RLP.RLPArray
                [ -- Reversed for big endian
                  RLP.RLPString $ BS.pack $ reverse $ unroll totalDifficulty
                , blockHeader
                ]
        let instructionDataHex = T.decodeLatin1 $ B16.encode $ RLP.rlpSerialize instructionData
        T.putStrLn ""
        T.putStrLn $ "Block " <> T.pack (show n)
        T.putStrLn $ "Relaying header: " <> instructionDataHex
        let p = bridgeToolProc (if n == relayingStart then "initialize" else "new-block")
              ["--instruction", instructionDataHex]

        readCreateProcessWithExitCode p "" >>= \case
          (ExitSuccess, txn, _) -> printTxn txn
          bad -> error $ show bad

        relayEthashElements n

        loop $ n + 1

  let loopStart = fromMaybe relayingStart contractState

  when (isJust contractState && zeroBits /= missingElementsBitmask) $
    relayEthashElements (loopStart - 1)

  loop loopStart >>= \case
    Right x -> pure $ case x of {}
    Left bad -> error $ show bad

unroll :: (Integral a, Bits a) => a -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

blockToHeader :: Text -> RLP.RLPObject
blockToHeader rlp = blockHeader
  where
    (blockData, "") = B16.decode $ T.encodeUtf8 rlp
    RLP.RLPArray (blockHeader:_) = RLP.rlpDeserialize blockData


testSolanaCrypto :: Eth.Provider -> Address -> IO ()
testSolanaCrypto node ca = do
  print ca
  let
    toShortHex :: BS.ByteString -> String
    toShortHex = \msg ->
        (show $ if BS.length msg > 16
          then (B16.encode (BS.take 8 msg) <> "..." <> B16.encode (BS.drop (BS.length msg - 8) msg))
          else (B16.encode msg)) <> show (BS.length msg) <> " bytes"
    check_ed25519_testvector (HexString msg) pkHex@(HexString pk) sigHex@(HexString sig) = do
      liftIO $ putStrLn $ "TEST ed25519: " <> toShortHex msg
      liftIO $ putStrLn $ "\tpublic key:" <> show pkHex <> " signature: " <> show sigHex
      testSig <- test_ed25519_verify node ca sig msg (Base58ByteString pk)
      unless testSig $ error $ unlines
        [ "ed25519_valid test vector failed:"
        , "sig:" <> show (HexString sig)
        , "msg:" <> show msg
        , "pubkey" <> show (HexString pk)
        ]
      rcpt <- test_ed25519_verify_gas node ca sig msg (Base58ByteString pk)
      liftIO $ putStrLn $ "\tGas used: " <> show (Eth.receiptCumulativeGasUsed rcpt)

    check_sha512_testvector
      :: (MonadIO m, MonadError String m)
      => BS.ByteString -> HexString -> m ()
    check_sha512_testvector msg (HexString expectedDigest) = do
      liftIO $ putStrLn $ "TEST sha512: " <> toShortHex msg
      testDigest <- test_sha512 node ca msg
      unless (expectedDigest == testDigest) $ error $ unlines
        [ "sha512 test vector failed:"
        , "msg: " <> show msg
        , "expected: " <> show (HexString expectedDigest)
        , "got: " <> show (HexString testDigest)
        ]
      rcpt <- test_sha512_gas node ca msg
      liftIO $ putStrLn $ "\tGas used: " <> show (Eth.receiptCumulativeGasUsed rcpt)

    check_sha512_synthetic
      :: (MonadIO m, MonadError String m)
      => BS.ByteString -> m ()
    check_sha512_synthetic msg = do
      let expectedDigest = ByteArray.convert $ hashWith SHA512 msg
      check_sha512_testvector msg (HexString expectedDigest)

    check_ed25519_synthetic sk msg = do
      let
        pk = Crypto.PubKey.Ed25519.toPublic sk
        pk' = ByteArray.convert $ pk
        sig = ByteArray.convert $ Crypto.PubKey.Ed25519.sign sk pk msg
      check_ed25519_testvector (HexString msg) (HexString pk') (HexString sig)


  res <- runExceptT $ do
    _ <- getInitialized node ca

    check_ed25519_testvector "0x"
      "0xd75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
      "0xe5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"

    check_sha512_testvector "abc"
      "0xDDAF35A193617ABACC417349AE20413112E6FA4E89A97EA20A9EEEE64B55D39A2192992A274FC1A836BA3C23A3FEEBBD454D4423643CE80E2A9AC94FA54CA49F"

    check_ed25519_testvector "0xa750c232933dc14b1184d86d8b4ce72e16d69744ba69818b6ac33b1d823bb2c3"
      "0xb49f3a78b1c6a7fca8f3466f33bc0e929f01fba04306c2a7465f46c3759316d9"
      "0x04266c033b91c1322ceb3446c901ffcf3cc40c4034e887c9597ca1893ba7330becbbd8b48142ef35c012c6ba51a66df9308cb6268ad6b1e4b03e70102495790b"


    check_sha512_synthetic
      "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"

    let
      -- derive a key very insecurely
      kdf :: BS.ByteString -> Crypto.PubKey.Ed25519.SecretKey
      kdf x = Crypto.Error.throwCryptoError $ Crypto.PubKey.Ed25519.secretKey $ hashWith SHA256 x

    check_ed25519_testvector "0xddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"
      "0xec172b93ad5e563bf4932c70e1245034c35467ef2efd4d64ebf819683467e2bf"
      "0xdc2a4459e7369633a52b1bf277839a00201009a3efbf3ecb69bea2186c26b58909351fc9ac90b3ecfdfbc7c66431e0303dca179c138ac17ad9bef1177331a704"

    check_ed25519_testvector "0x72"
      "0x3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"
      "0x92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00"

    for_ [0, 1, 11, 110,111, 112, 113, 126, 127,128,129,130] $ \len -> do
      let msg = BS.pack $ take len $ cycle [0..255]
      check_sha512_synthetic msg
      check_ed25519_synthetic
        (kdf $ T.encodeUtf8 $ T.pack $ show len)
        msg

    liftIO $ putStrLn "OK"
  case res of
    Right () -> pure ()
    Left bad -> error bad

deploySolanaClientContractImpl :: Eth.Provider -> IO Address
deploySolanaClientContractImpl node = do
  let
    runWeb3'' = runWeb3' node

    deployContract = do
      liftIO $ hPutStrLn stderr "Deploying contract"
      runWeb3'' (Eth.sendTransaction $ createContract unlockedAddress $ either error id . hexString $ solanaClientContractBin) >>= \case
        Left err -> throwError $ "Transaction failed: " <> show err
        Right tx -> do
          liftIO $ hPutStrLn stderr $ "Submitted contract in transaction " <> T.unpack (toText tx)
          pure tx

    getContractAddress receipt = case Eth.receiptContractAddress receipt of
      Nothing -> throwError $ "Contract address not found"
      Just ca -> pure ca

    waitForTx tx = do
      liftIO $ hPutStrLn stderr "Waiting for transaction to be committed"
      fix $ \go -> do
        runWeb3'' (Eth.getTransactionReceipt tx) >>= \case
          Left err -> throwError $ "getTransactionReceipt error: " <> show err
          Right (Just r) -> pure r
          Right Nothing -> do
            liftIO $ threadDelay 1e6
            go

  res <- runExceptT $ do
    tx <- deployContract
    receipt <- waitForTx tx
    getContractAddress receipt

  case res of
    Left err -> error err
    Right ca -> pure ca



deploySolanaClientContract :: Eth.Provider -> SolanaRpcConfig -> IO (Address, Word64)
deploySolanaClientContract node solanaConfig = do
  ca <- deploySolanaClientContractImpl node
  res <- runExceptT $ do
    liftIO $ hPutStrLn stderr $ "Deployed contract at address: " <> show ca

    ExceptT $ withSolanaWebSocket solanaConfig $ do
      liftIO $ hPutStrLn stderr "Initializing contract"
      -- get the latest confirmed block in
      epochSchedule <- getEpochSchedule
      bootEpochInfo <- getEpochInfo
      let bootSlot = _solanaEpochInfo_absoluteSlot bootEpochInfo
      bootConfirmedBlock <- getConfirmedBlocks (satsub bootSlot 128) bootSlot

      let
        slot0 = fromMaybe (error "no block found") $ lastOf traverse bootConfirmedBlock
        epochInfo0 = epochFromSlot epochSchedule slot0

      leaderSchedule <- getLeaderSchedule slot0
      Right (Just block0) <- getConfirmedBlock slot0

      let
        slotLeader0 :: Base58ByteString
        slotLeader0 = maybe (error "leader not found") fst $ uncons $ Map.keys $ Map.filter (List.elem $ _solanaEpochInfo_slotIndex epochInfo0) leaderSchedule
      runExceptT $ do
        initialize node ca slot0 block0 slotLeader0 epochSchedule
        liftIO $ hPutStrLn stderr $ "Initialized contract with slot " <> show slot0
        pure slot0

  case res of
    Left err -> error err
    Right slot0 -> do
      let
        loopUntilInitialized :: Int -> IO ()
        loopUntilInitialized n = do
          runExceptT (getInitialized node ca) >>= \case
            Right True -> pure ()
            bad -> if n <= 0
              then error $ "contract not initialized" <> show bad
              else do
                threadDelay 1e6
                loopUntilInitialized (pred n)
      loopUntilInitialized 10
      pure (ca, slot0)


relaySolanaToEthereum :: Eth.Provider -> SolanaRpcConfig -> Address -> IO ()
relaySolanaToEthereum node solanaConfig ca = do
  -- map from epoch to schedule
  leaderSchedulesRef :: MVar (Map Word64 SolanaLeaderSchedule) <- newMVar Map.empty

  res <- runExceptT $ do

    void $ getSeenBlocks node ca

    Right _ <- ExceptT $ withSolanaWebSocket solanaConfig $ do
      liftIO $ T.putStrLn "Connected to solana node"

      runExceptT (getInitialized node ca) >>= \case
        Right True -> pure ()
        bad -> error $ "contract not initialized" <> show bad

      epochSchedule <- getEpochSchedule
      let
        epochFromSlot' = epochFromSlot epochSchedule
        firstSlotInEpoch' = firstSlotInEpoch epochSchedule

        loop :: SolanaRpcM IO ()
        loop = do
          bootEpochInfo <- getEpochInfo
          contractSlot <- do
            Right contractSlot <- lift $ runExceptT $ getLastSlot node ca
            pure contractSlot

          let
            splitGE k xs =
              let (myBelow, x, above) = Map.splitLookup k xs
              in (,) myBelow $ case x of
                Nothing -> above
                Just x' -> Map.insert k x' above

            neededSchedules = Set.fromList [_solanaEpochInfo_epoch (epochFromSlot' contractSlot).._solanaEpochInfo_epoch bootEpochInfo]

          restoreSolanaRpcM <- unliftSolanaRpcM

          leaderSchedules <- liftIO $ modifyMVar leaderSchedulesRef $ \knownSchedules -> restoreSolanaRpcM $ do
            let
              haveSchedules = Map.keysSet knownSchedules
            newSchedules <- itraverse (\epoch () -> getLeaderSchedule $ firstSlotInEpoch' epoch) $ Map.fromSet (const ()) $ neededSchedules `Set.difference` haveSchedules
            let
              allKnownSchedules = Map.union knownSchedules newSchedules
              newKnownSchedules = snd $ splitGE (_solanaEpochInfo_epoch (epochFromSlot' contractSlot)) allKnownSchedules
            pure (newKnownSchedules, Map.restrictKeys allKnownSchedules neededSchedules)
          confirmedBlockSlots <- getConfirmedBlocksWithLimit (succ contractSlot) 64 -- (_solanaEpochInfo_absoluteSlot bootEpochInfo)
          confirmedBlocks' <- traverse getConfirmedBlock confirmedBlockSlots
          let Compose (Right (Just confirmedBlocks)) = traverse Compose confirmedBlocks'
              blocksAndSlots = zip confirmedBlockSlots confirmedBlocks

          liftIO $ do
            let rpcSlot = _solanaEpochInfo_absoluteSlot bootEpochInfo
            putStrLn $ unlines
              [ "Solana RPC slot: " <> show rpcSlot
              , "Ethereum contract slot: " <> show contractSlot
              ]
            when (rpcSlot < contractSlot) $ error "Network change detected: client contract is ahead of the Solana network"
            putStrLn $ "Contract is behind by " <> show (rpcSlot - contractSlot)

          case nonEmpty blocksAndSlots of
            Nothing -> liftIO $ threadDelay 1e6
            Just blocks -> do
              liftIO $ putStrLn $ "Sending new slots: " <> show confirmedBlockSlots
              for_ blocks $ \(s, b) -> do
                liftIO $ putStrLn $ "Slot " <> show s
                liftIO $ putStr $ showBlockInstructions b

              let
                mergedSchedules = mergeSchedules leaderSchedules epochSchedule
                blocks' = flip fmap blocks $ \(s,b) -> (s,b,mergedSchedules Map.! s)

              runExceptT (addBlocks node ca blocks') >>= \case
                Left bad -> error $ show bad
                Right _receipt -> pure ()

              liftIO $ putStrLn "Submitted new slots to contract"

              runExceptT (getSeenBlocks node ca) >>= \case
                Left _ -> pure ()
                Right bs -> liftIO $ putStrLn $ "Total blocks accepted by the contract: " <> show bs

          loop

      loop
      pure $ Left "not reachable"
    pure ()

  case res of
    Left err -> putStrLn err
    Right _ -> pure ()

  putStrLn "All done - network can be stopped now"

showBlockInstructions :: SolanaCommittedBlock -> String
showBlockInstructions b = showBlock
  where
    lookupProgram m i =
      Seq.lookup
      (fromIntegral $ _solanaTxnInstruction_programIdIndex i)
      (unCompactArray $ _solanaTxnMessage_accountKeys m)

    showProgram p = fromMaybe (show p) $ show <$> findWellKnownProgram p

    showTransaction idxT t = "  Transaction " <> show idxT <> ": " <> intercalate ", " (imap showInstruction instructions)
      where
        m = _solanaTxn_message $ _solanaTxnWithMeta_transaction t
        instructions = toList $ _solanaTxnMessage_instructions $ m
        showInstruction idxI i = maybe (error malformed) showProgram (lookupProgram m i)
          where malformed = "Malformed instruction #" <> show idxI <>  " has invalid program index: " <> show t

    showBlock = unlines $ imap showTransaction $ _solanaCommittedBlock_transactions b

data ContractConfig = ContractConfig
 { _contractConfig_programId :: Text
 , _contractConfig_accountId :: Text
 , _contractConfig_loopStart :: Maybe Word64
 } deriving (Show, Eq, Ord)

data EthashData = EthashData
  { _ethashData_header :: HexString
  , _ethashData_merkleRoot :: HexString
  , _ethashData_proofLength :: Word
  , _ethashData_elements :: [HexString]
  , _ethashData_merkleProofs :: [HexString]
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON EthashData where
    parseJSON = withObject "EthashData" $ \v -> EthashData
        <$> explicitParseField parseHex v "header_rlp"
        <*> explicitParseField parseHex v "merkle_root"
        <*> v .: "proof_length"
        <*> explicitParseField (listParser parseHex) v "elements"
        <*> explicitParseField (listParser parseHex) v "merkle_proofs"
      where
        parseHex :: Value -> Parser HexString
        parseHex = withText "hex" $ fmap reverseEndianness . parseJSON . String . pad . T.drop 2

        reverseEndianness :: HexString -> HexString
        reverseEndianness = HexString . BS.reverse . unHexString

        pad :: Text -> Text
        pad x =
          let size = max 0 (64 - T.length x)
          in stimesMonoid size "0" <> x

getEthashElements :: Word64 -> IO (Either Text [HexString])
getEthashElements blockHeight = runExceptT $ do
  liftIO $ putStrLn $ "Getting ethash proof data for block " <> show blockHeight
  output <- liftIO $ readProcess ethashproofRelayerPath [show blockHeight] ""
  ethashData <- case eitherDecode' $ LBS.fromStrict $ BSC.dropWhile (/= '{') $ BSC.pack output of
    Left err -> throwError $ T.pack err
    Right d -> pure d
  let elems = _ethashData_elements ethashData
      len = length elems
      paired = fmap snd
               $ filter (\(x, _) -> x `mod` 2 == 0)
               $ zip [(0 :: Int)..]
               $ zip elems (tail elems)
  when (len /= 256) $ throwError $ "Invalid length: " <> T.pack (show len)
  pure $ flip fmap paired $ \(x,y) -> x <> y

ethashproofCachePath :: FilePath
ethashproofCachePath = $(staticWhich "cache")

ethashproofRelayerPath :: FilePath
ethashproofRelayerPath = $(staticWhich "relayer")

solcPath :: FilePath
solcPath = $(staticWhich "solc")

gethPath :: FilePath
gethPath = $(staticWhich "geth")

solanaPath :: FilePath
solanaPath = $(staticWhich "solana")

solanaValidatorPath :: FilePath
solanaValidatorPath = $(staticWhich "solana-validator")

solanaGenesisPath :: FilePath
solanaGenesisPath = $(staticWhich "solana-genesis")

solanaFaucetPath :: FilePath
solanaFaucetPath = $(staticWhich "solana-faucet")

solanaBridgeToolPath :: FilePath
solanaBridgeToolPath = $(staticWhich "solana-bridge-tool")

genesisBlock :: BS.ByteString
genesisBlock = $(embedFile "ethereum/Genesis.json")

ethereumAccountFile :: String
ethereumAccountFile = "UTC--2020-09-17T02-34-16.613Z--0xabc6bbd0ad6aca2d25380fc7835fe088e7690c2c"

ethereumAccount :: BS.ByteString
ethereumAccount = $(embedFile "ethereum/UTC--2020-09-17T02-34-16.613Z--0xabc6bbd0ad6aca2d25380fc7835fe088e7690c2c")

ethereumAccountPass :: BS.ByteString
ethereumAccountPass = $(embedFile "ethereum/pass.txt")

runGeth ::  FilePath -> IO ()
runGeth runDir = do
  let
    dataDir = runDir <> "/.ethereum"
    genesisFile = runDir <> "/Genesis.json"
    passFile = dataDir <> "/pass.txt"

    dataDirArgs = [ "--datadir", dataDir ]
    httpArgs = [ "--http", "--http.api", "eth,net,web3,debug,personal" ]
    mineArgs = [ "--mine", "--miner.threads=1", "--miner.etherbase=" <> unlockedAddress ]
    initArgs = [ "init", genesisFile]
    privateArgs = [ "--nodiscover" ]
    nodeArgs = [ "--identity", "Testnet ethereum node 0"]
    unlockArgs = [ "--allow-insecure-unlock", "--unlock", unlockedAddress, "--password", passFile ]
    gascapArgs = ["--rpc.gascap", show @Integer 25e7]

  putStrLn $ "Creating genesis file: " <> genesisFile
  BS.writeFile (runDir <> "/Genesis.json") genesisBlock

  putStrLn "Initializing node"
  void $ readProcess gethPath (dataDirArgs <> initArgs) ""

  BS.writeFile (dataDir <> "/keystore/" <> ethereumAccountFile) ethereumAccount
  BS.writeFile passFile ethereumAccountPass

  (_,_,_,ph) <- createProcess $ proc gethPath $ fold [ dataDirArgs, httpArgs, mineArgs, privateArgs, unlockArgs, nodeArgs, gascapArgs]
  void $ waitForProcess ph

withGeth :: FilePath -> IO () -> IO ()
withGeth dir action = do
  withAsync action' $ \_ -> runGeth dir

  where
    action' = printErrors $ do
      threadDelay 4e6 -- TODO: poll node instead
      action

    printErrors = handle $ \(e :: SomeException) -> do
      putStrLn $ "Error when interacting with network:\n  " <> show e

logsSubdir :: FilePath -> FilePath
logsSubdir = (<> "/log")

unlockedAddress :: IsString s => s
unlockedAddress = "0xabc6bBD0aD6aca2D25380FC7835fe088E7690c2C"

-- bunch of solana keypairs, generated with:
--  solana-keygen new --no-passphrase -fso
solanaFaucetKeypair :: T.Text
solanaFaucetKeypair = "[71,246,227,15,183,154,72,252,69,32,15,111,223,164,103,186,79,159,115,61,174,120,204,134,145,174,44,24,80,226,175,207,29,4,251,191,17,108,250,213,190,67,179,253,188,118,62,224,190,227,157,203,228,244,95,161,40,123,58,106,229,156,161,86]"
solanaBootstrapValidatorIdentityKeypair :: T.Text
solanaBootstrapValidatorIdentityKeypair = "[192,159,180,229,68,70,233,70,83,76,150,77,185,87,166,222,88,179,154,0,158,179,128,238,167,148,135,86,191,109,94,98,211,82,244,11,26,46,138,30,212,209,111,171,170,186,133,40,144,233,101,93,215,121,42,110,169,165,224,69,186,192,120,10]"
voteAccountKeypair :: T.Text
voteAccountKeypair = "[183,84,232,40,36,248,21,231,135,120,104,233,237,92,143,38,177,127,63,199,44,101,82,126,213,199,20,227,73,253,234,87,160,1,85,103,20,207,0,131,28,53,240,217,131,246,147,9,136,69,122,225,14,34,195,97,242,39,224,85,152,209,183,249]"
stakeAccountKeypair :: T.Text
stakeAccountKeypair = "[55,217,78,20,228,230,230,89,66,11,131,181,64,47,247,36,11,78,76,54,43,57,160,189,228,203,8,66,0,233,135,3,159,165,84,42,226,126,129,204,24,141,148,117,233,154,29,94,204,98,176,43,7,76,26,23,146,121,196,145,159,152,8,111]"

do
  let x = (defaultOptions { fieldLabelModifier = dropWhile ('_' ==) . dropWhile ('_' /=) . dropWhile ('_' ==) })
  concat <$> traverse (deriveJSON x)
    [ ''ContractConfig
    , ''Eth.Provider
    ]
