{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Solana.Relayer where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Exception (SomeException, handle)
import Control.Lens
import Control.Monad
import Control.Monad.Catch (catch)
import Control.Monad.Catch (finally)
import Control.Monad.Except (runExceptT, throwError, ExceptT(..))
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.Aeson
import Data.Aeson.Lens (_String, key, nth)
import Data.Aeson.TH
import Data.ByteArray.HexString
import Data.Default (def)
import Data.FileEmbed (embedFile)
import Data.Foldable
import Data.Functor.Compose
import Data.List (intercalate)
import Data.Maybe(fromMaybe)
import Data.Solidity.Prim.Address (Address)
import Data.String (IsString)
import Data.Text (Text)
import Data.Word
import Network.Ethereum.Api.Types (Call(..))
import Network.JsonRpc.TinyClient as Eth
import Network.URI (URI(..), uriToString, parseURI)
import Network.Web3.Provider (runWeb3, runWeb3')
import System.Directory (canonicalizePath, createDirectory, getCurrentDirectory, removeFile, createDirectoryIfMissing)
import System.Environment
import System.Exit
import System.IO (IOMode(..), openFile, stderr, hPutStrLn)
import System.IO.Error (isAlreadyExistsError, isDoesNotExistError)
import System.IO.Temp (createTempDirectory)
import System.Posix.Files (createSymbolicLink)
import System.Process (CreateProcess(..), StdStream(..), callCommand, createProcess, spawnProcess , proc, readProcess, readCreateProcessWithExitCode, waitForProcess, terminateProcess)
import System.Which (staticWhich)
import qualified Blockchain.Data.RLP as RLP
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Network.Ethereum.Api.Debug as Eth
import qualified Network.Ethereum.Api.Eth as Eth (getTransactionReceipt, sendTransaction)
import qualified Network.Ethereum.Api.Types as Eth (Call(..), TxReceipt(..))
import qualified Network.Web3.Provider as Eth
import qualified System.Process.ByteString.Lazy

import Ethereum.Contracts as Contracts
import Solana.RPC
import Solana.Types


satsub :: Word64 -> Word64 -> Word64
satsub x y
  | x <= y = 0
  | otherwise = x - y

mainRelayer :: IO ()
mainRelayer = do
  getArgs >>= \case
    configFile:[] -> do
      configData <- BS.readFile configFile
      config :: ContractConfig <- case eitherDecodeStrict' configData of
          Right c -> pure c
          Left e -> fail $ show e

      runRelayer configFile config

    _ -> do
      progName <- getProgName
      hPutStrLn stderr $ "USAGE: " <> progName <> " CONFIGFILE.json"

type SolanaToEthereumConfig = (Eth.Provider, Address)

mainRelayerEth :: IO ()
mainRelayerEth = do
  getArgs >>= \case
    configFile:[] -> do
      configData <- BS.readFile configFile
      (node, address) :: SolanaToEthereumConfig <- case eitherDecodeStrict' configData of
          Right c -> pure c
          Left e -> fail $ show e

      runRelayerEth node address

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

mainDeploySolanaToEthereumContract :: IO ()
mainDeploySolanaToEthereumContract = do
  mProvider <- getArgs <&> \case
    [] -> Right def
    url:[] -> case parseURI url of
      Just url' -> uriToProvider url'
      Nothing -> Left "invalid uri"
    _ -> Left ""

    -- url:[] -> do

  case mProvider of
    Left err -> do
      progName <- getProgName
      T.hPutStrLn stderr $ T.unlines
        ["USAGE: " <> T.pack progName <> " [PROVIDER]"
        , "\tPROVIDER\tethereum provider url"
        , "\t\thttp://host[:port]"
        -- , "\t\tws://host:port"
        , err
        ]
    Right provider -> do
      ca <- deploySolanaRelayContract provider
      let config :: SolanaToEthereumConfig = (provider, ca)

      LBS.putStr $ encode config
      putStrLn ""

mainEthTestnet :: IO ()
mainEthTestnet = do
  currentDir <- getCurrentDirectory
  runDir <- canonicalizePath =<< createTempDirectory currentDir ".run"

  setupEth currentDir runDir
  withGeth runDir $ forever $ threadDelay 1e6

deployAndRunSolanaRelayer :: IO ()
deployAndRunSolanaRelayer = do
  ca <- deploySolanaRelayContract def
  runRelayerEth def ca

mainSolanaTestnet :: IO ()
mainSolanaTestnet = do
  currentDir <- getCurrentDirectory
  runDir <- canonicalizePath =<< createTempDirectory currentDir ".run"

  solanaSpecialPaths <- SolanaSpecialPaths <$> getEnv "SPL_TOKEN" <*> getEnv "SPL_MEMO"
  setupSolana runDir solanaSpecialPaths


data SolanaSpecialPaths = SolanaSpecialPaths
  { _solanaSpecialPaths_splToken :: !FilePath
  , _solanaSpecialPaths_splMemo :: !FilePath
  } deriving (Eq, Show)

setupSolana :: FilePath -> SolanaSpecialPaths -> IO ()
setupSolana solanaConfigDir solanaSpecialPaths = do
  putStrLn solanaConfigDir
  createDirectoryIfMissing True (solanaConfigDir <> "/bootstrap-validator")

  let
    solanaFaucetKeypairFile = solanaConfigDir <> "/faucet.json"
    bootstrapValidatorIdentity = solanaConfigDir <> "/bootstrap-validator/identity.json"
    voteAccountKeypairFile = solanaConfigDir <> "/bootstrap-validator/vote-account.json"
    stakeAccountKeypairFile = solanaConfigDir <> "/bootstrap-validator/stake-account.json"
    ledgerPath = solanaConfigDir <> "/ledger"

  T.writeFile solanaFaucetKeypairFile solanaFaucetKeypair
  T.writeFile bootstrapValidatorIdentity solanaBootstrapValidatorIdentityKeypair
  T.writeFile voteAccountKeypairFile voteAccountKeypair
  T.writeFile stakeAccountKeypairFile stakeAccountKeypair

  let
    genArgs :: [String]
    genArgs =
      [ "--max-genesis-archive-unpacked-size", "1073741824"
      , "--enable-warmup-epochs"
      , "--bootstrap-validator", bootstrapValidatorIdentity, voteAccountKeypairFile, stakeAccountKeypairFile
      , "--bpf-program", "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA", "BPFLoader1111111111111111111111111111111111", _solanaSpecialPaths_splToken solanaSpecialPaths
      , "--bpf-program", "Memo1UhkJRfHyvLMcVucJwxXeuD728EqVDDwQDxFMNo", "BPFLoader1111111111111111111111111111111111", _solanaSpecialPaths_splMemo solanaSpecialPaths
      , "--ledger", ledgerPath
      , "--faucet-pubkey", solanaFaucetKeypairFile
      , "--faucet-lamports", "500000000000000000"
      , "--hashes-per-tick", "sleep"
      , "--cluster-type", "development"
      ]

  putStrLn $ unwords $ solanaGenesisPath:genArgs

  let p = proc solanaGenesisPath genArgs
  readCreateProcessWithExitCode p "" >>= \case
    good@(ExitSuccess, _, _) -> print good
    bad -> error $ show bad

  faucet <- spawnProcess solanaFaucetPath
    ["--keypair", solanaFaucetKeypairFile]

  let
    bootstrapValidator = (proc solanaValidatorPath
      [ "--ledger", ledgerPath
      , "--rpc-port", "8899"
      , "--identity", bootstrapValidatorIdentity
      , "--vote-account" , voteAccountKeypairFile
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

createContract :: Address -> HexString -> Call
createContract fromAddr hex = Eth.Call
    { callFrom = Just fromAddr
    , callTo = Nothing
    , callGas = Just 1e6
    , callGasPrice = Just 1
    , callValue = Nothing
    , callData = Just hex
    , callNonce = Nothing
    }

runRelayer :: FilePath -> ContractConfig -> IO ()
runRelayer configFile config = do
  let solanaAccountLookupArgs = proc solanaPath $ T.unpack <$>
        [ "account"
        , _contractConfig_accountId config
        , "--output", "json"
        ]

  (highestBlock, nextBlockOffset, isFull) <- System.Process.ByteString.Lazy.readCreateProcessWithExitCode solanaAccountLookupArgs "" >>= \case
    (ExitSuccess, accountData, _) -> either (error . ("bad: " <>) ) pure $ do
      x :: Value <- eitherDecode' accountData
      x1 <- maybe (Left "missing account data") pure $ preview (key "account" . key "data" . nth 0 . _String) x
      () <- maybe (Left "invalid encoding") pure $ preview (key "account" . key "data" . nth 1 . _String . only "base64") x
      x2 <- maybe (Left "failed to decode") pure $ preview _Right $ Base64.decode $ T.encodeUtf8 x1
      bimap (view _3) (view _3) $ Binary.runGetOrFail ((,,) <$> Binary.getWord64le <*> Binary.getWord64le <*> Binary.getWord8) $ LBS.fromStrict x2

    bad -> error $ show bad

  let contractState = case (highestBlock, nextBlockOffset, isFull) of
        (0, 0, 0) -> Nothing
        _ -> Just $ succ highestBlock

  let loopStart = fromMaybe 1 $ _contractConfig_loopStart config
  let loop n = do
        res <- catch
          (runWeb3 $ Eth.getBlockRlp n)
          (\case
              ParsingException msg -> fail msg
              CallException _ -> do
                T.putStrLn $ "No new block, waiting (" <> T.pack (show n) <> ")"
                threadDelay 5e6
                loop n)
        case res of
          Left e -> fail $ show e
          Right rlp -> do
            let blockHeaderHex = blockToHeader rlp
            T.putStrLn $ T.pack (show n) <> ": " <> blockHeaderHex
            let p = (proc solanaBridgeToolPath $ T.unpack <$>
                      [ (if n == loopStart then "initialize" else "new-block")
                      , "--config", T.pack configFile
                      -- , "--payer", "/dev/null"
                      , "--instruction", blockHeaderHex
                      ])
            readCreateProcessWithExitCode p "" >>= \case
              (ExitSuccess, txn, _) -> putStrLn txn
              bad -> error $ show bad
            loop $ n + 1

  loop (fromMaybe loopStart contractState) >>= \case
    Right good -> T.putStrLn good
    Left bad -> error $ show bad

blockToHeader :: Text -> Text
blockToHeader rlp = blockHeaderHex
  where
    (blockData, "") = B16.decode $ T.encodeUtf8 rlp
    RLP.RLPArray (blockHeader:_) = RLP.rlpDeserialize blockData
    blockHeaderHex = T.decodeLatin1 $ B16.encode $ RLP.rlpSerialize blockHeader



deploySolanaRelayContract :: Eth.Provider -> IO Address
deploySolanaRelayContract node = do
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
    ca <- getContractAddress receipt
    liftIO $ hPutStrLn stderr $ "Contract deployed at address: " <> show ca
    pure ca
  case res of
    Left err -> error err
    Right ca -> pure ca

solanaClientContractBin :: BS.ByteString
solanaClientContractBin = $(embedFile "solidity/dist/SolanaClient.bin")

runRelayerEth :: Eth.Provider -> Address -> IO ()
runRelayerEth node ca = do
  res <- runExceptT $ do

    void $ getSeenBlocks node ca

    Right _ <- ExceptT $ withSolanaWebSocket (SolanaRpcConfig "127.0.0.1" 8899 8900) $ do
      liftIO $ T.putStrLn "Connected to solana node"

      epochSchedule <- getEpochSchedule
      let
        epochFromSlot' = epochFromSlot epochSchedule

        loop :: SolanaRpcM IO ()
        loop = do
          -- epochSchedule <- getEpochSchedule

          Right initializedAtStartup <- lift $ runExceptT $ getInitialized node ca

          bootEpochInfo <- getEpochInfo

          contractSlot <- if initializedAtStartup
            then do
              Right contractSlot <- lift $ runExceptT $ getLastSlot node ca
              pure contractSlot
            else do
              liftIO $ putStrLn "Initializing contract"
              -- get the latest confirmed block in
              let bootSlot = _solanaEpochInfo_absoluteSlot bootEpochInfo
              bootConfirmedBlock <- getConfirmedBlocks (satsub bootSlot 128) bootSlot

              let
                slot0 = fromMaybe (error "no block found") $ lastOf traverse bootConfirmedBlock
                epochInfo0 = epochFromSlot' slot0
              Right (Just block0) <- getConfirmedBlock slot0
              Right () <- lift $ runExceptT $ do
                setEpoch node ca (_solanaEpochInfo_epoch epochInfo0) Map.empty
                liftIO $ threadDelay 4e6
                addBlocks node ca [(slot0, block0)]
              pure slot0

          confirmedBlockSlots <- getConfirmedBlocksWithLimit (succ contractSlot) 64 -- (_solanaEpochInfo_absoluteSlot bootEpochInfo)
          confirmedBlocks' <- traverse getConfirmedBlock confirmedBlockSlots
          let Compose (Right (Just confirmedBlocks)) = traverse Compose confirmedBlocks'
              blocksAndSlots = zip confirmedBlockSlots confirmedBlocks

          liftIO $ do
            let rpcSlot = _solanaEpochInfo_absoluteSlot bootEpochInfo
            putStr $ unlines
              [ ""
              , "Solana RPC slot: " <> show rpcSlot
              , "Ethereum contract slot: " <> show contractSlot
              , "Contract is behind by " <> show (rpcSlot - contractSlot)
              ]
          when (not $ null confirmedBlocks) $ do
            Right () <- lift $ runExceptT $ addBlocks node ca blocksAndSlots
            liftIO $ do
              putStrLn $ "Sending new slots: " <> show confirmedBlockSlots
              putStrLn "Submitted new slots to contract"

          when (null confirmedBlocks) $ liftIO $ threadDelay 1e6
          loop

      loop
      pure $ Left "not reachable"
    pure ()

  case res of
    Left err -> putStrLn err
    Right _ -> pure ()

  putStrLn "All done - network can be stopped now"


data ContractConfig = ContractConfig
 { _contractConfig_programId :: Text
 , _contractConfig_accountId :: Text
 , _contractConfig_loopStart :: Maybe Word64
 } deriving (Show, Eq, Ord)

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

genesisPath :: FilePath
genesisPath = "ethereum/Genesis.json"

accountFile :: String
accountFile = "UTC--2020-09-17T02-34-16.613Z--0xabc6bbd0ad6aca2d25380fc7835fe088e7690c2c"

runGeth ::  FilePath -> IO ()
runGeth runDir = do
  let
    dataDirArgs = [ "--datadir", runDir <> "/.ethereum"]
    httpArgs = [ "--http", "--http.api", "eth,net,web3,debug,personal" ]
    mineArgs = [ "--mine", "--miner.threads=1", "--etherbase=0x0000000000000000000000000000000000000001" ]
    initArgs = [ "init", genesisPath]
    privateArgs = [ "--nodiscover" ]
    nodeArgs = [ "--identity", "Testnet ethereum node 0"]
    unlockArgs = [ "--allow-insecure-unlock", "--unlock", unlockedAddress, "--password", "ethereum/pass.txt" ]

  putStr "Initializing node with genesis file: "
  putStrLn =<< canonicalizePath genesisPath
  void $ readProcess gethPath (dataDirArgs <> initArgs) ""

  callCommand $ "cp " <> "ethereum/" <> accountFile <> " " <> runDir <> "/.ethereum/keystore"

  putStrLn "Launching ethereum node"
  h <- openFile (logsSubdir runDir) WriteMode
  let p = proc gethPath $ fold [ dataDirArgs, httpArgs, mineArgs, privateArgs, unlockArgs, nodeArgs ]
  (_,_,_,ph) <- createProcess $ p
    { std_out = UseHandle h
    , std_err = UseHandle h
    }
  void $ waitForProcess ph

withGeth :: FilePath -> IO () -> IO ()
withGeth dir action = do
  withAsync action' $ \_ -> runGeth dir

  where
    action' = printErrors $ do
      threadDelay 1e6
      putStrLn "Waiting for ethereum node to launch"
      threadDelay 3e6
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
