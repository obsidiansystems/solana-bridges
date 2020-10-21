{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Solana.Relayer where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (withAsync)
import           Control.Exception (SomeException, handle)
import           Control.Monad (unless)
import           Control.Monad.Catch (catch)
import           Control.Monad.Fix (fix)
import           Control.Monad.Except (liftIO, runExceptT, throwError)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bool (bool)
import           Data.ByteArray.HexString
import qualified Data.ByteString as BS
import           Data.Default (def)
import           Data.Foldable (fold, for_)
import           Data.Functor (void)
import           Data.List (intercalate)
import           Data.Solidity.Prim.Address (Address)
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Network.Web3.Provider (runWeb3, runWeb3')
import qualified Network.Web3.Provider as Eth
import           Network.JsonRpc.TinyClient as Eth
import qualified Network.Ethereum.Account as Eth
import qualified Network.Ethereum.Api.Eth as Eth (blockNumber, getBalance, getTransactionReceipt, sendTransaction)
import qualified Network.Ethereum.Api.Debug as Eth
import qualified Network.Ethereum.Api.Types as Eth (Call(..), DefaultBlock(..), TxReceipt(..))
import           Network.Ethereum.Api.Types (Call(..))
import qualified Network.Ethereum.Unit as Eth
import           System.Directory (canonicalizePath, createDirectory, getCurrentDirectory, removeFile, createDirectoryIfMissing)
import           System.IO (IOMode(ReadMode, WriteMode), openFile)
import           System.IO.Error (isAlreadyExistsError, isDoesNotExistError)
import           System.IO.Temp (createTempDirectory)
import           System.Posix.Files (createSymbolicLink)
import           System.Process (CreateProcess(..), StdStream(..), callCommand, createProcess, spawnProcess
                                , proc, readProcess, readCreateProcessWithExitCode, waitForProcess)
import           System.Which (staticWhich)
import           System.Environment
import           System.Exit
import           Data.Word
import Data.Maybe(fromMaybe)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as Base64
import qualified Blockchain.Data.RLP as RLP
import qualified Data.ByteString.Lazy as LBS
import qualified System.Process.ByteString.Lazy
import Control.Lens
import Data.Aeson.Lens (_String, key, nth)
import Data.Binary.Get

import qualified Ethereum.Contracts as Contracts

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
      putStrLn "USAGE: solana-bridges CONFIGFILE.json"

mainEthTestnet :: IO ()
mainEthTestnet = do
  currentDir <- getCurrentDirectory
  runDir <- canonicalizePath =<< createTempDirectory currentDir ".run"

  setupEth currentDir runDir
  runEthereum def runDir

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

  _faucet <- spawnProcess solanaFaucetPath
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
  _ <- waitForProcess validator
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
  _ <- openFile "/dev/null" ReadMode
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
      bimap (view _3) (view _3) $ runGetOrFail ((,,) <$> getWord64le <*> getWord64le <*> getWord8) $ LBS.fromStrict x2

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


runEthereum :: Eth.Provider -> FilePath -> IO ()
runEthereum node runDir = withGeth runDir $ do
  let
    runWeb3'' = runWeb3' node

    invokeContract :: Address
                   -> Eth.DefaultAccount Eth.Web3 a
                   -> Eth.Web3 a
    invokeContract a = Eth.withAccount ()
                       . Eth.withParam (Eth.to .~ a)
                       . Eth.withParam (Eth.gasLimit .~ 1e6)
                       . Eth.withParam (Eth.gasPrice .~ (1 :: Eth.Wei))

    printCurrentBalance = do
      balance <- runWeb3'' (Eth.getBalance unlockedAddress Eth.Latest) >>= \case
        Left err -> throwError $ "Balance query failed: " <> show err
        Right balance -> pure balance
      liftIO $ putStrLn $ intercalate " " [ "Balance for", unlockedAddress, "is", show balance]

    printCurrentBlockNumber = do
      height <- runWeb3'' Eth.blockNumber >>= \case
        Left err -> throwError $ "Block number query failed: " <> show err
        Right height -> pure height
      liftIO $ putStrLn $ "Current block number is " <> show height

    deployContract path = do
      bin <- liftIO $ BS.readFile path
      liftIO $ putStrLn "Creating contract"
      runWeb3'' (Eth.sendTransaction $ createContract unlockedAddress $ either error id . hexString $ bin) >>= \case
        Left err -> throwError $ "Transaction failed: " <> show err
        Right tx -> do
          liftIO $ putStrLn $ "Submitted contract in transaction " <> T.unpack (toText tx)
          pure tx

    getContractAddress receipt = case Eth.receiptContractAddress receipt of
      Nothing -> throwError $ "Contract address not found"
      Just ca -> pure ca

    waitForTx tx = do
      liftIO $ putStrLn "Waiting for transaction to be committed"
      fix $ \go -> do
        runWeb3'' (Eth.getTransactionReceipt tx) >>= \case
          Left err -> throwError $ "getTransactionReceipt error: " <> show err
          Right (Just r) -> pure r
          Right Nothing -> do
            liftIO $ threadDelay 1e6
            go

    simulate ca name x = do
      let qname = "'" <> name <> "'"
      liftIO $ putStr $ "Invoking " <> qname <> " ...... "
      runWeb3'' (invokeContract ca x) >>= \case
        Left err -> throwError $ "Failed " <> qname <> ": " <> show err
        Right r -> do
          liftIO $ putStrLn $ show r
          pure r

    submit ca name x = do
      let qname = "'" <> name <> "'"
      liftIO $ putStr $ "Submitting " <> qname <> " ...... "
      runWeb3'' (invokeContract ca x) >>= \case
        Left err -> throwError $ "Failed " <> qname <> ": " <> show err
        Right r -> liftIO $ putStrLn $ bool "Success" "Failed" $ null $ Eth.receiptLogs r

    getEpoch ca = simulate ca "epoch" Contracts.epoch
    getLastSlot ca = simulate ca "lastSlot" Contracts.lastSlot
    getLastHash ca = simulate ca "lastHash" Contracts.lastHash
    getSeenBlocks ca = simulate ca "seenBlocks" Contracts.seenBlocks
    getSlotLeader ca s = simulate ca "getSlotLeader" $ Contracts.getSlotLeader s

    setEpoch ca e = submit ca "setEpoch" $ Contracts.setEpoch e [111, 222, 333] [2, 1, 0]
    addBlock ca slot hash parentSlot parentHash = void $ submit ca "addBlock" $ Contracts.addBlock slot hash parentSlot parentHash

  res <- runExceptT $ do
    printCurrentBalance

    tx <- deployContract "solidity/dist/SolanaClient.bin"

    receipt <- waitForTx tx
    ca <- getContractAddress receipt

    printCurrentBlockNumber

    liftIO $ putStrLn $ "Contract address: " <> show ca

    _ <- setEpoch ca 123
    addBlock ca 11 22 33 44
    addBlock ca 12 23 11 22

    printCurrentBlockNumber

    void $ getEpoch ca
    void $ getLastSlot ca
    void $ getLastHash ca
    void $ getSeenBlocks ca
    for_ [0,1,2] $ getSlotLeader ca

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

  putStrLn "Importing account"
  callCommand $ "cp " <> "ethereum/" <> accountFile <> " " <> runDir <> "/.ethereum/keystore"

  putStrLn "Launching node"
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
      putStrLn "Waiting a few seconds for node to launch"
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

deriveJSON (defaultOptions { fieldLabelModifier = dropWhile ('_' ==) . dropWhile ('_' /=) . dropWhile ('_' ==) })
  ''ContractConfig
