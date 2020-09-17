{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (withAsync)
import           Control.Exception (SomeException, handle)
import           Control.Monad (unless)
import           Data.ByteArray.HexString
import qualified Data.ByteString as BS
import           Data.Foldable (fold)
import           Data.Function ((&))
import           Data.Functor (void)
import           Data.List (intercalate)
import           Data.Solidity.Prim.Address (Address)
import           Data.String (IsString)
import qualified Data.Text as T
import           Network.Web3 (runWeb3)
import qualified Network.Ethereum.Api.Eth as Eth
import qualified Network.Ethereum.Api.Types as Eth
import           Network.Ethereum.Api.Types (Call(..))
import           System.Directory (canonicalizePath, createDirectory, getCurrentDirectory, removeFile)
import           System.IO (IOMode(ReadMode, WriteMode), openFile)
import           System.IO.Error (isAlreadyExistsError, isDoesNotExistError)
import           System.IO.Temp (createTempDirectory)
import           System.Posix.Files (createSymbolicLink)
import           System.Process (CreateProcess(std_err, std_out), StdStream(..), callCommand, createProcess
                                , proc, readProcess, readCreateProcessWithExitCode, waitForProcess)
import           System.Which (staticWhich)

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  runDir <- canonicalizePath =<< createTempDirectory currentDir ".run"
  setup currentDir runDir
  runEthereum runDir

setup :: FilePath -> FilePath -> IO ()
setup currentDir runDir = do
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
createContract addr hex = Eth.Call
    { callFrom = Just addr
    , callTo = Nothing
    , callGas = Just 1e5
    , callGasPrice = Just 1
    , callValue = Just 1
    , callData = Just hex
    , callNonce = Nothing
    }

runEthereum :: FilePath -> IO ()
runEthereum runDir = withGeth runDir $ do
  let contract = "solidity/helloWorld.sol"
  putStrLn $ "Compiling " <> contract

  h <- openFile "/dev/null" ReadMode
  let p = (proc solcPath ["--bin", contract, "-o", "solidity", "--overwrite" ])
        { std_out = UseHandle h
        , std_err = UseHandle h
        }
  void $ readCreateProcessWithExitCode p ""
  bin <- BS.readFile "solidity/HelloWorld.bin"

  runWeb3 (Eth.getBalance unlockedAddress Eth.Latest) >>= \case
    Left err -> putStrLn $ "Balance query failed: " <> show err
    Right balance -> putStrLn $ intercalate " " [ "Balance for", unlockedAddress, "is", show balance]
  putStrLn "Sending transaction"
  runWeb3 (Eth.sendTransaction $ createContract unlockedAddress $ fromBytes bin) >>= \case
    Left err -> putStrLn $ "Transaction failed: " <> show err
    Right hex -> do
      putStrLn $ "Transaction succeeded: hash is " <> T.unpack (toText hex)
      threadDelay 5e6
      runWeb3 (Eth.getTransactionReceipt hex) >>= \case
        Left err -> putStrLn $ "Query failed: " <> show err
        Right tr -> putStrLn $ "Transaction receipt:\n  " <> show tr

  putStrLn "All done - network can be stopped now"


solcPath :: FilePath
solcPath = $(staticWhich "solc")

gethPath :: FilePath
gethPath = $(staticWhich "geth")

genesisPath :: FilePath
genesisPath = "ethereum/Genesis.json"

accountFile :: String
accountFile = "UTC--2020-09-17T02-34-16.613Z--0xabc6bbd0ad6aca2d25380fc7835fe088e7690c2c"

runGeth ::  FilePath -> IO ()
runGeth runDir = do
  let
    dataDirArgs = [ "--datadir", runDir <> "/.ethereum"]
    httpArgs = [ "--rpc" ]
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
