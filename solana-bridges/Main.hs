{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (withAsync)
import           Control.Exception (handle)
import           Control.Monad (unless)
import           Data.Foldable (fold)
import           Data.Function ((&))
import           Data.Functor (void)
import           Network.Web3 (runWeb3)
import qualified Network.Ethereum.Api.Eth as Eth
import           System.Directory (canonicalizePath, createDirectory, getCurrentDirectory, removeFile)
import           System.IO (IOMode(WriteMode), openFile)
import           System.IO.Error (isAlreadyExistsError, isDoesNotExistError)
import           System.IO.Temp (createTempDirectory)
import           System.Posix.Files (createSymbolicLink)
import           System.Process (CreateProcess(std_err, std_out), StdStream(..), createProcess, proc, readProcess)
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

runEthereum :: FilePath -> IO ()
runEthereum runDir = withGeth runDir $ do
  print =<< runWeb3 Eth.coinbase
  putStrLn "Done with queries"
  threadDelay 10e6
  putStrLn "Exiting"

gethPath :: FilePath
gethPath = $(staticWhich "geth")

genesisPath :: FilePath
genesisPath = "ethereum/Genesis.json"

runGeth ::  FilePath -> IO ()
runGeth runDir = do
  let
    dataDirArgs = [ "--datadir", runDir <> "/.ethereum"]
    httpArgs = [ "--http" ]
    mineArgs = [ "--mine", "--miner.threads=1", "--etherbase=0x0000000000000000000000000000000000000001" ]
    initArgs = [ "init", genesisPath]
    privateArgs = [ "--nodiscover" ]
    nodeArgs = [ "--identity", "Testnet ethereum node 0"]

  putStrLn "Initializing node"
  void $ readProcess gethPath (dataDirArgs <> initArgs) ""
  h <- openFile (logsSubdir runDir) WriteMode

  putStrLn "Launching node"
  let p = proc gethPath $ fold [ dataDirArgs, httpArgs, mineArgs, privateArgs, nodeArgs ]
  void $ createProcess $ p
    { std_out = UseHandle h
    , std_err = UseHandle h
    }

withGeth :: FilePath -> IO () -> IO ()
withGeth dir action = do
  withAsync (runGeth dir) $ \_ -> do
    threadDelay 1e6
    putStrLn "Waiting for a few seconds for node to launch"
    threadDelay 3e6
    action

logsSubdir :: FilePath -> FilePath
logsSubdir = (<> "/log")
