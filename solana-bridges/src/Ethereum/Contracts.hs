{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
module Ethereum.Contracts where
import Network.Ethereum.Contract.TH

[abiFrom|src/Ethereum/SolanaClient.abi|]
