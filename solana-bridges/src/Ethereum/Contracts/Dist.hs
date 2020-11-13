{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Ethereum.Contracts.Dist where

import qualified Data.ByteString as BS
import Ethereum.Contracts.TH

solanaClientContractAbi :: String
solanaClientContractBin :: BS.ByteString
(solanaClientContractAbi, solanaClientContractBin) = $(compileSolidity)
