{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Ethereum.Contracts.Bindings where

import Network.Ethereum.Contract.TH (quoteAbiDec)

import Ethereum.Contracts.Dist

$(quoteAbiDec solanaClientContractAbi)
