{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           , RecordWildCards
           , DeriveGeneric
           , OverloadedStrings
           #-}

module Aws.Ec2.Commands.GetConsoleOutput where

import Data.Aeson (Value(..), FromJSON, (.:), parseJSON)

import Aws.Ec2.TH
import GHC.Generics

data GetConsoleOutput = GetConsoleOutput { gco_instanceId :: Text }
                       deriving (Show)

data ConsoleOutput = ConsoleOutput
                   { coRequestId :: Text
                   , coInstanceId :: Text
                   , coTimestamp :: UTCTime
                   , coOutput :: Text
                   } deriving (Generic, Show)

instance FromJSON ConsoleOutput where
  parseJSON (Object v) = ConsoleOutput <$>
    v .: "requestId" <*>
    v .: "instanceId" <*>
    v .: "timestamp" <*>
    v .: "output"

instance SignQuery GetConsoleOutput where
    type ServiceConfiguration GetConsoleOutput = EC2Configuration
    signQuery GetConsoleOutput{..} = ec2SignQuery [ ("Action", qArg "GetConsoleOutput")
                                                  , defVersion
                                                  , ("InstanceId", qArg gco_instanceId)
                                                  ]

ec2ValueTransaction ''GetConsoleOutput "GetConsoleOutputResponse"
