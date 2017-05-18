{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           #-}

module Aws.Ec2.Commands.DescribeInstanceStatus where

import Data.Aeson (Value(..), FromJSON, parseJSON, (.:), (.:?))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T

import Aws.Ec2.TH
import Aws.Ec2.Types

data DescribeInstanceStatus = DescribeInstanceStatus { dis_instanceIds :: [Text] }
                       deriving (Show)

ec2ValueTransactionDef ''DescribeInstanceStatus 'DescribeInstanceStatus "instanceStatusSet" "InstanceId"


data InstanceStatusDetailName = Reachability deriving Eq

instance Show InstanceStatusDetailName where
  show Reachability = "reachability"

instance Read InstanceStatusDetailName where
  readsPrec _ h =
    case h of
      "reachability" -> [(Reachability, "")]
      _ -> fail $ "Failed to parse " ++ h ++ " into InstanceStatusDetailName."


data InstanceStatusDetailStatus = Passed
                                | Failed
                                | DInsufficientData
                                | DInitializing
                                deriving Eq

instance Show InstanceStatusDetailStatus where
  show Passed = "passed"
  show Failed = "failed"
  show DInsufficientData = "insufficient-data"
  show DInitializing = "initializing"

instance Read InstanceStatusDetailStatus where
  readsPrec _ h =
    case h of
      "passed" -> [(Passed, "")]
      "failed" -> [(Failed, "")]
      "insufficient-data" -> [(DInsufficientData, "")]
      "initializing" -> [(DInitializing, "")]
      _ -> fail $ "Failed to parse " ++ h ++ " into InstanceStatusDetailStatus."


data InstanceStatusDetails =
  InstanceStatusDetails { isdName :: InstanceStatusDetailName
                        , isdStatus :: InstanceStatusDetailStatus
                        } deriving Show

instance FromJSON InstanceStatusDetails where
  parseJSON (Object v) = InstanceStatusDetails <$>
    (read <$> (v .: "name")) <*>
    (read <$> (v .: "status"))
  parseJSON invalid = typeMismatch "InstanceState" invalid


data InstanceStatusStatus = Ok
                          | Impaired
                          | InsufficientData
                          | NotApplicable
                          | Initializing
                          deriving Eq

instance Show InstanceStatusStatus where
  show Ok = "ok"
  show Impaired = "impaired"
  show InsufficientData = "insufficient-data"
  show NotApplicable = "not-applicable"
  show Initializing = "initializing"

instance Read InstanceStatusStatus where
  readsPrec _ h =
    case h of
      "ok" -> [(Ok, "")]
      "impaired" -> [(Impaired, "")]
      "insufficient-data" -> [(InsufficientData, "")]
      "not-applicable" -> [(NotApplicable, "")]
      "initializing" -> [(Initializing, "")]
      _ -> fail $ "Failed to parse " ++ h ++ " into InstanceStatusStatus."

data InstanceStatusSummary =
  InstanceStatusSummary { siStatus :: InstanceStatusStatus
                        , siDetails :: [InstanceStatusDetails]
                        } deriving Show

instance FromJSON InstanceStatusSummary where
  parseJSON (Object v) = InstanceStatusSummary <$>
    (read <$> (v .: "status")) <*>
    v .: "details"
  parseJSON invalid = typeMismatch "InstanceStatusSummary" invalid

data InstanceStatus =
  InstanceStatus { isInstanceId :: T.Text
                 , isAvailabilityZone :: T.Text
                 , isInstanceStatus :: Maybe InstanceStatusSummary
                 , isSystemStatus :: Maybe InstanceStatusSummary
                 , isState :: Maybe InstanceState
                 } deriving Show

instance FromJSON InstanceStatus where
  parseJSON (Object v) = InstanceStatus <$>
    v .: "instanceId" <*>
    v .: "availabilityZone" <*>
    v .:? "instanceStatus" <*>
    v .:? "systemStatus" <*>
    v .:? "instanceState"
  parseJSON invalid = typeMismatch "InstanceStatusSummary" invalid

newtype DescribeInstanceStatusResponse =
  DescribeInstanceStatusResponse {disrInstanceStatuses :: [InstanceStatus]} deriving (Show)

instance FromJSON DescribeInstanceStatusResponse where
  parseJSON v = DescribeInstanceStatusResponse <$>
    (maybeToList <$> parseJSON v)
