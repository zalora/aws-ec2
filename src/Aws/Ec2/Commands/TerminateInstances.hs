{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           , LambdaCase
           #-}

module Aws.Ec2.Commands.TerminateInstances where

import Data.Aeson (Value (..), FromJSON, (.:), parseJSON)
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import Data.ByteString.Char8 (pack)
import qualified Network.HTTP.Types as HTTP
import Data.Monoid
import Aws.Ec2.TH

-- http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-TerminateInstances.html
data TerminateInstances = TerminateInstances
                  { term_instanceIds :: [Text]
                  } deriving (Show)

instance SignQuery TerminateInstances where
    type ServiceConfiguration TerminateInstances = EC2Configuration
    signQuery TerminateInstances{..} = ec2SignQuery $
                                  main
                                  +++ enumerateInstances (zip [1..] term_instanceIds)
        where
          main :: HTTP.Query
          main = [ ("Action", qArg "TerminateInstances")
                 , defVersion
                 ]
          enumerateInstances :: [(Int,Text)] -> HTTP.Query
          enumerateInstances [] = []
          enumerateInstances ((i,x):xs) = [("InstanceId." <> pack (show i), qArg x)] +++ enumerateInstances xs


ec2ValueTransaction ''TerminateInstances "TerminateInstancesResponse"

data TerminateInstancesResponse =
  TerminateInstancesResponse { tirRequestId :: Text
                             , tirInstancesSet :: [InstanceStateChange]}

instance FromJSON TerminateInstancesResponse where
  parseJSON (Object v) = TerminateInstancesResponse <$>
    v .: "requestId" <*>
    v .: "instancesSet"
  parseJSON invalid = typeMismatch "TerminateInstancesResponse" invalid

data InstanceStateChange =
  InstanceStateChange { iscInstanceId    :: Text
                      , iscCurrentState  :: InstanceState
                      , iscPreviousState :: InstanceState}

instance FromJSON InstanceStateChange where
  parseJSON (Object v) = InstanceStateChange <$>
    v .: "instanceId" <*>
    v .: "currentState" <*>
    v .: "previousState"
  parseJSON invalid = typeMismatch "InstanceStateChange" invalid
