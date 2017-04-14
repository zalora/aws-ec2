{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           , LambdaCase
           #-}

module Aws.Ec2.Commands.RunInstances where

import Control.Applicative ((<$>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString.Char8 (pack, ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Network.HTTP.Types as HTTP
import Data.Monoid
import Aws.Ec2.TH

-- http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RunInstances.html
data RunInstances = RunInstances
                  { run_imageId :: Text
                  , run_count :: (Int, Int)
                  , run_instanceType :: Text
                  , run_securityGroupIds :: [Text]
                  , run_blockDeviceMappings :: [BlockDeviceMapping]
                  , run_subnetId :: Maybe Text
                  , run_monitoringEnabled :: Bool
                  , run_disableApiTermination :: Bool
                  , run_instanceInitiatedShutdownBehavior :: Maybe InstanceInitiatedShutdownBehavior
                  , run_ebsOptimized :: Bool

                  , run_keyName :: Maybe Text
                  , run_userData :: Maybe Text
                  , run_kernelId :: Maybe Text
                  , run_ramdiskId :: Maybe Text
                  , run_clientToken :: Maybe Text
                  , run_iamInstanceProfileARN :: Maybe Text
                  , run_availabilityZone :: Maybe Text
                  , run_associatePublicIpAddress :: Bool
                  , run_placementGroup :: Maybe Text
                  -- also missing: NetworkInterface
                  } deriving (Show)

data InstanceInitiatedShutdownBehavior = Stop | Terminate

instance Show InstanceInitiatedShutdownBehavior where
    show Stop = "stop"
    show Terminate = "terminate"

-- http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RunInstances.html
instanceTypes :: [Text]
instanceTypes = [ "t2.micro"
                , "t2.small"
                , "t2.medium"
                , "m3.medium"
                , "m3.large"
                , "m3.xlarge"
                , "m3.2xlarge"
                , "m1.small" -- default
                , "m1.medium"
                , "m1.large"
                , "m1.xlarge"
                , "c3.large"
                , "c3.xlarge"
                , "c3.2xlarge"
                , "c3.4xlarge"
                , "c3.8xlarge"
                , "c1.medium"
                , "c1.xlarge"
                , "cc2.8xlarge"
                , "r3.large"
                , "r3.xlarge"
                , "r3.2xlarge"
                , "r3.4xlarge"
                , "r3.8xlarge"
                , "m2.xlarge"
                , "m2.2xlarge"
                , "m2.4xlarge"
                , "cr1.8xlarge"
                , "i2.xlarge"
                , "i2.2xlarge"
                , "i2.4xlarge"
                , "i2.8xlarge"
                , "hs1.8xlarge"
                , "hi1.4xlarge"
                , "t1.micro"
                , "g2.2xlarge"
                , "cg1.4xlarge"
                ]

enumerateBlockDevices :: [BlockDeviceMapping] -> HTTP.Query
enumerateBlockDevices = enumerateLists "BlockDeviceMapping." . fmap unroll
  where
    unroll BlockDeviceMapping{..} = [ ("DeviceName", qArg bdm_deviceName)
                                    ] +++ case bdm_device of
                                            Ephemeral{..} -> [("VirtualName", qArg bdm_virtualName)]
                                            EBS ebs -> queryEbsBlockDevice ebs

instance SignQuery RunInstances where
    type ServiceConfiguration RunInstances = EC2Configuration
    signQuery RunInstances{..} = ec2SignQuery $
                                  main
                                  +++ (optionalA "KeyName" run_keyName)
                                  +++ (optionalA "UserData" (decodeUtf8 . Base64.encode . encodeUtf8 <$> run_userData))
                                  +++ (optionalA "KernelId" run_kernelId)
                                  +++ (optionalA "RamdiskId" run_ramdiskId)
                                  +++ (optionalA "ClientToken" run_clientToken)
                                  +++ (optionalA "Placement.AvailabilityZone" run_availabilityZone)
                                  +++ (optionalA "Placement.GroupName" run_placementGroup)
                                  +++ enumerateBlockDevices run_blockDeviceMappings
        where
          main :: HTTP.Query
          main = [ ("Action", qArg "RunInstances")
                 , defVersion
                 , ("ImageId", qArg run_imageId)
                 , ("MinCount", qShow $ fst run_count)
                 , ("MaxCount", qShow $ snd run_count)
                 , ("InstanceType", qArg run_instanceType)
                 , ("Monitoring.Enabled", qShow run_monitoringEnabled)
                 , ("DisableApiTermination", qShow run_disableApiTermination)
                 , ("EbsOptimized", qShow run_ebsOptimized)
                 ] +++ optionalA "IamInstanceProfile.Arn" run_iamInstanceProfileARN
                   +++ optional "InstanceInitiatedShutdownBehavior" run_instanceInitiatedShutdownBehavior qShow
                   +++ case run_subnetId of
                         Nothing -> enumerate "SecurityGroupId" run_securityGroupIds qArg
                         Just subnetId -> [ ("NetworkInterface.0.DeviceIndex", qShow 0)
                                          , ("NetworkInterface.0.SubnetId", qArg subnetId)
                                          , ("NetworkInterface.0.AssociatePublicIpAddress", qShow run_associatePublicIpAddress)
                                          ] +++ enumerate "NetworkInterface.0.SecurityGroupId" run_securityGroupIds qArg

ec2ValueTransaction ''RunInstances "RunInstancesResponse"

type RunInstancesResponse = Reservation
