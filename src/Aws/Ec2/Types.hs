{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , RecordWildCards
           , LambdaCase
           , OverloadedStrings
           #-}

module Aws.Ec2.Types where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Monoid hiding (All)
import Data.ByteString.Char8 (ByteString, pack)

import Network.HTTP.Types as HTTP

import Aws.Query
import Aws.Ec2.Core

data InstanceTenancy = Default | Dedicated

instance Show InstanceTenancy where
    show Default = "default"
    show Dedicated = "dedicated"

data VolumeType = Standard | GP2SSD | IOPSSD Int

instance Show VolumeType where
    show Standard = "standard"
    show GP2SSD = "gp2"
    show (IOPSSD _) = "io1"

data BlockDeviceMapping = BlockDeviceMapping
                                { bdm_deviceName :: Text
                                , bdm_device :: BlockDevice
                                } deriving (Show)

data BlockDevice = Ephemeral {bdm_virtualName :: Text}
                 | EBS EbsBlockDevice
                 deriving (Show)

data EbsBlockDevice = EbsBlockDevice
                    { ebd_snapshotId :: Maybe Text
                    , ebd_deleteOnTermination :: Bool
                    , ebd_volumeType :: VolumeType
                    , ebd_volumeSize :: Int
                    , ebd_encrypted :: Bool
                    } deriving (Show)

queryEbsBlockDevice EbsBlockDevice{..} = [ ("VolumeType", qShow ebd_volumeType)
                                         -- , ("VolumeSize", qShow ebd_volumeSize)
                                         , ("Size", qShow ebd_volumeSize) -- RunInstances: VolumeSize
                                         -- , ("DeleteOnTermination", qShow ebd_deleteOnTermination) -- RunInstances only
                                         , ("Encrypted", qShow ebd_encrypted)
                                         ] +++ optionalA "SnapshotId" ebd_snapshotId
                                           +++ case ebd_volumeType of
                                                 IOPSSD iops -> [("Iops", qShow iops)]
                                                 _ -> []


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

data IpProtocol = TCP | UDP | ICMP | Proto Int | All

instance Show IpProtocol where
    show TCP = "tcp" -- 6
    show UDP = "udp" -- 17
    show ICMP = "icmp" -- 1
    show (Proto i) = show i
    show All = "-1"

type CidrIp = Text
type SgGroupId = Text

data SgPermission = IpPermission IpProtocol (Maybe Int) (Maybe Int) [CidrIp]
                  | SgPermission IpProtocol (Maybe Int) (Maybe Int) [SgGroupId]
                deriving (Show)

enumeratePermissions :: [SgPermission] -> HTTP.Query
enumeratePermissions = enumerateLists "IpPermissions." . fmap unroll
  where
    port n = maybe [] (\p -> [(n, qShow p)])
    unroll (IpPermission proto from to ips) =
      [("IpProtocol", qShow proto)] ++
      port "FromPort" from ++
      port "ToPort" to +++
      [(mconcat [k, ".CidrIp"], v)| (k, v) <- enumerate "IpRanges" ips qArg]
    unroll (SgPermission proto from to sgs) =
      [("IpProtocol", qShow proto)] ++
      port "FromPort" from ++
      port "ToPort" to +++
      [(mconcat [k, ".GroupId"], v)| (k, v) <- enumerate "Groups" sgs qArg]
