{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , RecordWildCards
           , LambdaCase
           , OverloadedStrings
           #-}

-- | This module implements a number of response types for the aws-ec2
-- library. All commands from the library are written to return a JSON
-- 'Value', but we provide more specific types for some responses
-- where possible. Implementing and especially testing response types
-- is a cumbersome job because the AWS documentation is not accurate
-- and response types might be missing in various ways (absent, there
-- but empty, sometimes there). Although the ideal would be to be
-- feature complete, a lot of trial and error is needed to get the
-- response types right so we've not yet done this.
--
-- However, it's useful to have more specific return types in common
-- cases, even without full coverage.  Since decoding of return values
-- is type-driven, if you need more specific response decoding or
-- coverage of features not implemented here, just define a new
-- response type and use that.
module Aws.Ec2.Types where

import Control.Applicative
import Data.Aeson (Value (..), FromJSON, (.:), (.:?), (.!=), parseJSON)
import Data.Aeson.Types (typeMismatch)
import Data.Char (toUpper, toLower)
import Data.IP (IPv4)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Monoid hiding (All)
import Data.ByteString.Char8 (ByteString, pack)

import Network.HTTP.Types as HTTP

import Aws.Query
import Aws.Ec2.Core


-- EC2 Data Types

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


type CidrIp = Text
type SgGroupId = Text

data SgPermission = IpPermission IpProtocol (Maybe Int) (Maybe Int) [CidrIp]
                  | SgPermission IpProtocol (Maybe Int) (Maybe Int) [SgGroupId]
                deriving (Show)


data Region = Region { regionEndpoint :: T.Text
                     , regionName :: T.Text}
                     deriving (Show)

instance FromJSON Region where
  parseJSON (Object v) = Region <$>
    v .: "regionEndpoint" <*>
    v .: "regionName"
  parseJSON invalid = typeMismatch "Region" invalid

data Reservation = Reservation { reservationId :: T.Text
                               , reservationOwnerId :: T.Text
                               , reservationSecurityGroups :: [Group]
                               , reservationInstances :: [Instance]}
                               deriving (Show)

instance FromJSON Reservation where
  parseJSON (Object v) = Reservation <$>
    v .: "reservationId" <*>
    v .: "ownerId" <*>
    (maybeToList <$> (v .:? "groupSet" .!= Nothing)) <*>
    v .: "instancesSet"
  parseJSON invalid = typeMismatch "Reservation" invalid

data Group = Group { groupId :: T.Text
                   , groupName :: T.Text}
                   deriving (Show)

instance FromJSON Group where
  parseJSON (Object v) = Group <$>
    v .: "groupId" <*>
    v .: "groupName"
  parseJSON invalid = typeMismatch "Group" invalid

-- | The respose type describing an instance.
--
-- Note: this response type is not complete (according to the docs) but only
-- contains the types we have encountered. The other types that could be present
-- are commented out, as they could not be tested.
data Instance = Instance { instanceId :: T.Text
                         , instanceImageId :: T.Text
                         , instanceState :: InstanceState
                         , instancePrivateDnsName :: Maybe T.Text
                         , instancePublicDnsName :: Maybe T.Text
                         , instanceStateTransitionReason :: Maybe T.Text
                         , instanceKeyName :: Maybe T.Text
                         , instanceAmiLaunchIndex :: Int
                         , instanceProductCodes :: [ProductCode]
                         , instanceType :: T.Text -- InstanceType = ...
                         , instanceLaunchTime :: UTCTime
                         , instancePlacement :: Placement
                         , instanceMonitoring :: Monitoring
                         , instanceSecurityGroups :: [Group]
                         , instanceArchitecture :: Architecture
                         , instanceRootDeviceType :: RootDeviceType
                         , instanceRootDeviceName :: Maybe T.Text
                         , instanceHypervisor :: Hypervisor
                         , instanceVirtualizationType :: VirtualizationType
                         , instanceClientToken :: Maybe T.Text
                         , instanceBlockDeviceMapping :: [InstanceBlockDeviceMapping]
                         , instanceEbsOptimized :: Bool
                         , instanceStateReason :: Maybe StateReason
                         , instanceNetworkInterfaces :: [InstanceNetworkInterface]
                         , instancePrivateIpAddress :: Maybe IPv4
                         , instancePublicIpAddress :: Maybe IPv4
                         , instanceTags :: [Tag]
                         , instanceKernelId :: Maybe T.Text
                         , instanceSourceDestCheck :: Maybe Bool
                         , instanceVpcId :: Maybe T.Text
                         , instanceSubnetId :: Maybe T.Text
                         -- The following fields are documented but have not been
                         -- encountered in an actual response. During the development
                         -- phase of the launhcer we will leave the possible fields
                         -- commented out in case we will need them.
                         --, instanceIamInstanceProfile :: Maybe IamInstanceProfile
                         --, instanceLifecycle :: Maybe T.Text -- InstanceLifecycle = spot
                         --, instanceRamdiskId :: Maybe T.Text
                         --, instancePlatform :: Maybe T.Text -- Platform = windows
                         --, instanceSpotInstanceRequestId :: Maybe T.Text
                         --, instanceSriovNetSupport :: Maybe T.Text
                         } deriving (Show)

instance FromJSON Instance where
  parseJSON (Object v) = Instance <$>
    v .: "instanceId" <*>
    v .: "imageId" <*>
    v .: "instanceState" <*>
    v .:? "privateDnsName" .!= Nothing <*>
    v .:? "dnsName" .!= Nothing <*>
    v .:? "reason" .!= Nothing <*>
    v .:? "keyName" .!= Nothing <*>
    (read <$> v .: "amiLaunchIndex") <*>
    (maybeToList <$> (v .:? "productCodes" .!= Nothing)) <*>
    v .: "instanceType" <*>
    v .: "launchTime" <*>
    v .: "placement" <*>
    v .: "monitoring" <*>
    (maybeToList <$> (v .:? "groupSet" .!= Nothing)) <*>
    (read <$> (v .: "architecture")) <*>
    (read <$> (v .: "rootDeviceType")) <*>
    v .:? "rootDeviceName" .!= Nothing <*>
    (read <$> (v .: "hypervisor")) <*>
    (read <$> (v .: "virtualizationType")) <*>
    v .:? "clientToken" .!= Nothing <*>
    (maybeToList <$> (v .:? "blockDeviceMapping" .!= Nothing)) <*>
    (readBool <$> (v .: "ebsOptimized")) <*>
    v .:? "stateReason" .!= Nothing <*>
    (maybeToList <$> (v .:? "networkInterfaceSet" .!= Nothing)) <*>
    (fmap read <$> (v .:? "privateIpAddress" .!= Nothing)) <*>
    (fmap read <$> (v .:? "ipAddress" .!= Nothing)) <*>
    (maybeToList <$> (v .:? "tagSet" .!= Nothing)) <*>
    v .:? "kernelId" .!= Nothing <*>
    (fmap readBool <$> (v .:? "sourceDestCheck" .!= Nothing)) <*>
    v .:? "vpcId" .!= Nothing <*>
    v .:? "subnetId" .!= Nothing
  parseJSON invalid = typeMismatch "Instance" invalid

data ProductCode = ProductCode { productCodeId :: T.Text
                               , productCodeType :: T.Text} --ProductCodeType = devpay | marketplace
                               deriving (Show)

instance FromJSON ProductCode where
  parseJSON (Object v) = ProductCode <$>
    v .: "productCodeId" <*>
    v .: "productCodeType"
  parseJSON invalid = typeMismatch "ProductCode" invalid

data InstanceNetworkInterface =
  InstanceNetworkInterface { iniNetworkInterfaceId :: T.Text
                           , iniSubnetId :: Maybe T.Text
                           , iniVpcId :: Maybe T.Text
                           , iniDescription :: Maybe T.Text
                           , iniOwnerId :: T.Text
                           , iniStatus :: T.Text -- Status = available | attaching | in-use | detaching
                           , iniMacAddress :: Maybe T.Text
                           , iniPrivateIpAddress :: Maybe IPv4
                           , iniSourceDestCheck :: Maybe Bool
                           , iniSecurityGroups :: [Group]
                           , iniAttachment :: Value -- Attachment
                           , iniAssociation :: Maybe Value} -- Maybe Association}
                           deriving (Show)

instance FromJSON InstanceNetworkInterface where
  parseJSON (Object v) = InstanceNetworkInterface <$>
    v .: "networkInterfaceId" <*>
    v .:? "subnetId" .!= Nothing <*>
    v .:? "vpcId" .!= Nothing <*>
    v .:? "description" .!= Nothing <*>
    v .: "ownerId" <*>
    v .: "status" <*>
    v .:? "macAddress" .!= Nothing <*>
    (fmap read <$> (v .:? "privateIpAddress" .!= Nothing)) <*>
    (fmap readBool <$> (v .:? "sourceDestCheck" .!= Nothing)) <*>
    (maybeToList <$> (v .:? "groupSet" .!= Nothing)) <*>
    v .: "attachment" <*>
    v .:? "association" .!= Nothing
  parseJSON invalid = typeMismatch "InstanceNetworkInterface" invalid

data Tag = Tag { tagKey :: T.Text
               , tagValue :: T.Text}
               deriving (Show)

instance FromJSON Tag where
  parseJSON (Object v) = Tag <$>
    v .: "key" <*>
    v .: "value"
  parseJSON invalid = typeMismatch "Tag" invalid

data InstanceBlockDeviceMapping =
  InstanceBlockDeviceMapping { ibdmDeviceName :: T.Text
                             , ibdmEbs :: EbsInstanceBlockDevice}
                             deriving (Show)

instance FromJSON InstanceBlockDeviceMapping where
  parseJSON (Object v) = InstanceBlockDeviceMapping <$>
    v .: "deviceName" <*>
    v .: "ebs"
  parseJSON invalid = typeMismatch "InstanceBlockDeviceMapping" invalid

data EbsInstanceBlockDevice =
  EbsInstanceBlockDevice { eibdVolumeId :: T.Text
                         , eibdStatus :: T.Text -- Status = attaching | attached | detatichg | detached
                         , eibdAttachTime :: UTCTime
                         , eibdDeleteOnTermination :: Bool}
                         deriving (Show)

instance FromJSON EbsInstanceBlockDevice where
  parseJSON (Object v) = EbsInstanceBlockDevice <$>
    v .: "volumeId" <*>
    v .: "status" <*>
    v .: "attachTime" <*>
    (readBool <$> (v .: "deleteOnTermination"))
  parseJSON invalid = typeMismatch "EbsInstanceBlockDevice" invalid

data InstanceState = InstanceState { isName :: InstanceStateName
                                   , isCode :: Int}
                                   deriving (Show)

instance FromJSON InstanceState where
  parseJSON (Object v) = InstanceState <$>
    (read <$> (v .: "name")) <*>
    (read <$> (v .: "code"))
  parseJSON invalid = typeMismatch "InstanceState" invalid


data Placement = Placement { pAvailabilityZone :: T.Text
                           , pTenancy :: T.Text
                           , pGroupName :: Maybe T.Text}
                           deriving (Show)

instance FromJSON Placement where
  parseJSON (Object v) = Placement <$>
    v .: "availabilityZone" <*>
    v .: "tenancy"  <*>
    v .: "groupName"
  parseJSON invalid = typeMismatch "Placement" invalid

data Monitoring = Monitoring {monitoringState :: T.Text} deriving (Show) -- State = disabled | disabling | enabled | pendig

instance FromJSON Monitoring where
  parseJSON (Object v) = Monitoring <$> v .: "state"
  parseJSON invalid = typeMismatch "Monitoring" invalid

data StateReason = StateReason { srCode :: T.Text
                               , srMessage :: T.Text}
                               deriving (Show)

instance FromJSON StateReason where
  parseJSON (Object v) = StateReason <$>
    v .: "code" <*>
    v .: "message"
  parseJSON invalid = typeMismatch "StateReason" invalid

data AvailabilityZone = AvailabilityZone { azRegionName :: T.Text
                                         , azZoneName :: T.Text
                                         , azZoneState :: T.Text
                                         , azMessageSet :: Value}
                                         deriving (Show)

instance FromJSON AvailabilityZone where
  parseJSON (Object v) = AvailabilityZone <$>
    v .: "regionName" <*>
    v .: "zoneName" <*>
    v .: "zoneState" <*>
    v .: "messageSet"
  parseJSON invalid = typeMismatch "AvailabilityZone" invalid

data SecurityGroup = SecurityGroup { sgOwnerId :: T.Text
                                   , sgGroupId :: T.Text
                                   , sgGroupName :: T.Text
                                   , sgGroupDescription :: T.Text
                                   , sgVpcId :: Maybe T.Text
                                   , sgIpPermissionsEgress :: Value
                                   , sgIpPermissions :: Value}
                                   deriving (Show)

instance FromJSON SecurityGroup where
  parseJSON (Object v) = SecurityGroup <$>
    v .: "ownerId" <*>
    v .: "groupId" <*>
    v .: "groupName" <*>
    v .: "groupDescription" <*>
    v .:? "vpcId" .!= Nothing <*>
    v .: "ipPermissionsEgress" <*>
    v .: "ipPermissions"
  parseJSON invalid = typeMismatch "SecurityGroup" invalid

data PlacementGroup = PlacementGroup { pgGroupName :: T.Text
                                     , pgStrategy :: T.Text
                                     , pgState :: T.Text}
                                     deriving (Show)

instance FromJSON PlacementGroup where
  parseJSON (Object v) = PlacementGroup <$>
    v .: "groupName" <*>
    v .: "strategy" <*>
    v .: "state"
  parseJSON invalid = typeMismatch "PlacementGroup" invalid

data KeyPair = KeyPair { keyName :: T.Text
                       , keyFingerprint :: T.Text}
                       deriving (Show)

instance FromJSON KeyPair where
  parseJSON (Object v) = KeyPair <$>
    v .: "keyName" <*>
    v .: "keyFingerprint"
  parseJSON invalid = typeMismatch "KeyPair" invalid

data Vpc = Vpc { vpcId :: T.Text
               , vpcState :: T.Text
               , vpcCIDR :: T.Text
               , vpcDHCPOptions :: T.Text
               , vpcInstanceTenacity :: T.Text
               , vpcIsDefault :: Maybe Bool}
               deriving (Show)

instance FromJSON Vpc where
  parseJSON (Object v) = Vpc <$>
    v .: "vpcId" <*>
    v .: "state" <*>
    v .: "cidrBlock" <*>
    v .: "dhcpOptionsId" <*>
    v .: "instanceTenancy" <*>
    (fmap readBool <$> (v .:? "isDefault" .!= Nothing))
  parseJSON invalid = typeMismatch "Vpc" invalid

data Subnet = Subnet { subnetId :: T.Text
                     , subnetVpcId :: T.Text
                     , subnetState :: T.Text
                     , subnetCIDR :: T.Text
                     , subnetAZ :: T.Text
                     , subnetAvailableIpCount :: Int
                     , subnetDefaultForAZ :: Maybe Bool
                     , subnetMapPublicIp :: Maybe Bool}
                     deriving (Show)

instance FromJSON Subnet where
  parseJSON (Object v) = Subnet <$>
    v .: "subnetId" <*>
    v .: "vpcId" <*>
    v .: "state" <*>
    v .: "cidrBlock" <*>
    v .: "availabilityZone" <*>
    (read <$> (v .: "availableIpAddressCount")) <*>
    (fmap readBool <$> (v .:? "defaultForAz" .!= Nothing)) <*>
    (fmap readBool <$> (v .:? "mapPublicIpOnLaunch" .!= Nothing))
  parseJSON invalid = typeMismatch "Subnet" invalid

-- Enums

data InstanceTenancy = Default | Dedicated

instance Show InstanceTenancy where
    show Default = "default"
    show Dedicated = "dedicated"

data VolumeType = Standard | GP2SSD | IOPSSD Int

instance Show VolumeType where
    show Standard = "standard"
    show GP2SSD = "gp2"
    show (IOPSSD _) = "io1"

data IpProtocol = TCP | UDP | ICMP | Proto Int | All

instance Show IpProtocol where
    show TCP = "tcp" -- 6
    show UDP = "udp" -- 17
    show ICMP = "icmp" -- 1
    show (Proto i) = show i
    show All = "-1"

data VirtualizationType = HVM | PARAVIRTUAL deriving (Eq)

instance Show VirtualizationType where
    show HVM = "hvm"
    show PARAVIRTUAL = "paravirtual"

instance Read VirtualizationType where
  readsPrec _ h = case h of
                    "hvm" -> [(HVM,"")]
                    "paravirtual" -> [(PARAVIRTUAL,"")]
                    _ -> fail $ "Failed to parse " ++ h ++ " into VirtualizationType."

data RootDeviceType = EBS_STORE | INSTANCE_STORE deriving (Eq)

instance Show RootDeviceType where
    show EBS_STORE = "ebs"
    show INSTANCE_STORE = "instance-store"

instance Read RootDeviceType where
  readsPrec _ h = case h of
                    "ebs" -> [(EBS_STORE,"")]
                    "instance-store" -> [(INSTANCE_STORE,"")]
                    _ -> fail $ "Failed to parse " ++ h ++ " into RootDeviceType."

data Architecture = I386 | X86_64 deriving (Eq)

instance Show Architecture where
    show I386 = "32-bit"
    show X86_64 = "64-bit"

instance Read Architecture where
  readsPrec _ h = case h of
                    "i386" -> [(I386,"")]
                    "x86_64" -> [(X86_64,"")]
                    _ -> fail $ "Failed to parse " ++ h ++ " into Architecture."

data Hypervisor = OVM | XEN deriving (Eq)

instance Show Hypervisor where
    show OVM = "ovm"
    show XEN = "xen"

instance Read Hypervisor where
  readsPrec _ h = case h of
                    "ovm" -> [(OVM,"")]
                    "xen" -> [(XEN,"")]
                    _ -> fail $ "Failed to parse " ++ h ++ " into Hypervisor."

data InstanceStateName = Pending
                       | Running
                       | ShuttingDown
                       | Terminated
                       | Stopping
                       | Stopped
                       deriving (Eq)

instance Show InstanceStateName where
    show Pending = "pending"
    show Running = "running"
    show ShuttingDown = "shutting-down"
    show Terminated = "terminated"
    show Stopping = "stopping"
    show Stopped = "stopped"

instance Read InstanceStateName where
  readsPrec _ h = case h of
                    "pending" -> [(Pending,"")]
                    "running" -> [(Running,"")]
                    "shutting-down" -> [(ShuttingDown,"")]
                    "terminated" -> [(Terminated,"")]
                    "stopping" -> [(Stopping,"")]
                    "stopped" -> [(Stopped,"")]
                    _ -> fail $ "Failed to parse " ++ h ++ " into InstanceStateName."


-- Helper functions

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

-- JSON parsing helper functions

maybeToList :: Maybe [a] -> [a]
maybeToList (Just a) = a
maybeToList Nothing = []

readBool :: String -> Bool
readBool (x:xs) = read $ toUpper x : map toLower xs
readBool [] = error "Cannot read bool. String is empty."
