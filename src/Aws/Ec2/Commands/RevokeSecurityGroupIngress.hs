{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.Ec2.Commands.RevokeSecurityGroupIngress where

import Data.Text (Text)
import Aws.Ec2.TH
import Aws.Ec2.Types

data RevokeSecurityGroupIngress = RevokeSecurityGroupIngress
               { rsi_groupId :: Text
               , rsi_permissions :: [SgPermission]
               } deriving (Show)

instance SignQuery RevokeSecurityGroupIngress where
    type ServiceConfiguration RevokeSecurityGroupIngress = EC2Configuration
    signQuery RevokeSecurityGroupIngress{..} = ec2SignQuery $
                                           [ ("GroupId", qArg rsi_groupId)
                                           , ("Action", qArg "RevokeSecurityGroupIngress")
                                           , defVersion
                                           ] +++ enumeratePermissions rsi_permissions

ec2ValueTransaction ''RevokeSecurityGroupIngress "RevokeSecurityGroupIngressResponse"
