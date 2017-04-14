{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.Ec2.Commands.AuthorizeSecurityGroupIngress where

import Data.Text (Text)
import Aws.Ec2.TH
import Aws.Ec2.Types

data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress
               { asi_groupId :: Text
               , asi_permissions :: [SgPermission]
               } deriving (Show)

instance SignQuery AuthorizeSecurityGroupIngress where
    type ServiceConfiguration AuthorizeSecurityGroupIngress = EC2Configuration
    signQuery AuthorizeSecurityGroupIngress{..} = ec2SignQuery $
                                           [ ("GroupId", qArg asi_groupId)
                                           , ("Action", qArg "AuthorizeSecurityGroupIngress")
                                           , defVersion
                                           ] +++ enumeratePermissions asi_permissions

ec2ValueTransaction ''AuthorizeSecurityGroupIngress "AuthorizeSecurityGroupIngressResponse"
