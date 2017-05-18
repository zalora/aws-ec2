{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           , RecordWildCards
           , OverloadedStrings
           #-}

module Aws.Ec2.Commands.DescribeSecurityGroups where

import Data.Aeson (Value (..), FromJSON, (.:), parseJSON)
import Data.Aeson.Types (typeMismatch)

import Aws.Ec2.TH

type SecurityGroupId = Text
type SecurityGroupName = Text

data DescribeSecurityGroups = DescribeSecurityGroups
                            { sg_ids :: [SecurityGroupId]
                            , sg_names :: [SecurityGroupName]
                            }
                      deriving (Show)

instance SignQuery DescribeSecurityGroups where
    type ServiceConfiguration DescribeSecurityGroups = EC2Configuration
    signQuery DescribeSecurityGroups{..} = ec2SignQuery $
                                                [ ("Action", qArg "DescribeSecurityGroups")
                                                , defVersion
                                                ] +++ enumerate "GroupId" sg_ids qArg
                                                  +++ case sg_names of
                                                        [] -> []
                                                        _ -> ("Filter.1.Name", qArg "group-name"):(enumerate "Filter.1.Value" sg_names qArg)

ec2ValueTransaction ''DescribeSecurityGroups "securityGroupInfo"

newtype DescribeSecurityGroupsResponse =
  DescribeSecurityGroupsResponse {dsgrGroups :: [Group]} deriving (Show)

instance FromJSON DescribeSecurityGroupsResponse where
  parseJSON v = DescribeSecurityGroupsResponse <$>
    (maybeToList <$> parseJSON v)
