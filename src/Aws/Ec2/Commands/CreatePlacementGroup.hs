{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.Ec2.Commands.CreatePlacementGroup where

import Aws.Ec2.TH

data CreatePlacementGroup = CreatePlacementGroup
               { cpg_name :: Text
               -- currently strategy must be "cluster"
               --, cpg_strategy :: Text
               } deriving (Show)

instance SignQuery CreatePlacementGroup where
    type ServiceConfiguration CreatePlacementGroup = EC2Configuration
    signQuery CreatePlacementGroup{..} = ec2SignQuery $
                                           [ ("GroupName", qArg cpg_name)
                                           , ("Strategy", qArg "cluster")
                                           , ("Action", qArg "CreatePlacementGroup")
                                           , defVersion
                                           ]

EC2VALUETRANSACTION(CreatePlacementGroup,"return")
