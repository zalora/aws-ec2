{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.Ec2.Commands.ModifyInstanceAttribute where

import Aws.Ec2.TH

data InstanceAttribute = DisableApiTermination Bool
                  deriving (Show)

data ModifyInstanceAttribute = ModifyInstanceAttribute
               { mia_InstanceId :: Text
               , mia_attribute :: InstanceAttribute
               } deriving (Show)

instance SignQuery ModifyInstanceAttribute where
    type ServiceConfiguration ModifyInstanceAttribute = EC2Configuration
    signQuery ModifyInstanceAttribute{..} = ec2SignQuery $
                                           [ ("InstanceId", qArg mia_InstanceId)
                                           , ("Action", qArg "ModifyInstanceAttribute")
                                           , defVersion
                                           ] +++ case mia_attribute of
                                                   DisableApiTermination dat -> [("DisableApiTermination.Value", qShow dat)]

ec2ValueTransaction ''ModifyInstanceAttribute "return"
