{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           #-}

module Aws.Ec2.Commands.DescribeInstances where

import Data.Aeson (Value (..), FromJSON, parseJSON)

import Aws.Ec2.TH

data DescribeInstances = DescribeInstances { di_instanceIds :: [Text] }
                       deriving (Show)

instance SignQuery DescribeInstances where
    type ServiceConfiguration DescribeInstances = EC2Configuration
    signQuery DescribeInstances{..} = ec2SignQuery $
                                                [ ("Action", qArg "DescribeInstances")
                                                , defVersion
                                                ] +++ enumerate "InstanceId" di_instanceIds qArg

ec2ValueTransaction ''DescribeInstances "reservationSet"

newtype DescribeInstancesResponse =
  DescribeInstancesResponse {dirReservations :: [Reservation]} deriving (Show)

instance FromJSON DescribeInstancesResponse where
  parseJSON v = DescribeInstancesResponse <$>
    (maybeToList <$> parseJSON v)
