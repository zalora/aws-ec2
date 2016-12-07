{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           , CPP
           #-}

module Aws.Elb.Commands.DescribeInstanceHealth where

import Aws.Elb.TH

data DescribeInstanceHealth = DescribeInstanceHealth Text

instance SignQuery DescribeInstanceHealth where
    type ServiceConfiguration DescribeInstanceHealth = QueryAPIConfiguration
    signQuery (DescribeInstanceHealth lb) = elbSignQuery $ [ ("Action", qArg "DescribeInstanceHealth")
                                                           , defVersion
                                                           , ("LoadBalancerName", qArg lb)
                                                           ]

ELBVALUETRANSACTION(DescribeInstanceHealth,"DescribeInstanceHealthResult")
