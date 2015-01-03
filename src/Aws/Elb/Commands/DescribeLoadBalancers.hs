{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           #-}

module Aws.Elb.Commands.DescribeLoadBalancers where

import Aws.Elb.TH

data DescribeLoadBalancers = DescribeLoadBalancers [Text]

ELBVALUETRANSACTIONDEF(DescribeLoadBalancers,"DescribeLoadBalancers","LoadBalancerDescriptions","LoadBalancerNames.member")
