{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           , OverloadedStrings
           , CPP
           #-}

module Aws.Elb.Commands.DescribeLoadBalancers where

import Aws.Elb.TH

data DescribeLoadBalancers = DescribeLoadBalancers [Text]

ELBVALUETRANSACTIONDEF(DescribeLoadBalancers,"DescribeLoadBalancers","LoadBalancerDescriptions","LoadBalancerNames.member")
