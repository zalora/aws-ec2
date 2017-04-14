{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           , OverloadedStrings
           #-}

module Aws.Elb.Commands.DescribeLoadBalancers where

import Aws.Elb.TH

data DescribeLoadBalancers = DescribeLoadBalancers [Text]

elbValueTransactionDef ''DescribeLoadBalancers 'DescribeLoadBalancers "LoadBalancerDescriptions" "LoadBalancerNames.member"
