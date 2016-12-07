{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , TemplateHaskell
           , CPP
           , OverloadedStrings
           #-}

module Aws.Ec2.Commands.DescribeVpcs where

import Aws.Ec2.TH

data DescribeVpcs = DescribeVpcs { dvpc_vpcIds :: [Text] }
                       deriving (Show)

EC2VALUETRANSACTIONDEF(DescribeVpcs,"DescribeVpcs","vpcSet","VpcId")
