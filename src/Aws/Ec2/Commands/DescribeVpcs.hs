{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , TemplateHaskell
           , OverloadedStrings
           #-}

module Aws.Ec2.Commands.DescribeVpcs where

import Aws.Ec2.TH

data DescribeVpcs = DescribeVpcs { dvpc_vpcIds :: [Text] }
                       deriving (Show)

ec2ValueTransactionDef ''DescribeVpcs 'DescribeVpcs "vpcSet" "VpcId"
