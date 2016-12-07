{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           , OverloadedStrings
           , CPP
           #-}

module Aws.Ec2.Commands.DescribeRegions where

import Aws.Ec2.TH

data DescribeRegions = DescribeRegions [Text]
                       deriving (Show)

EC2VALUETRANSACTIONDEF(DescribeRegions,"DescribeRegions","regionInfo","RegionName")
