{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           , OverloadedStrings
           , CPP
           #-}

module Aws.Ec2.Commands.DescribeVolumeStatus where

import Aws.Ec2.TH

data DescribeVolumeStatus = DescribeVolumeStatus [Text]
                       deriving (Show)

EC2VALUETRANSACTIONDEF(DescribeVolumeStatus,"DescribeVolumeStatus","volumeStatusSet","VolumeId")
