{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           , OverloadedStrings
           #-}

module Aws.Ec2.Commands.DescribeVolumeStatus where

import Aws.Ec2.TH

data DescribeVolumeStatus = DescribeVolumeStatus [Text]
                       deriving (Show)

ec2ValueTransactionDef ''DescribeVolumeStatus 'DescribeVolumeStatus "volumeStatusSet" "VolumeId"
