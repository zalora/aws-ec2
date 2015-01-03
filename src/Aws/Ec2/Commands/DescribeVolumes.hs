{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           #-}

module Aws.Ec2.Commands.DescribeVolumes where

import Aws.Ec2.TH

data DescribeVolumes = DescribeVolumes [Text]
                       deriving (Show)

EC2VALUETRANSACTIONDEF(DescribeVolumes,"DescribeVolumes","volumeSet","VolumeId")
