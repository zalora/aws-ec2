{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           , OverloadedStrings
           #-}

module Aws.Ec2.Commands.DescribeVolumes where

import Aws.Ec2.TH

data DescribeVolumes = DescribeVolumes [Text]
                       deriving (Show)

ec2ValueTransactionDef ''DescribeVolumes 'DescribeVolumes "volumeSet" "VolumeId"
