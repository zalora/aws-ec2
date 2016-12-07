{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           , OverloadedStrings
           #-}

module Aws.Ec2.Commands.DescribePlacementGroups where

import Aws.Ec2.TH

data DescribePlacementGroups = DescribePlacementGroups [Text]
                       deriving (Show)

ec2ValueTransactionDef ''DescribePlacementGroups 'DescribePlacementGroups "placementGroupSet" "GroupName"
