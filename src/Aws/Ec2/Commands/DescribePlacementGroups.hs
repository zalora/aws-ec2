{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           , CPP
           , OverloadedStrings
           #-}

module Aws.Ec2.Commands.DescribePlacementGroups where

import Aws.Ec2.TH

data DescribePlacementGroups = DescribePlacementGroups [Text]
                       deriving (Show)

EC2VALUETRANSACTIONDEF(DescribePlacementGroups,"DescribePlacementGroups","placementGroupSet","GroupName")
