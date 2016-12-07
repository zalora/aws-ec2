{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           , CPP
           , OverloadedStrings
           #-}

module Aws.Ec2.Commands.DescribeTags where

import Aws.Ec2.TH

data DescribeTags = DescribeTags [Text]
                       deriving (Show)

EC2VALUETRANSACTIONDEF(DescribeTags,"DescribeTags","tagSet","") -- search not implemented
