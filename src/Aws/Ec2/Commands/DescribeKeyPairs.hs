{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           , CPP
           , OverloadedStrings
           #-}

module Aws.Ec2.Commands.DescribeKeyPairs where

import Aws.Ec2.TH

data DescribeKeyPairs = DescribeKeyPairs [Text]
                       deriving (Show)

EC2VALUETRANSACTIONDEF(DescribeKeyPairs,"DescribeKeyPairs","keySet","KeyName")
