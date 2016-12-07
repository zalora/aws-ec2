{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           , LambdaCase
           , CPP
           #-}

module Aws.Ec2.Commands.TerminateInstances where

import Data.Text ()
import Data.ByteString.Char8 (pack)
import qualified Network.HTTP.Types as HTTP
import Data.Monoid
import Aws.Ec2.TH

-- http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-TerminateInstances.html
data TerminateInstances = TerminateInstances
                  { term_instanceIds :: [Text]
                  } deriving (Show)

instance SignQuery TerminateInstances where
    type ServiceConfiguration TerminateInstances = EC2Configuration
    signQuery TerminateInstances{..} = ec2SignQuery $
                                  main
                                  +++ enumerateInstances (zip [1..] term_instanceIds)
        where
          main :: HTTP.Query
          main = [ ("Action", qArg "TerminateInstances")
                 , defVersion
                 ]
          enumerateInstances :: [(Int,Text)] -> HTTP.Query
          enumerateInstances [] = []
          enumerateInstances ((i,x):xs) = [("InstanceId." <> pack (show i), qArg x)] +++ enumerateInstances xs


EC2VALUETRANSACTION(TerminateInstances,"TerminateInstancesResponse")
