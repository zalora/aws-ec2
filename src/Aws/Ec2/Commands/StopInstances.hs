{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           , LambdaCase
           , CPP
           #-}

module Aws.Ec2.Commands.StopInstances where

import Data.Text ()
import Data.ByteString.Char8 (pack)
import qualified Network.HTTP.Types as HTTP
import Data.Monoid
import Aws.Ec2.TH

-- http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-StopInstances.html
data StopInstances = StopInstances
                  { stop_instanceIds :: [Text]
                  } deriving (Show)

instance SignQuery StopInstances where
    type ServiceConfiguration StopInstances = EC2Configuration
    signQuery StopInstances{..} = ec2SignQuery $
                                  main
                                  +++ enumerateInstances (zip [1..] stop_instanceIds)
        where
          main :: HTTP.Query
          main = [ ("Action", qArg "StopInstances")
                 , defVersion
                 ]
          enumerateInstances :: [(Int,Text)] -> HTTP.Query
          enumerateInstances [] = []
          enumerateInstances ((i,x):xs) = [("InstanceId." <> pack (show i), qArg x)] +++ enumerateInstances xs


EC2VALUETRANSACTION(StopInstances,"StopInstancesResponse")
