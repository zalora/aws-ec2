{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           , LambdaCase
           , CPP
           #-}

module Aws.Ec2.Commands.StartInstances where

import Data.Text ()
import Data.ByteString.Char8 (pack)
import qualified Network.HTTP.Types as HTTP
import Data.Monoid
import Aws.Ec2.TH

-- http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-StartInstances.html
data StartInstances = StartInstances
                  { start_instanceIds :: [Text]
                  } deriving (Show)

instance SignQuery StartInstances where
    type ServiceConfiguration StartInstances = EC2Configuration
    signQuery StartInstances{..} = ec2SignQuery $
                                  main
                                  +++ enumerateInstances (zip [1..] start_instanceIds)
        where
          main :: HTTP.Query
          main = [ ("Action", qArg "StartInstances")
                 , defVersion
                 ]
          enumerateInstances :: [(Int,Text)] -> HTTP.Query
          enumerateInstances [] = []
          enumerateInstances ((i,x):xs) = [("InstanceId." <> pack (show i), qArg x)] +++ enumerateInstances xs

EC2VALUETRANSACTION(StartInstances,"StartInstancesResponse")
