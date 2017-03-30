{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , RecordWildCards
           , LambdaCase
           , OverloadedStrings
           #-}

module Aws.Elb.Types where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Monoid
import Data.ByteString.Char8 (ByteString, pack)

import Network.HTTP.Types as HTTP

import Aws.Query
import Aws.Elb.Core

enumerateInstanceIds :: [Text] -> HTTP.Query
enumerateInstanceIds = enumerateLists "Instances.member." . fmap unroll
  where
    unroll i = [("InstanceId", qArg i)]

