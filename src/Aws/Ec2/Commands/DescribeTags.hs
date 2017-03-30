{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           , OverloadedStrings
           #-}

module Aws.Ec2.Commands.DescribeTags where

import Data.Aeson (Value (..), FromJSON, (.:), parseJSON)
import Data.Aeson.Types (typeMismatch)

import Aws.Ec2.TH

data DescribeTags = DescribeTags [Text]
                       deriving (Show)

ec2ValueTransactionDef ''DescribeTags 'DescribeTags "tagSet" "" -- search not implemented

type DescribeTagsResponse = [TagDescription]

-- | Data structure returned from EC2 DescribeTags command.
data TagDescription = TagDescription
                      { tdResourceId :: Text
                      , tdResourceType :: Text
                      , tdKey :: Text
                      , tdValue :: Text }
                    deriving Show

instance FromJSON TagDescription where
  parseJSON (Object v) = TagDescription <$>
    v .: "resourceId" <*>
    v .: "resourceType" <*>
    v .: "key" <*>
    v .: "value"
  parseJSON invalid = typeMismatch "TagDescription" invalid
