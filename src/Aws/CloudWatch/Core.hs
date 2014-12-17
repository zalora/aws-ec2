{-# LANGUAGE RecordWildCards #-}

module Aws.CloudWatch.Core (
  module Network.HTTP.Types
, module Aws.Core
, module Aws.Query
, module Aws.Query.TH
, Dimension (..)
, cwSignQuery
) where

import qualified Data.ByteString as B
import Network.HTTP.Types hiding (Method)
import Aws.Core
import Aws.Query
import Aws.Query.TH

data Dimension = Dimension { di_name :: Text
                           , di_value :: Text
                           } deriving (Show, Eq)

cwSignQuery :: Query -> QueryAPIConfiguration qt -> SignatureData -> SignedQuery
cwSignQuery query QueryAPIConfiguration{..} = v2SignQuery query qd
  where
    qd = QueryData { qdRegion = qaRegion
                   , qdEndpoint = B.concat ["monitoring.", qaRegion, ".amazonaws.com"]
                   , qdService = ""
                   }
