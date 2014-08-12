{-# LANGUAGE RecordWildCards #-}

module Aws.CloudWatch.Core (
  module Network.HTTP.Types
, module Aws.Core
, module Aws.Query
, module Aws.Query.TH
, cwSignQuery
) where

import qualified Data.ByteString as B
import Network.HTTP.Types hiding (Method)
import Aws.Core
import Aws.Query
import Aws.Query.TH

cwSignQuery :: Query -> QueryAPIConfiguration qt -> SignatureData -> SignedQuery
cwSignQuery query QueryAPIConfiguration{..} sd = v2SignQuery query qd sd
  where
    qd = QueryData { qdRegion = qaRegion
                   , qdEndpoint = B.concat ["monitoring.", qaRegion, ".amazonaws.com"]
                   , qdService = ""
                   }
