{-# LANGUAGE RecordWildCards #-}

module Aws.SNS.Core (
  module Aws.Query
, defVersion
, snsSignQuery
) where

import qualified Data.ByteString as B
import Network.HTTP.Types hiding (Method)
import Aws.Core
import Aws.Query
import Aws.Query.TH

defVersion :: QueryItem
defVersion = ("Version", Just "2010-03-31")

snsSignQuery :: Query -> QueryAPIConfiguration qt -> SignatureData -> SignedQuery
snsSignQuery query QueryAPIConfiguration{..} = v2SignQuery query qd
  where
    qd = QueryData { qdRegion = qaRegion
                   , qdEndpoint = B.concat ["sns.", qaRegion, ".amazonaws.com"]
                   , qdService = ""
                   }
