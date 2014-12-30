{-# LANGUAGE RecordWildCards #-}

module Aws.CloudWatch.Core (
  module Network.HTTP.Types
, module Aws.Core
, module Aws.Query
, module Aws.Query.TH
, Dimension (..)
, cwSignQuery
, enumerateDimensions
) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Network.HTTP.Types hiding (Method)
import Aws.Core
import Aws.Query
import Aws.Query.TH

data Dimension = Dimension { di_name :: Text
                           , di_value :: Text
                           } deriving (Show, Eq)

instance Read Dimension where
    readsPrec _ v = case T.split (== '=') $ T.pack v of
        [x, y] -> [(Dimension x y, "")]
        _ -> []

enumerateDimensions :: [Dimension] -> Query
enumerateDimensions = enumerateLists "Dimensions.member." . fmap unroll
  where
    unroll Dimension{..} = [ ("Name", qArg di_name)
                           , ("Value", qArg di_value)
                           ]

cwSignQuery :: Query -> QueryAPIConfiguration qt -> SignatureData -> SignedQuery
cwSignQuery query QueryAPIConfiguration{..} = v2SignQuery query qd
  where
    qd = QueryData { qdRegion = qaRegion
                   , qdEndpoint = B.concat ["monitoring.", qaRegion, ".amazonaws.com"]
                   , qdService = ""
                   }
