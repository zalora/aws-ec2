{-# LANGUAGE RecordWildCards #-}

module Aws.CloudWatch.Core
    ( Dimension(..)
    , cwSignQuery
    , enumerateDimensions
    , module X) where
import Network.HTTP.Types as X hiding (Method)
import Aws.Core as X
import Aws.Query as X
import Aws.Query.TH as X

import qualified Data.ByteString as B
import qualified Data.Text as T


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
