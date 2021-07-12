-- | HTTPRequests.hs

module Chapter21.HTTPRequests where

import Data.ByteString.Lazy hiding (map)
import Network.Wreq

urls :: [String]
urls = [ "http://httpbin.org/ip"
       , "http://httpbin.org/bytes/5"
       ]

mappingGet :: IO [(Response ByteString)]
mappingGet = traverse get urls
