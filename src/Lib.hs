module Lib
  ( insertMany
  ) where

import Data.Map (Map, insert)

insertMany :: Ord k => [(k, a)] -> (Map k a) -> (Map k a)
insertMany kvs m = foldr (\(k, v) acc -> insert k v acc) m kvs
