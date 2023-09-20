module Lib (insertMany) where

import Data.Map hiding (map)

insertMany :: Ord k => [(k, a)] -> (Map k a) -> (Map k a)
insertMany kvs m = Prelude.foldr (\(k, v) acc -> insert k v acc) m kvs
