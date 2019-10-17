module Function 
where

import Data.List

isFunction :: [(a,a)] -> Bool
isFunction x = (length (nub $ map fst x)) == (length x)  