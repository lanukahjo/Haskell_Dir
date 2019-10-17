module Matrix
where

import Data.List
-----------------------------------------------------------------------------------
transpose' :: [[a]]->Int->[[a]]
transpose' _ 0 = []
transpose' x col = [(map head x)] ++ (transpose' (map tail x) (col-1))

mytranspose :: [[a]]->Maybe [[a]]
mytranspose x
        | ((checkMatrix x) == True) = Just (transpose' x (length $ head x))
        | otherwise                 = Nothing
------------------------------------------------------------------------------------
checkMatrix :: [[a]]->Bool
checkMatrix x = (==1).length $ nub $ map length x
------------------------------------------------------------------------------------
isSquare :: [[a]]->Bool
isSquare x = ((length x) == (length $ head x)) && (checkMatrix x)
------------------------------------------------------------------------------------
 