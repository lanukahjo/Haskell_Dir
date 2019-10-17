module StringToInt
where

convert :: String->Integer
convert str = foldl (\x y ->(digitToInt x)*10 + (digitToInt y)): 0 str

digitToInt :: Char -> Integer
digitToInt '0' = 0
digitToInt '1' = 1
digitToInt '2' = 2
digitToInt '3' = 3
digitToInt '4' = 4
digitToInt '5' = 5
digitToInt '6' = 6
digitToInt '7' = 7
digitToInt '8' = 8
digitToInt '9' = 9