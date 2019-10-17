module Sample 
where

e :: Float->Float->Float
e x 0 = 1
e x n = (term x n) + e x (n-1)

term :: Float->Float->Float
term x n= (pow x n)/(fact n)

fact :: Float->Float
fact 0 = 1
fact n = n * fact (n-1) 

pow :: Float->Float->Float
pow x 0 = 1
pow x n = x * pow x (n-1)

mylength :: [a] -> Int
mylength [] = 0
mylength (_:xs) = (+1) $ mylength xs 

palindrome :: [a] -> [a]
palindrome [] = []
palindrome p = p ++ rev p

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome p = (p == (rev p))