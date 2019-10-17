-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Data.List
import Control.Monad

main :: IO [()]
main = do
        n <- readLn :: IO Int
        forM [1..n] $ \_ -> do
        mylist <- acceptFunction n
        print (polArea mylist)

acceptFunction :: Int -> IO [(Float,Float)]
acceptFunction 0 = return []
acceptFunction n = do
                    str <-  readInts
                    let k = (str !! 0, str !! 1)
                    p   <- acceptFunction (n-1)
                    return (k:p)

readInts :: IO [Float]
readInts = fmap (map read.words) getLine

midPoint :: [(Float,Float)]->(Float,Float)
midPoint x = ((sum $ map fst x) / (fromInteger $ toInteger l) , (sum $ map snd x) / (fromInteger $ toInteger l))
              where
              l = length x 

traingle :: (Float,Float)->(Float,Float)->(Float,Float)->Float
traingle x x' mp = sqrt $ s*(s-a)*(s-b)*(s-c)
                   where
                   a = sqrt $ ((fst x) - (fst x'))^2 + ((snd x) - (snd x'))^2
                   b = sqrt $ ((fst x) - (fst mp))^2 + ((snd x) - (snd mp))^2
                   c = sqrt $ ((fst mp) - (fst x'))^2 + ((snd mp) - (snd x'))^2
                   s = (a+b+c)/2

area :: [(Float,Float)]->(Float,Float)->Float
area [a] _ = 0
area (x:y:rest) mp = (traingle x y mp) + area (y:rest) mp

polArea :: [(Float,Float)]->Float
polArea [] = 0
polArea [a] = 0
polArea [a,b] = 0
polArea coordinates = area (coordinates ++ [head coordinates])  (midPoint coordinates)
