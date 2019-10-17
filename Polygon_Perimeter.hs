import Data.List
import Control.Monad

main :: IO ()
main = do
        n <- readLn :: IO Int
        mylist <- acceptFunction n
        print (perimeter mylist (head mylist) )

acceptFunction :: Int -> IO [(Int,Int)]
acceptFunction 0 = return []
acceptFunction n = do
                    str <-  readInts
                    let k = (str !! 0, str !! 1)
                    p   <- acceptFunction (n-1)
                    return (k:p)

readInts :: IO [Int]
readInts = fmap (map read.words) getLine

perimeter :: [(Int,Int)] -> (Int,Int) -> Double
perimeter [] _= 0
perimeter [(a,b)] (c,d) = sqrt $ (fromIntegral (d - b)^2) + (fromIntegral (c - a)^2)
perimeter ((a,b):(c,d):rest) e= sqrt ((fromIntegral (d - b)^2) + (fromIntegral (c - a)^2)) + (perimeter ((c,d):rest) e)

