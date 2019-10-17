import Data.List
import Control.Monad

main :: IO ()
main = do
        n <- readLn :: IO Int
        forM [1..n] 
        mylist <- acceptFunction n
        print (isFunction mylist)

acceptFunction :: Int -> IO [(Int,Int)]
acceptFunction 0 = return []
acceptFunction n = do
                    str <-  readInts
                    let k = (str !! 0, str !! 1)
                    p   <- acceptFunction (n-1)
                    return (k:p)

readInts :: IO [Int]
readInts = fmap (map read.words) getLine

isFunction :: (Eq a) => [(a,a)] -> Bool
isFunction x = (length (nub $ map fst x)) == (length x)