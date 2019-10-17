import System.IO

main :: IO()
main = do 
        inh <- openFile "The_Autumn_Of_The_Patriarch.txt" ReadMode
        inpstr <- hGetContents inh
        putStrLn $ "Number of Full Stops: " ++ (show $ countFullStop inpstr) ++ "\n"
        hClose inh

countFullStop :: String -> Int
countFullStop [] = 0
countFullStop (x:str) | x == ','  = 1 + countFullStop str
                      | otherwise = countFullStop str