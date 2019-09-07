
main :: IO()
main = do
    n <- read <$> getLine
    putStrLn $ show $ reverse $ calc n

calc :: Int -> [Int]
calc n = loopCalc n 1 [0]

loopCalc :: Int -> Int -> [Int] -> [Int]
loopCalc 0 _ x = x
loopCalc n c x = loopCalc (n - 1) (c + 1) $ isLoseAdd c x

isLoseAdd :: Int -> [Int] -> [Int]
isLoseAdd c x =
    if isLose c 1 x then c:x else x

isLose :: Int -> Int -> [Int] -> Bool
isLose c i x  
    | c < i = True
    | not $ isPrime i = isLose c (i + 1) x
    | length (filter (\x -> x == c) (map (+ i) x)) /= 0 = False
    | otherwise = isLose c (i + 1) x


isPrime :: Int -> Bool
isPrime p
    | p < 2 = False
    | p == 2 = True
    | mod p 2 == 0 = False
    | otherwise = isOddPrimeLoop p 3

isOddPrimeLoop :: Int -> Int -> Bool
isOddPrimeLoop p i
    | p < i * i = True
    | mod p i == 0 = False
    | otherwise = isOddPrimeLoop p $ i + 2
