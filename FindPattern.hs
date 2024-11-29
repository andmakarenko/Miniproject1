import System.Random (randomRIO)
import Data.List (group, sort)
import qualified Data.Map as Map

main :: IO ()
main = do
    -- Generate an array of 1000 random integers between 1 and 9
    numbers <- generateRandomList 1000 1 9

    -- Find the frequency of each number
    let frequencyMap = Map.fromListWith (+) [(x, 1) | x <- numbers]

    -- Print the frequency of each number
    putStrLn "Frequency of each number:"
    mapM_ (\(k, v) -> putStrLn $ show k ++ ": " ++ show v ++ " times") $ Map.toList frequencyMap

    -- Find repeated sequences of length 3
    let sequences = [take 3 (drop i numbers) | i <- [0..(length numbers - 3)]]
        sequenceMap = Map.fromListWith (+) [(s, 1) | s <- sequences]

    -- Print sequences repeated more than 4 times
    putStrLn "\nRepeated sequences of length 3 that come up more than 4 times:"
    mapM_ (\(s, v) -> putStrLn $ show s ++ " appears " ++ show v ++ " times") $
        filter ((> 4) . snd) $ Map.toList sequenceMap

-- Helper function to generate a list of random numbers between given bounds
generateRandomList :: Int -> Int -> Int -> IO [Int]
generateRandomList 0 _ _ = return []
generateRandomList n minVal maxVal = do
    r <- randomRIO (minVal, maxVal)
    rs <- generateRandomList (n - 1) minVal maxVal
    return (r:rs)
