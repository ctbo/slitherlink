import Slitherlink
import System.Environment

main :: IO ()
main = do
     args <- getArgs
     case args of
          [filename, number] -> do
                     s <- readFile filename
                     work s (read number)
          [filename] -> do
                     s <- readFile filename
                     work s 2
          [] -> work sampleProblemString 2
          _  -> error "Too many arguments."
  where work s n = case readProblem s of
             Left e -> putStrLn e
             Right p -> do
                   putStrLn $ "Showing " ++ (if n == 0 then "all" else "up to " ++ show n) ++ " solutions."
                   let solutions = solve p
                   let display
                         | n == 0 = solutions
                         | otherwise = take n solutions
                   putStr $ concatMap showState display
                   putStrLn $ "Total number of solutions: " ++ show (length solutions)

sampleProblemString :: String
sampleProblemString = unlines [".3.112.2.."
                              ,".3..3.1312"
                              ,"22.1......"
                              ,".3..3..2.2"
                              ,"2.....2.21"
                              ,"31.3.....3"
                              ,"2.2..3..2."
                              ,"......1.32"
                              ,"2220.3..3."
                              ,"..3.122.2."
                              ]

-- stuff for interactive experiments

sampleProblem :: Problem
sampleProblem = case readProblem sampleProblemString of 
  Right x -> x
  Left _ -> undefined -- can't happen
