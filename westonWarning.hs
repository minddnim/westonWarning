import Control.Monad
import Control.Arrow
import System.Environment(getArgs)
import Data.List
import Data.Ord
import Text.Printf

main :: IO()
main = do
  args <- getArgs
  logFiles <- mapM readFile args
  let contents = map lines logFiles
  mapM_ (\(t,s) -> printf "%f\t%s\n" t s) $ conv contents 0 []

conv :: [[String]] -> Int -> [[(Double, String)]] -> [(Double, String)]
conv [] _ ret = sortBy (comparing fst) $ concat ret
conv (f:fs) n ret = conv fs (n+1) (fileLog:ret)
  where fLog = convLog 0.0 [] f
        fileLog = map (second (replicate n '\t' ++)) fLog

convLog :: Double -> [(Double, String)] -> [String] -> [(Double, String)]
convLog _ ret [] = ret
convLog t ret (l:ls) = convLog time (r:ret) ls
  where r@(time, _) = parse t l

parse :: Double -> String -> (Double, String)
parse time l | length l <= 12 = (time, l)
             | e == ']' && b == '[' = (read t :: Double, str)
             | otherwise = (time, l)
  where (b:t, e:str) = splitAt 12 l