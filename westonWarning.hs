import Control.Monad
import Control.Applicative()
import System.Environment(getArgs)

main :: IO()
main = do
  args <- getArgs
  forM_ args $ \readFilePath -> do
    contents <- readFile readFilePath
  writeFile "westonWarning.log" logStr
