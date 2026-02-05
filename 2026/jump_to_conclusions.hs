import System.IO (readFile)
import Data.List (lines)

patterns = [
  [6,3,7,2,8,9,5,8,1,1],
  [6,10,9,7,8,9],
  [6,4,3,7,2,1,8],
  [4,3,7,6,8,9],
  [4,9,3,6,6,8,7],
  [3,7,6,8,9,6,4,3,7,8]
  ]

itWorks :: String -> Bool
itWorks s =
  length s == 10
  && s!!0 == s!!7
  && s!!1 == s!!8
  && s!!2 == s!!5
  && s!!3 == s!!9

main :: IO ()
main = do
    contents <- readFile "/usr/share/dict/words"
    let words = lines contents
    let good = filter itWorks words
    mapM_ putStrLn good
