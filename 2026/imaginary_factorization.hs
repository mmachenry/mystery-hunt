import Math.NumberTheory.Primes
import Data.List (intersperse)

nums = [
  9903552973,
  241782329,
  3146964869,
  215915369,
  2629518821,
  2537276909,
  25510684337,
  2559143957,
  482739197,
  8903894677,
  1028854741,
  3041152021,
  1372902373,
  843025553,
  3866809733,
  270931121,
  207515981,
  7974863189,
  301786817,
  1287594293,
  232157309,
  339041009,
  2132673077,
  353887733,
  368681561,
  210173081,
  54921162917,
  263047769,
  747139697,
  553949117,
  457551377,
  1479176597,
  442149221,
  922769993,
  476141069,
  308724149,
  340514081,
  285007469,
  469894277,
  499756193,
  203931521,
  248315117,
  238197473,
  649834517,
  1073039021,
  316990049,
  1028000741,
  7152154301
  ]

factorNMinusOne :: Integer -> [(Prime Integer, Word)]
factorNMinusOne n = factorise (n-1)

showFactors :: [(Prime Integer, Word)] -> String
showFactors l = concat $ intersperse " * " $ map showFactor l

showFactor :: (Prime Integer, Word) -> String
showFactor (p, 1) = show (unPrime p)
showFactor (p, e) = show (unPrime p) ++ "^" ++ show e

main = mapM_ (putStrLn . showFactors . factorNMinusOne) nums
