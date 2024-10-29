import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (replicate, delete, intersperse)
import Data.Maybe (fromMaybe)

type Production = (Char, String)
type Productions = [Production]

initProductions = [
    ('2', "3")
  , ('A', "LLNNNRSTW")
  , ('B', "ER")
  , ('C', "AEKK")
  , ('D', "IJR")
  , ('E', "AACEMNNORRRSSSTTUV")
  , ('F', "OTUU")
  , ('G', "H")
  , ('H', "DEEEIOOT")
  , ('I', "CCEEGNNT")
  , ('J', "A")
  , ('K', "AE")
  , ('L', "EEMS")
  , ('M', "BIIO")
  , ('N', "AADDEEFHIPSTUWY")
  , ('O', "!FFNNRRT")
  , ('P', "EIO")
  , ('R', ".2INSTTU")
  , ('S', "'AAEEHMNOPSSU")
  , ('T', "'EEHHHLOSUY")
  , ('U', "CMNNNNS")
  , ('V', "E")
  , ('W', "HI")
  , ('Y', "S")
  , ('.', "S")
  , ('\'',"HS")
  ]

printProductions :: Productions -> IO ()
printProductions p = mapM_ printProduction p

printProduction :: Production -> IO ()
printProduction (lhs, rhs) =
  putStrLn ([lhs] ++ " -> " ++ (intersperse ',' rhs))

removeAllStrings :: Productions -> [String] -> Productions
removeAllStrings ps strs = foldl (flip removeString) ps strs

removeString :: String -> Productions -> Productions
removeString "" ps = ps
removeString [_] ps = ps
removeString (c1:c2:cs) ps =
  map (removeChar c1 c2) (removeString (c2:cs) ps)

removeChar :: Char -> Char -> Production -> Production
removeChar c1 c2 (lhs, str) =
  (lhs, if lhs == c1 then carefulDelete c2 str else str)
  where carefulDelete c s =
            if elem c s
            then delete c s
            else error "c is not in s"

reverseProductions :: Productions -> Productions
reverseProductions ps =
  let charactersForLHS =
          Set.unions (map (Set.fromList . snd) ps)
  in map (buildRule ps) (Set.toList charactersForLHS)

buildRule :: Productions -> Char -> Production
buildRule ps lhs =
  let oldRHSCounts = getRHSCounts lhs ps
      newRHS = createString oldRHSCounts
  in (lhs, newRHS)

getRHSCounts ::  Char -> Productions -> [(Char, Int)]
getRHSCounts target p = map aux p
  where aux (lhs, rhs) =
          let chars = filter (== target) rhs
          in (lhs, length chars)

createString :: [(Char, Int)] -> String
createString counts = concatMap (\(c,n)->replicate n c) counts

countInsAndOuts :: Productions -> [(Char, Int, Int)]
countInsAndOuts p = map (charInsAndOuts . fst) p
  where charInsAndOuts c = (c, ins c, outs c)
        ins c = sum $ map snd $ getRHSCounts c p
        outs c = case lookup c p of
                   Nothing -> error "not found"
                   Just rhs -> length rhs

---------

buildWords :: Productions -> Char -> Int -> [String]
buildWords p _ 0 = [""]
buildWords p c n = do
  nextLetter <- fromMaybe [] (lookup c p)
  restOfWord <- buildWords (removeString [c,nextLetter] p) nextLetter (n-1)
  return $ c:restOfWord
