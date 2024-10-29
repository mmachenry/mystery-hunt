module Romans where

import Data.Char (ord)
import Text.Numeral.Roman
import Data.List.Split
import Data.Maybe (catMaybes, fromJust)
import Data.List (product, elem, stripPrefix)

cRowSolutions :: String -> Int -> [(String, [Int], String)]
cRowSolutions constraints targetProduct =
  let possible :: [String]
      possible = cAllPossibleRows constraints
      both = catMaybes (map romanToBoth possible)
  in filter sumsUp (filter noDupes (filter noGt39 both))
  where sumsUp (_,nums,_) = product nums == targetProduct
        noGt39 (_,nums,_) = filter (>39) nums == []
        noDupes (_,nums,_) = hasNoDupes nums

rowSolutions :: Int -> Int -> [(String, [Int], String)]
rowSolutions numSquares targetProduct =
  let possible :: [String]
      possible = allPossibleRows numSquares
      both = catMaybes (map romanToBoth possible)
  in filter sumsUp (filter noDupes (filter noGt39 both))
  where sumsUp (_,nums,_) = product nums == targetProduct
        noGt39 (_,nums,_) = filter (>39) nums == []
        noDupes (_,nums,_) = hasNoDupes nums

romanToBoth :: String -> Maybe (String, [Int], String)
romanToBoth str = case convertToArabic str of
  Just ns -> Just (str, ns, filter (/='*') str)
  Nothing -> Nothing

hasNoDupes :: [Int] -> Bool
hasNoDupes [] = True
hasNoDupes (e:r) = if elem e r then False else hasNoDupes r

allRomans :: Int -> [String]
allRomans 0 = [""]
allRomans numSquares = do
  firstChar <- "IVX"
  rest <- allRomans (numSquares - 1)
  return $ firstChar : rest

cAllRomans :: String -> [String]
cAllRomans "" = [""]
cAllRomans (c:constraints) = do
  firstChar <- if c == '_' then "IVX" else [c]
  rest <- cAllRomans constraints
  return $ firstChar : rest

maybeMult :: String -> [String]
maybeMult str = map (filter (/= ' ')) (aux str)
  where aux :: String -> [String]
        aux [c] = [[c]]
        aux (first:rest) = do
          op <- [' ', '*']
          others <- aux rest
          return $ first:op:others

allPossibleRows :: Int -> [String]
allPossibleRows numSquares =
  concatMap maybeMult (allRomans numSquares)

cAllPossibleRows :: String -> [String]
cAllPossibleRows constraints =
  concatMap maybeMult (cAllRomans constraints)

convertToArabic :: String -> Maybe [Int]
convertToArabic row =
  let romans = splitOn "*" row
  in case removeJust (map fromRoman romans) of
          Just ns -> Just ns
          Nothing -> Nothing

removeJust :: [Maybe a] -> Maybe [a]
removeJust [] = Just []
removeJust (Just x:r) = case removeJust r of
  Nothing -> Nothing
  Just rs -> Just (x : rs)
removeJust (Nothing:_) = Nothing

-----------------------------------------------------------------
trd (_,_,c) = c

{-
[("IIVIIV","IIIIIV","XIXVII","IIXXIVII"),
 ("IIVIIV","IIIIIV","XIXVII","IIXXIVII"),
 ("IIVIIV","IIIIVI","XIXVII","IIXXIVII"),
 ("VIIIIV","IIIIIV","XIXVII","IIXXIVII"),
 ("VIIIIV","IIIIIV","XIXVII","IIXXIVII"),
 ("VIIIIV","IIIIVI","XIXVII","IIXXIVII"),
 ("VIIIIV","IIVIII","XIXVII","IIXXIVII"),
 ("VIIIVI","IIIIIV","XIXVII","IIXXIVII"),
 ("VIIIVI","IIIIIV","XIXVII","IIXXIVII"),
 ("VIIIVI","IIIIVI","XIXVII","IIXXIVII"),
 ("VIIIVI","IIVIII","XIXVII","IIXXIVII"),
 ("VIVIII","IIIIIV","XIXVII","IIXXIVII"),
 ("VIVIII","IIIIIV","XIXVII","IIXXIVII"),
 ("VIVIII","IIIIVI","XIXVII","IIXXIVII"),
 ("XIIIIV","IIIIIV","XIXVII","IIXXIVII"),
 ("XIIIIV","IIIIIV","XIXVII","IIXXIVII"),
 ("XIIIIV","IIIIVI","XIXVII","IIXXIVII"),
 ("XIIIIV","IIVIII","XIXVII","IIXXIVII"),
 ("XIIIVI","IIIIIV","XIXVII","IIXXIVII"),
 ("XIIIVI","IIIIIV","XIXVII","IIXXIVII"),
 ("XIIIVI","IIIIVI","XIXVII","IIXXIVII"),
 ("XIIIVI","IIVIII","XIXVII","IIXXIVII")]
-}
topRights = [(row1,row2,row3,row4)
  | row1 <- map trd (rowSolutions 6 48)
  , row2 <- map trd (rowSolutions 6 12)
  , row3 <- map trd (rowSolutions 6 133)
  , row4 <- map trd (rowSolutions 8 294)
  -- , let col1 = [row1!!0,row2!!0,row3!!0,row4!!3]
  , let col2 = [row1!!1,row2!!1,row3!!1,row4!!2]
  , let col3 = [row1!!2,row2!!2,row3!!2,row4!!3]
  , let col4 = [row1!!3,row2!!3,row3!!3,row4!!4]
  , canBe col2 18
  , canBe col3 100
  , canBe col4 12
  --, let col5 = [row1!!4,row2!!5,row3!!5,row4!!5]
  --, let col6 = [row1!!4,row2!!5,row3!!5,row4!!5]
  ]

{-
[("IIIVX","VIIXII","XXXIVI","VIIVII","VXV","IXIV","VIVI","XIII"), -- failed
 ("IVIXX","VIIXII","XVIIII","VIIVII","VXV","IXIV","XIII","XIII")]
-}
bottomRights = [(row1, row2, row3, row4, col1, col2, col3, col4)
  | row1 <- map trd (rowSolutions 5 80)
  , row2 <- map trd (rowSolutions 6 72)
  , row3 <- map trd (rowSolutions 6 34)
  , row4 <- map trd (rowSolutions 6 48)
  , let col1 = map (!!0) [row2,row3,row4]
  , let col2 = [row1!!2,row2!!3,row3!!3,row4!!3]
  , let col3 = [row1!!3,row2!!4,row3!!4,row4!!4]
  , let col4 = [row1!!4,row2!!5,row3!!5,row4!!5]
  , canBe col1 75
  , canBe col2 14
  , canBe col3 30
  , canBe col4 20
  ]

{-
[("IIIIXVII","IIVVIX","IIIIII","IIIIXV"),
 ("IIIIXVII","IIVVIX","IIIIII","IIIIXV"),
 ("IIIIXVII","IIVVIX","IIIIII","IIIIXV"),
 ("IIIIXVII","IIVVIX","IIIIII","IIIIXV"),
 ("IIIIXVII","IIVVIX","IIIIII","IIIIXV"),
 ("IIIIXVII","IIVVIX","IIIIII","IIIIXV"),
 ("IIIIXVII","IIVVIX","IIIIII","IIIIXV"),
 ("IIIIXVII","IIVVIX","IIIIII","IIIIXV"),
 ("IIIIXVII","IIVVIX","IIIIII","IIIIXV"),
 ("IIIIXVII","IIVVIX","IIIIII","IIIIXV"),
 ("IIIIXVII","IIVVIX","IIIIII","IIIIXV"),
 ("IIIIXVII","IIVVIX","IIIIII","IIIIXV")]
-}

bottomLefts = [(row1,row2,row3,row4)
  | row1 <- map trd (cRowSolutions "III____I" 162)
  , row2 <- map trd (cRowSolutions "II____" 180)
  , row3 <- map trd (cRowSolutions "II___I" 6)
  , row4 <- map trd (cRowSolutions "II____" 90)
  -- , (row1!!6 == 'I' && row2!!5 == 'X')
  --    || (row1!!6 == 'V' && row2!!5 == 'V')
  , let col1 = [row1!!3,row2!!2,row3!!2,row4!!2]
  , let col2 = [row1!!4,row2!!3,row3!!3,row4!!3]
  , let col3 = [row1!!5,row2!!4,row3!!4,row4!!4]
  , let col4 = ['I', row1!!6, row2!!5, row3!!5, row4!!5]
  , canBe col1 10
  , canBe col2 100 
  , canBe col3 54
  , canBe col4 28
  ]

--[("IXVIII","VIIIXI","IIXVIV","IVVII"),("IXVIII","VIIIXI","IIXXIV","IVVII")]

topLefts = [(row1,row2,row3,row4)
  | row1 <- map trd $ rowSolutions 6 72
  , row2 <- map trd $ rowSolutions 6 63
  , row3 <- map trd $ rowSolutions 6 160
  , row4 <- map trd $ rowSolutions 5 24
  , let col1 = [row1!!0,row2!!0,row3!!0,row4!!0]
  , let col2 = [row1!!1,row2!!1,row3!!1,row4!!1]
  , let col3 = [row1!!2,row2!!2,row3!!2,row4!!2]
  , let col4 = [row1!!5,row2!!5,row3!!5]
  , canBe col1 8
  , canBe col2 44
  , canBe col3 90
  , canBe col4 10
  ]
  
canBe :: String -> Int -> Bool
canBe str targetProduct =
  any (== targetProduct) $ map product $
    catMaybes (map convertToArabic (maybeMult str))

anyAllSame :: [String] -> String
anyAllSame (primary:possibles) =
  map markSames [0..length primary -1]
  where markSames n =
          if all (== primary!!n) (map (!!n) possibles) 
          then primary!!n
          else '_'

showPossible numSquares targetProduct =
  let solutions = rowSolutions numSquares targetProduct
  in do mapM_ print solutions
        putStrLn (anyAllSame (map trd solutions))

---------------------------------------------------------------

translateString :: String -> [String]
translateString str =
  let nums = map (\c->ord c - ord 'A' + 1) str
      romans = map toRoman nums
  in romans

romanSplit :: String -> [[(String, Integer)]]
romanSplit "" = [[]]
romanSplit str = do
  (roman, arabic) <- getFirsts str
  rest <- romanSplit (fromJust (stripPrefix roman str))
  return $ (roman, arabic):rest

getFirsts :: String -> [(String, Integer)]
getFirsts str =
  let subs = map (\n->take n str) [1..length str]
      both = map (\sub->(sub,fromRoman sub)) subs
  in eliminateMaybe both

eliminateMaybe :: [(String, Maybe Integer)] -> [(String, Integer)]
eliminateMaybe [] = []
eliminateMaybe ((s,Just n):rest) = (s,n):eliminateMaybe rest
eliminateMaybe ((s,Nothing):rest) = eliminateMaybe rest
