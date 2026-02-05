{-# LANGUAGE ScopedTypeVariables #-}

-- Replacements, a puzzle from Mystery Hunt 2026

import Data.SBV
import qualified Data.Matrix as M
import Data.Matrix ((!))

main = do
  result <- solution
  print result

mkMatrix :: String -> Int -> Int -> Symbolic (M.Matrix SInteger)
mkMatrix name rows cols = do
  xs <- mapM sInteger
        [ name ++ "_" ++ show i ++ "_" ++ show j
        | i <- [1..rows], j <- [1..cols]
        ]
  pure $ M.fromList rows cols xs

solution = sat $ do
-- solution = allSat $ do
  grid <- mkMatrix "grid" 6 6

  mapM_ (\v -> constrain $ v .>= 1 .&& v .<= 6) (M.toList grid)
  mapM_ (\row -> constrain $ distinct row) (M.toLists grid)
  mapM_ (\col -> constrain $ distinct col) (M.toLists (M.transpose grid))

  constrain $ grid!(1,1) + grid!(1,2) .== 9
  constrain $ grid!(1,3) * grid!(1,4) .== 18
  constrain $ abs(grid!(1,5) - grid!(1,6)) .== 1
  constrain $ grid!(2,1) * grid!(3,1) .== 18
  -- ... the above is only the constaints for one row, we solved this before I finished
  -- but this is a good example of how to do constraint puzzles with grids
