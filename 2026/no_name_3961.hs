{-# LANGUAGE ScopedTypeVariables #-}

-- No Name 3961, a puzzle from Mystery Hunt 2026
-- this solves on of the puzzles. I have edited to program to
-- fit each one as I go

import Data.SBV
import qualified Data.Matrix as M
import Data.Matrix ((!))

main = do
  result <- solution
  print result

mkMatrix :: String -> Int -> Int -> Symbolic (M.Matrix SBool)
mkMatrix name rows cols = do
  xs <- mapM sBool
        [ name ++ "_" ++ show i ++ "_" ++ show j
        | i <- [1..rows], j <- [1..cols]
        ]
  pure $ M.fromList rows cols xs

neighbors :: M.Matrix SBool -> Int -> Int -> [SBool]
neighbors m r c =
  [ m ! (r', c')
  | r' <- [r-1 .. r+1]
  , c' <- [c-1 .. c+1]
  , r' >= 1, r' <= M.nrows m
  , c' >= 1, c' <= M.ncols m
  ]

countBombsNeighbors :: M.Matrix SBool -> (Int, Int) -> SInteger
countBombsNeighbors m (r, c) =
  sum [ ite d 1 0 | d <- neighbors m r c ]

--solution = sat $ do
solution = allSat $ do
  grid <- mkMatrix "grid" 7 7

  constrain $ grid!(1,1) -- constrain which of the two answers to use
  constrain $ countBombsNeighbors grid (1,3) .== 2
  constrain $ countBombsNeighbors grid (1,7) .== 3

  constrain $ countBombsNeighbors grid (2,1) .== 3
  constrain $ countBombsNeighbors grid (2,2) .== 6
  constrain $ countBombsNeighbors grid (2,5) .== 4

  constrain $ countBombsNeighbors grid (3,7) .== 4

  constrain $ countBombsNeighbors grid (4,1) .== 3
  constrain $ countBombsNeighbors grid (4,3) .== 4
  constrain $ countBombsNeighbors grid (4,4) .== 5
  constrain $ countBombsNeighbors grid (4,6) .== 4

  constrain $ countBombsNeighbors grid (5,1) .== 3
  constrain $ countBombsNeighbors grid (5,4) .== 2
  constrain $ countBombsNeighbors grid (5,5) .== 1
  constrain $ countBombsNeighbors grid (5,7) .== 3

  constrain $ countBombsNeighbors grid (6,3) .== 4

  constrain $ countBombsNeighbors grid (7,1) .== 3
  constrain $ countBombsNeighbors grid (7,4) .== 3
  constrain $ countBombsNeighbors grid (7,7) .== 2
