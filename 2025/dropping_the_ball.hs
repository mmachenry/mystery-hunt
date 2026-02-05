-- This program was written to solve Dropping the Ball
-- from Mystery Hunt 2025
-- https://www.two-pi-noir.agency/puzzles/dropping_the_ball

import Data.List (delete)

data Color = Red | Black deriving (Eq, Show)

letters = [
  "abco",
  "cdfm",
  "ainr",
  "anor",
  "adio",
  "elrw",
  "dirs",
  "ikln",
  "aors",
  "dekt",
  "rsyy",
  "oopt",
  "imss",
  "aepy",
  "cdeo",
  "anot",
  "hilr",
  "entu",
  "enrs",
  "otyy",
  "egnr",
  "bhno",
  "aaht",
  "elno",
  "orrt",
  "rrss",
  "hosu",
  "lstw",
  "dhmr",
  "hnor",
  "aisu",
  "aeou",
  "enno",
  "enrt",
  "gisx",
  "ggnp"
  ]

nums = [32,15,19,4,21,2,25,17,34,6,27,13,36,11,30,8,32,10,5,24,16,33,1,20,14,31,9,22,18,29,7,28,12,35,3,26]

color = take 36 (concat (repeat [Red, Black]))

columns = zip3 nums color letters

low (n,_,_) = n <= 18
high (n,_,_) = n > 18
fEven (n,_,_) = n `mod` 2 == 0
fOdd (n,_,_) = n `mod` 2 == 1
red (_,Red,_) = True
red (_,Black,_) = False
black (_,Red,_) = False
black (_,Black,_) = True

nutrimatic f cols = concat $ map format (map (\(_,_,ls)->ls) (filter f cols))
  where format ls = "[" ++ ls ++ "]"

cull cols word = zipWith cullCell cols word
  where cullCell col@(n,c,ls) letter =
          if letter == '_'
          then col
          else (n,c,delete letter ls)

rowLow =   "_d_o_w_n_d_o_y_o_ur_e_a_r_s_h_a_n_g_"
rowEven =  "b__r_e__ak__m_on_e_yn__ot__wr__on__g"
rowRed =   "c_r_o_s_s_r_i_d_i_n_g_h_o_o_d_s_o_x_"
rowBlack = "_m_a_r_k_e_t_p_a_n_t_h_e_r_s_h_e_e_p"
rowOdd =   "_fa_i_rl__yp_a__r_e__nt__sh__ou__rs_"
rowHigh =  "a_n_d_d_r_y_s_c_h__o_o_l_s_t_r_u_n_g"

colsRemain = without columns [rowLow,rowEven,rowRed,rowBlack,rowOdd,rowHigh]

without cols sols =
  case sols of
    [] -> cols
    (s:ss) -> without (cull cols s) ss
