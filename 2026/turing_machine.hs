exampleA code =
  let cardA = length (filter (==4) code) + 1
  in cardA

exampleB code@[t,s,c] =
  let cardA = if t == 1 then 1 else 2
      cardB = case compare t 3 of
                LT -> 1
                EQ -> 2
                GT -> 3
      cardC = case compare s 3 of
                LT -> 1
                EQ -> 2
                GT -> 3
      cardD = length (filter (==1) code) + 1
  in [cardA, cardB, cardC, cardD]

codes = [(t,s,c) | t <- [1..5], s <- [1..5], c <- [1..5]]
showCode (t,s,c) = show t ++ "\t" ++ show s ++ "\t" ++ show c
main = mapM_ (putStrLn . showCode) codes
-- ([1,1,1],[1,1,1,4])
-- ([1,3,1],[1,1,2,3])

