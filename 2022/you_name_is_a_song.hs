import Data.List
import Control.Monad

three = [
  "ANN",
  "CAT",
  --"DEE",
  -- "ENT",
  -- "IDA",
  "JOY",
  "LEE",
  "MAE",
  "ORR",
  -- "POI",
  "RAY"
  ]

four = [
  -- "BESS",
  -- "COLE",
  "DIDA",
  "DUFF",
  -- "ELLA",
  -- "EMMA",
  -- "HOST",
  -- "MAYO",
  -- "MIEL",
  "RITA",
  -- "SHIN",
  "TYRA"
  ]

five = [
  -- "ADELE",
  -- "CAMMY",
  -- "JENNA",
  "MOORE",
  -- "NONNA",
  -- "PAULA",
  -- "RAMEN",
  "YEATS"
  ]

choose = do
  a <- four
  b <- four
  guard (a /= b)
  return (a++ " " ++b)
