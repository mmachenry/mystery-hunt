import Data.List
import Data.Char

answers = [
  "OAK GROVE",
  "FOOLHARDINESS",
  "BEEBLEBROX",
  "ONE-TRICK PONY",
  "PARALLEL LINES",
  "QUANTIZATION",
  -- "VALLICELLIANA",
  "BACON",
  "GOLDEN BAMBOO",
  -- "MAMMOTH HOT SPRINGS",
  "YARDSTICK",
  "AUTOBIOGRAPHY",
  "FILMOGRAPHIES",
  "GOSPEL OF ST LUKE",
  -- "GET BOGGED DOWN WITH",
  "",
  "",
  "ONE IN A MILLION",
  "RECORDANT",
  -- "REPROACHLESSNESS",
  "",
  -- "BEECHWOOD",
  "BEDTIME STORIES",
  "JAVELIN THROW",
  "MARTIN LUTHER KING JR",
  "CARILLONNEURS"
  ]

normalize = filter isAlpha

uniqueLetters :: String -> String
uniqueLetters [] = []
uniqueLetters (c:cs) =
  if elem c cs
  then uniqueLetters (delete c cs)
  else c : uniqueLetters cs

uniqueIn word l = length (filter (==l) word) == 1

groupByUorP word =
  groupBy
    (\a b -> groupable a == groupable b)
    word
  where groupable x = uniqueIn word x || elem x "PRECIOUS"
