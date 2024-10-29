import Data.List (elem)

green = ["BIRTHDAY", "STAPLER", "KILO", "ADDLE"]
magenta = ["UPCYCLE", "HABIT"]
cyan = ["NOLTE"]
black = [ "BLANKET", "BREAD", "COIN", "DRILL", "HAT", "KEYCHAIN", "LUTE", "PUMP", "YODEL" ]

(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 1 |>

add = flip (++)

sub [] w1 = w1
sub (c:cs) w1 = takeWhile (/=c) w1 ++ sub cs (tail (dropWhile (/=c) w1))

result =
  "BIRTHDAY"
  |> add "STAPLER"
  |> sub "HAT"
  |> add "KILO"
  |> add "ADDLE"
  |> sub "DRILL"
  |> sub "BREAD"
  |> add "UPCYCLE"
  |> add "LONE"
  -- |> add "HABIT"
  -- |> sub "KEYCHAIN"
  -- |> sub "YODEL"
  -- |> sub "LUTE"
  -- |> add "LATHE"
  -- |> add "UNKEMPT"
  -- |> sub "PUMP"
  -- |> sub "BLANKET"
  -- |> add "NOLTE"
  -- |> sub "CENT"
