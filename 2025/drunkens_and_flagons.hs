{-# LANGUAGE ScopedTypeVariables #-}

-- This is an implementation of the Drunkens and Flagons puzzle from MIT Mystery Hunt 2025
-- https://puzzles.mit.edu/2025/hunt/puzzles/drunkens_and_flagons
-- This is a solve of only the grid puzzle portion whichis is 90% of the puzzle. Not the extraction.

import Data.SBV
import qualified Data.Map as Map
import Data.Map (Map, (!))

main = do
  result <- solution
  print result

mkGrid :: [[String]] -> Symbolic (Map String SInteger)
mkGrid labels = do
  sVars <- mapM gridCategory labels
  pure (Map.unions sVars)
  where gridCategory ls = do
          vs <- mapM (\str->sInteger str) ls
          mapM_ (\v -> constrain $ v .>= 1 .&& v .<= literal (fromIntegral (length ls))) vs
          constrain $ distinct vs
          pure (Map.fromList (zip ls vs))

-- solution = sat $ do
solution = allSat $ do
  grid <- mkGrid [
            ["left1", "left2", "left3", "left4", "left5", "left6", "left7", "left8"],
            ["alison", "august", "bill", "gracie", "oliver", "patrick", "tabitha", "yaron"],
            ["artificer", "bard", "fighter", "monk", "paladin", "rogue", "sorcerer", "wizard"],
            ["ale", "fernet", "gin", "mead", "rye", "tequila", "vodka", "wine"],
            ["drank05", "drank07", "drank08", "drank09", "drank12", "drank13", "drank15", "drank21"]
            ]

  -- start of grid puzzle boilerplate
  alison <- sInteger "alison"
  august <- sInteger "august"
  bill <- sInteger "bill"
  gracie <- sInteger "gracie"
  oliver <- sInteger "oliver"
  patrick <- sInteger "patrick"
  tabitha <- sInteger "tabitha"
  yaron <- sInteger "yaron"
  let names = [alison, august, bill, gracie, oliver, patrick, tabitha, yaron]

  artificer <- sInteger "artificer"
  bard <- sInteger "bard"
  fighter <- sInteger "fighter"
  monk <- sInteger "monk"
  paladin <- sInteger "paladin"
  rogue <- sInteger "rogue"
  sorcerer <- sInteger "sorcerer"
  wizard <- sInteger "wizard"
  let classes = [artificer, bard, fighter, monk, paladin, rogue, sorcerer, wizard]

  ale <- sInteger "ale"
  fernet <- sInteger "fernet"
  gin <- sInteger "gin"
  mead <- sInteger "mead"
  rye <- sInteger "rye"
  tequila <- sInteger "tequila"
  vodka <- sInteger "vodka"
  wine <- sInteger "wine"
  let drinks = [ale, fernet, gin, mead, rye, tequila, vodka, wine]

  drank05 <- sInteger "drank05"
  drank07 <- sInteger "drank07"
  drank08 <- sInteger "drank08"
  drank09 <- sInteger "drank09"
  drank12 <- sInteger "drank12"
  drank13 <- sInteger "drank13"
  drank15 <- sInteger "drank15"
  drank21 <- sInteger "drank21"
  let dranks = [drank05, drank07, drank08, drank09, drank12, drank13, drank15, drank21]

  let drinkCount :: SInteger -> SInteger
      drinkCount d =
          ite (d .== drank05) 5 $
          ite (d .== drank07) 7 $
          ite (d .== drank08) 8 $
          ite (d .== drank09) 9 $
          ite (d .== drank12) 12 $
          ite (d .== drank13) 13 $
          ite (d .== drank15) 15 $
          ite (d .== drank21) 21 $
          0


  let allVars = concat [names, classes, drinks, dranks]
  mapM_ (\v -> constrain $ v .>= 1 .&& v .<= 8) allVars

  constrain $ distinct names
  constrain $ distinct classes
  constrain $ distinct drinks
  constrain $ distinct dranks

  -- end of grid puzzle boilerplate

  --Fighter: Ugh, what a brutal night. At least I got to see all of it, and have 15 drinks to boot!
  constrain $ fighter .== 8
  constrain $ fighter .== drank15

  --Sorcerer: That’s probably because you weren’t drinking liquor. Not that that stopped you from cleaving your tankards in twain. Liquor is where I went wrong though, I should have stuck to the lighter stuff.
  let liquors = [rye, fernet, gin, tequila, vodka]
  constrain $ sNot (sElem fighter liquors)
  constrain $ sElem sorcerer liquors

  --Bard: I didn’t have much, but Alison had even less than me. I just barely stayed awake to see her keel over before I passed out too.
  constrain $ bard ./= alison
  constrain $ drinkCount alison .< drinkCount bard
  constrain $ bard .== alison + 1

  --Artificer: I remember Tabitha and Yaron were having a drinking contest. They went drink-for-drink for a while, but Tabitha won by exactly one drink. She passed out right after him, too.
  constrain $ artificer ./= tabitha
  constrain $ artificer ./= yaron
  constrain $ drinkCount tabitha .== drinkCount yaron + 1
  constrain $ tabitha .== yaron + 1

  --Paladin: You made the right choice drinking wine, I imagine it’s best to drink something light while crafting. Now that I think about it, you spend all your free time making rings. How many rings do you even need?
  constrain $ artificer .== wine

  --Fighter: Indeed, our friend the Artificer had as many drinks as Gracie and Yaron combined!
  constrain $ fighter ./= gracie
  constrain $ fighter ./= yaron
  constrain $ artificer ./= gracie
  constrain $ artificer ./= yaron
  constrain $ drinkCount artificer .== drinkCount gracie + drinkCount yaron

  --Wizard: Alas, I fell one drink short of Gracie’s total.
  constrain $ wizard ./= gracie
  constrain $ drinkCount wizard .== drinkCount gracie - 1

  --Fighter: Tabitha did have fewer than ten drinks, but I know she didn’t have exactly eight—that was someone else.
  constrain $ fighter ./= tabitha
  constrain $ drinkCount tabitha .< 10
  constrain $ drinkCount tabitha ./= 8

  --Monk: Well, that’s not many drinks, now is it? Tabitha should know better than to drink things made from grapes, she’s allergic. August had a grape-based drink too and he’s fine.
  constrain $ monk ./= tabitha
  constrain $ monk ./= august
  let grapeBased = [wine, fernet]
  constrain $ sElem tabitha grapeBased
  constrain $ sElem august grapeBased

  --Rogue: You only think that’s not many because you weren’t drinking spirits. You know, one would think a Monk so fluid as yourself would be willing to switch it up a bit from their usual.
  constrain $ sNot (sElem monk liquors) -- liquors == spirits?

  --Sorcerer: And yet our dear Monk managed to put back far more drinks than you, even though you outlasted her!
  --let femaleNames = [alison, gracie, tabitha]
  --constrain $ sElem monk femaleNames
  constrain $ drinkCount monk .> drinkCount rogue
  constrain $ rogue .> monk

  --Rogue: Wow, there’s thirteen empty bottles of ale at our table.
  constrain $ ale .== drank13

  --Wizard: Way to deflect, you’re always so shifty. I suppose I should expect that from a Rogue, though. Anyways, I know you had a prime number of drinks, but it wasn’t thirteen.
  constrain $ sElem rogue [drank05, drank07]

  --Sorcerer: What on earth is a “prime number”, Patrick? You really learn that stuff at Wizard school? Nerd. No wonder your familiar flapped away.
  constrain $ wizard .== patrick

  --Paladin: Well then! You had the least to drink, comrade Sorcerer, everyone else had already surpassed your total of five drinks when you passed out. You’re usually so self-reflective—perhaps it’s time to do some thinking about whether you can keep up.
  constrain $ sorcerer .== drank05

  --Artificer: I heartily agree with that, my Paladin friend, and I admire your dedication to rooting out evil. As I recall, Bill was drinking mead, and he was still standing after I. We should all follow his example.
  constrain $ artificer ./= bill
  constrain $ bill .== mead
  constrain $ bill .> artificer

  --Bard: I agree with you as well, Artificer. You, Bill, and Tabitha were still drinking when I fell asleep.
  constrain $ bard ./= bill
  constrain $ bard ./= tabitha
  constrain $ artificer ./= bill
  constrain $ artificer ./= tabitha
  constrain $ artificer .> bard
  constrain $ bill .> bard
  constrain $ tabitha .> bard
  constrain $ bard .== 5

  --Paladin: Wow, whoever was drinking rye and gin didn’t have much at all. Those two have the fewest empty glasses. Meanwhile, tequila has the most empties of all the spirits.
  constrain $ paladin ./= rye
  constrain $ paladin ./= gin
  constrain $ sElem rye [drank05, drank07]
  constrain $ sElem gin [drank05, drank07]
  constrain $ drinkCount tequila .> drinkCount fernet
  constrain $ drinkCount tequila .> drinkCount gin
  constrain $ drinkCount tequila .> drinkCount vodka

  --Monk: Rye and gin had the fewest orders? That’s odd, because vodka and fernet aren’t very palatable.
  constrain $ monk ./= vodka
  constrain $ monk ./= fernet

  --Artificer: That’s obviously not a universal truth, because this set of nine empty glasses is for one of those spirits.
  constrain $ sElem drank09 [vodka, fernet]

  --Bard: I’m scraping the bottom of the figurative barrel here, but I remember seeing twelve identical branded glasses in front of someone who passed out before me.
  constrain $ drank12 .< bard

  --Wizard: What a useful detail. Boy, your memory is terrible. Is that why you always keep notes? Here’s one for you all: Oliver had exactly seven drinks before I saw him go under the table.
  constrain $ wizard ./= oliver
  constrain $ oliver .== drank07
  constrain $ wizard .> oliver

  --Rogue: Ugh. You’re all the worst. I’m never drinking gin ever again.
  constrain $ rogue .== gin

  --Everyone: Oh no! The bartender!

  --Bartender: One of you jerks had TWENTY ONE drinks and we had to clean up after ALL of you. And now you’re just standing around insulting each other? You all need an attitude adjustment.
