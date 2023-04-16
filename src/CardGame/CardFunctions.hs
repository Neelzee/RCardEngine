module CardGame.CardFunctions where
import CDSL.CDSLExpr
import CardGame.Card
import Data.List (intercalate)


defaultCardSuits :: [CDSLExpr]
defaultCardSuits = [Text "Hearts", Text "Clubs", Text "Diamonds", Text "Spades"]

defaultCardRanks :: [CDSLExpr]
defaultCardRanks = [Text "Ace", Text "Two", Text "Three", Text "Four", Text "Five", Text "Six", Text "Seven", Text "Eight", Text "Nine", Text "Ten", Text "Jack", Text "Queen", Text "King"]

defaultCardValues :: [CDSLExpr]
defaultCardValues = map Numeric [1..13]

defaultCardDeck :: Deck
defaultCardDeck = [ Card s n sc | (Text s) <- defaultCardSuits, (Text n) <- defaultCardRanks, (Numeric sc) <- defaultCardValues ]

makeDeck :: [CDSLExpr] -> [CDSLExpr] -> [CDSLExpr] -> Deck
makeDeck suits ranks values =
  let paddedValues = values ++ repeat (Numeric 0)
      rankValuePairs = zipWith (curry (\(Text r, Numeric i) -> (r, i))) ranks paddedValues
      suitCards s = map (uncurry (Card s)) rankValuePairs
  in concatMap (suitCards . (\(Text s) -> s)) suits

prettyPrintCards :: [Card] -> IO ()
prettyPrintCards xs = putStrLn (intercalate ", " (map show xs))