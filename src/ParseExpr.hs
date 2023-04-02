module ParseExpr where
import Game
import Player (Player(..))
import Data.List (sortBy)
import Text.Read (readMaybe)

data GameExpr =
    Any GameExpr
    | All GameExpr
    | Greatest GameExpr
    | Players GameExpr
    | Score
    | Hand
    | IsEqual GameExpr GameExpr
    | GValue Int
    | IsEmpty GameExpr
    | If GameExpr [GameExpr]
    | Swap GameExpr GameExpr
    | Shuffle GameExpr
    | Deck
    | Pile
    | Take GameExpr GameExpr GameExpr
    | Null
    deriving (Show)


test :: IO ()
test = do
    print (parseIfString (["isEmpty", "deck"],[["swap","pile","deck"], ["shuffle","deck"], ["take","1","deck","pile"]]))


parseIfString :: ([String], [[String]]) -> GameExpr
parseIfString (xs, ys) = If (parseString xs) (map parseString ys)


parseString :: [String] -> GameExpr
parseString [] = Null
parseString (x:xs) = case x of
    "any" -> Any (parseString xs)
    "greatest" -> Greatest (parseString xs)
    "players" -> Players (parseString xs)
    "score" -> Score
    "hand" -> Hand
    "isEqual" -> IsEqual (parseString [head xs]) (parseString [xs !! 1])
    "isEmpty" -> IsEmpty (parseString xs)
    "deck" -> Deck
    "shuffle" -> Shuffle (parseString xs)
    "swap" -> Swap (parseString [head xs]) (parseString [xs !! 1])
    "pile" -> Pile
    "take" -> Take (parseString [head xs]) (parseString [xs !! 1]) (parseString [xs !! 2])
    s -> maybe Null GValue (readMaybe s :: Maybe Int)

createEndCon :: GameExpr -> Game -> Game
createEndCon (Any (Players (IsEmpty Hand))) g = g { endCon = any (null . hand) . players }
createEndCon (Any (Players (IsEqual Score (GValue n)))) g = g { endCon = any ((== n) . pScore) . players }


createWinCon :: GameExpr -> Game -> Game
createWinCon (Any expr) g = createWinCon expr g
createWinCon (All expr) g = createWinCon expr g
createWinCon (Greatest (Players Score)) g = g { winCon = sortBy (\p1 p2  -> compare (pScore p1) (pScore p2)) . players }
createWinCon (Players (IsEqual Score (GValue n))) g = g { winCon = sortBy (\p1 p2 -> compare (distance n (pScore p1)) (distance n (pScore p2))) . players }
    where distance x y = abs (x - y)
createWinCon (Players (IsEmpty Hand)) g = g { winCon = sortBy (\p1 p2 -> compare (length $ hand p1) (length $ hand p2)) . players }