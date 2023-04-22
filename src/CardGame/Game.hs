module CardGame.Game (
    GameState (..)
    , Game (..)
    , dealCards
    , createEmptyGame
    , gameActions
) where


import CardGame.Player ( deal, Player )
import CardGame.Card ( Card )

import Data.CircularList ( CList, empty )
import Feature (Feature)
import CardGame.PlayerMove (Move)
import CDSL.CDSLExpr (CDSLExpr)

data GameState = Start | TurnStart | TurnEnd | RoundStart | RoundEnd | End
    deriving (Show, Eq)

data Game = Game {
    gameName :: String
    , playerMoves :: [(Move, Bool)]
    , cardGen :: [Card]
    , discard :: [Card]
    , turnOrder :: [CDSLExpr]
    , cardSuits :: [CDSLExpr]
    , cardRanks :: [CDSLExpr]
    , cardEffects :: [CDSLExpr]
    , deck :: [Card]
    , pile :: [(Card, Maybe Card)]
    , players :: CList Player
    , endCon :: [Game -> Bool]
    , winCon :: [Game -> [Player]]
    , state :: GameState
    , rules :: [(Feature, [CDSLExpr])]
    , actions :: [(GameState, [(Game -> IO Game, Bool)])]
    , rounds :: Int
    , canPlaceCard :: [Game -> Card -> Bool]
    , cpcException :: [CDSLExpr]
}



dealCards :: Game -> Int -> Game
dealCards game n = do
    let (plrs', deck') = deal n (deck game) (players game)
    game { players = plrs', deck = deck'}


createEmptyGame :: Game
createEmptyGame = Game {
                    gameName = "Default Game"
                    , turnOrder = []
                    , discard = []
                    , cpcException = []
                    , playerMoves = []
                    , cardGen = []
                    , cardSuits = []
                    , cardRanks = [] 
                    , cardEffects = []
                    , deck = []
                    , pile = []
                    , players = empty
                    , endCon = []
                    , winCon = []
                    , state = Start
                    , rules = []
                    , actions = []
                    , rounds = 0
                    , canPlaceCard = []
                    }

gameActions :: [[Game -> IO Game]] -> Game -> IO Game
gameActions xs = go (concat xs)
    where
        go :: [Game -> IO Game] -> Game -> IO Game
        go [] gm = return gm
        go (f:fs) gm = f gm >>= go fs
