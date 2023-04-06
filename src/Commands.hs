module Commands where


data CommandEffect = CommandEffect {
    short :: String
    , verbose :: String
}
    deriving (Show, Eq)