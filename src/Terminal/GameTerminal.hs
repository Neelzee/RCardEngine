module Terminal.GameTerminal where
import System.Console.ANSI
import Data.List (intercalate)

data StringBox =
    StringBox
    {
        top :: Char
        , topCorner :: Char
        , topPadding :: Int

        , bot :: Char
        , botCorner :: Char
        , botPadding :: Int

        , side :: Char
        , sidePadding :: (Int, Int)

        , content :: [String]
        , contentPaddingTop :: Int
        , contentPaddingBot :: Int
        , contentPaddingLeft :: Int
        , contentPaddingRight :: Int
    }

instance Show StringBox where
    show b = do
        let center = map (\c -> [side b] ++ replicate (contentPaddingLeft b + fst (sidePadding b)) ' ' ++ c ++ replicate (contentPaddingRight b + snd (sidePadding b)) ' ' ++ [side b]) (content b)
        let center' =
                replicate (contentPaddingTop b) ([side b] ++ replicate (contentPaddingLeft b + maximum (map length center) + contentPaddingRight b) ' ' ++ [side b])
                ++ center
                ++ replicate (contentPaddingBot b) ([side b] ++ replicate (contentPaddingLeft b + maximum (map length center) + contentPaddingRight b) ' ' ++ [side b])
        let tL = length (head center')
        let tp = [topCorner b] ++ replicate (tL - 2) (top b) ++ [topCorner b]
        let bt = [botCorner b] ++ replicate (tL - 2) (bot b) ++ [botCorner b]
        let s = [side b] ++ replicate (tL - 2) ' ' ++ [side b]
        let tSide = intercalate "\n" (replicate (topPadding b) s)
        let bSide = intercalate "\n" (replicate (topPadding b) s)
        tp ++ tSide ++ intercalate "\n" center' ++ bSide ++ bt


data Terminal =
    Terminal
    {
        header :: [String]
        , headerPadding :: Int
        , headerBorder :: Char

        , leftBody :: [String]
        , rightBody :: [String]
        , lrBodyPadding :: Int
        , lrBodyBorder :: Char

        , centerBody :: [String]
        , cBodyPadding :: Int
        , cBodyBorder :: Char
    }

printTerminal :: Terminal -> IO ()
printTerminal t = do
    undefined



-- Takes in a string, and returns in surrounded in a box
boxString :: String -> (Int, Int, Int, Int) -> (Char, Char, Char, Char) -> [String]
boxString c (pT,pB,pL,pR) (bTC, bBC, bSC, bCC) = do
    let center = [bSC] ++ replicate pL ' ' ++ c ++ replicate pR ' ' ++ [bSC]
    let tL = length center
    let top = [bCC] ++ replicate (tL - 2) bTC ++ [bCC]
    let bot = [bCC] ++ replicate (tL - 2) bBC ++ [bCC]
    let side = [bSC] ++ replicate (tL - 2) ' ' ++ [bSC]
    let tSide = replicate pT side
    let bSide = replicate pB side
    [top] ++ tSide ++ [center] ++ bSide ++ [bot]




makeBox :: String -> (Int, Int, Int, Int) -> (Char, Char, Char, Char) -> (Int, Int, Int, Int) -> StringBox
makeBox c (pT,pB,pL,pR) (bTC, bBC, bSC, bCC) (pCT,pCB,pCL,pCR) =
    StringBox {
        top = bTC
        , topCorner = bCC
        , topPadding = pT

        , bot = bBC
        , botCorner = bCC
        , botPadding = pB

        , side = bSC
        , sidePadding = (pL, pR)

        , content = [c]
        , contentPaddingTop = pCT
        , contentPaddingBot = pCB
        , contentPaddingLeft = pCL
        , contentPaddingRight = pCR
    }