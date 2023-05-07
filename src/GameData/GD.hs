module GameData.GD (GameData) where

import Data.Map ( Map )
import Feature (Feature, Attribute)
import CDSL.CDSLExpr (CDSLExpr)


type GameData = Map Attribute (Map (Feature, Maybe [CDSLExpr]) [CDSLExpr])



