module GameData.GD (GameData) where

import Feature (Feature)
import CDSL.CDSLExpr (CDSLExpr)

type GameData = [(Feature, [CDSLExpr])]

