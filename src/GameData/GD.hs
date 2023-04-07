module GameData.GD where

import Feature (Feature)
import CDSL.CDSLExpr (CDSLExpr)

type GameData = [(Feature, [CDSLExpr])]

