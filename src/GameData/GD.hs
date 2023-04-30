module GameData.GD (GameData) where

import Feature (Feature (..))
import CDSL.CDSLExpr (CDSLExpr (CEffect), CDSLExecError (CDSLExecError), CDSLParseErrorCode (MissMatchCardError), CDSLParseError (CDSLParseError, pErr, pExpr, rawExpr))
import Functions (lookupAll)
import CardGame.CardFunctions (makeDeck, cardElem)

type GameData = [(Feature, [CDSLExpr])]

