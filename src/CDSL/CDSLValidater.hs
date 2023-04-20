module CDSL.CDSLValidater (validateCDSLExpression) where
import CDSL.CDSLExpr
import Data.Either (partitionEithers)
import Data.List (partition)



-- Checks if a given CDSLExpr is valid.
-- Due to this language being more command-like, there are only a certain ways expressions can be built up
validateCDSLExpression :: CDSLExpr -> Either CDSLExpr [CDSLExecError]
-- Players
validateCDSLExpression e@(Any (Players (IsEmpty Hand))) = Left e
validateCDSLExpression e@(All (Players (IsEmpty Hand))) = Left e
validateCDSLExpression e@(Any (Players (IsEqual a (Numeric _)))) = if a == Hand || a == Score
    then
        Left e
    else
        Right [(CDSLExecError { err = InvalidSyntaxError, expr = e })]
validateCDSLExpression e@(Any (Players (IsEqual (Numeric _) a))) = if a == Hand || a == Score
    then
        Left e
    else
        Right [(CDSLExecError { err = InvalidSyntaxError, expr = e })]

validateCDSLExpression e@(All (Players (IsEqual a (Numeric _)))) = if a == Hand || a == Score
    then
        Left e
    else
        Right [(CDSLExecError { err = InvalidSyntaxError, expr = e })]
validateCDSLExpression e@(All (Players (IsEqual (Numeric _) a))) = if a == Hand || a == Score
    then
        Left e
    else
        Right [(CDSLExecError { err = InvalidSyntaxError, expr = e })]
validateCDSLExpression e@(Greatest (Players a)) = if a == Score || a == Hand
    then
        Left e
    else
        Right [(CDSLExecError { err = InvalidSyntaxError, expr = e })]

-- Cards
validateCDSLExpression e@(Shuffle a) = if a == Deck || a == Pile || a == Players TurnOrder
    then
        Left e
    else
        Right [(CDSLExecError { err = InvalidSyntaxError, expr = e })]
validateCDSLExpression e@(IsEmpty a) = if a == Deck || a == Pile
    then
        Left e
    else
        Right [(CDSLExecError { err = InvalidSyntaxError, expr = e })]
validateCDSLExpression e@(Swap a b) = if (a == Pile || a == Deck) && (b == Pile || b == Deck)
    then
        Left e
    else
        Right [(CDSLExecError { err = InvalidSyntaxError, expr = e})]
validateCDSLExpression e@(Take (Numeric _) f t) = if (f == Pile || f == Deck) && (t == Pile || t == Deck)
    then
        Left e
    else
        Right [(CDSLExecError { err = InvalidSyntaxError, expr = e})]
validateCDSLExpression (And l r) = case (partitionEithers $ map validateCDSLBool l, partitionEithers $ map validateCDSLBool r) of
    ((_, []), (_, [])) -> Left (And l r)
    ((_, er), _) -> Right (CDSLExecError { err = SyntaxErrorLeftOperand, expr = And l [Null] }:er)
validateCDSLExpression (Or l r) = case (partitionEithers $ map validateCDSLBool l, partitionEithers $ map validateCDSLBool r) of
    ((_, []), (_, [])) -> Left (Or l r)
    ((_, er), _) -> Right (CDSLExecError { err = SyntaxErrorLeftOperand, expr = Or l [Null] }:er)
validateCDSLExpression Always = Left Always
validateCDSLExpression Never = Left Never
validateCDSLExpression ex@(Reset (CurrentPlayer e)) = if e == PMoves || e == Score || e == Hand
    then
        Left ex
    else
        Right [(CDSLExecError { err = InvalidSyntaxError, expr = e})]
validateCDSLExpression e@(If conds exprs) = case (partitionEithers (map validateCDSLBool conds), partitionEithers (map validateCDSLExpression exprs)) of
    ((_, []), (_, [])) -> Left e
    ((_, cErrs), (_, eErrs)) -> Right (CDSLExecError { err = InvalidBoolEvaluationError, expr = e }:cErrs ++ concat eErrs)
validateCDSLExpression e = Right [(CDSLExecError { err = InvalidSyntaxError, expr = e })]






validateCDSLBool :: CDSLExpr -> Either CDSLExpr CDSLExecError
validateCDSLBool Always = Left Always
validateCDSLBool Never = Left Never
validateCDSLBool e@(IsEqual (Numeric _) (Numeric _)) = Left e
validateCDSLBool e@(IsEqual (Numeric _) Score) = Left e
validateCDSLBool e@(IsEqual Score (Numeric _)) = Left e
validateCDSLBool e@(Not ex) = case partitionEithers (map validateCDSLBool ex) of
    (_, []) -> Left e
    (_, err) -> Right (head err)
validateCDSLBool (Or l r) = case (partitionEithers $ map validateCDSLBool l, partitionEithers $ map validateCDSLBool r) of
    ((lt, []), (rt, [])) -> Left (Or lt rt)
    ((_, err), _) -> Right (head err)
validateCDSLBool (And l r) = case (partitionEithers $ map validateCDSLBool l, partitionEithers $ map validateCDSLBool r) of
    ((lt, []), (rt, [])) -> Left (And lt rt)
    ((_, err), _) -> Right (head err)
validateCDSLBool e@(IsEmpty a) = if a == Deck || a == Pile
    then
        Left e
    else
        Right (CDSLExecError { err = InvalidSyntaxError, expr = e })
validateCDSLBool e = Right (CDSLExecError { err = InvalidBoolEvaluationError, expr = e })
