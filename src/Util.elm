module Util exposing (denode, denodeAll, denodeMaybe, nodify, nodifyAll, nodifyAndParentifyAll, nodifyMaybe, parentify)

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (emptyRange)


denode : Node a -> a
denode =
    Node.value


denodeAll : List (Node a) -> List a
denodeAll =
    List.map denode


denodeMaybe : Maybe (Node a) -> Maybe a
denodeMaybe =
    Maybe.map denode


nodify : a -> Node a
nodify exp =
    Node emptyRange exp


nodifyAll : List a -> List (Node a)
nodifyAll =
    List.map nodify


nodifyMaybe : Maybe a -> Maybe (Node a)
nodifyMaybe =
    Maybe.map nodify


nodifyAndParentifyAll : Int -> InfixDirection -> List Expression -> List (Node Expression)
nodifyAndParentifyAll precedence associativity =
    List.map (parentify precedence associativity >> nodify)


parentify : Int -> InfixDirection -> Expression -> Expression
parentify precedence associativity expr =
    case expr of
        Application apps ->
            if List.length apps == 1 then
                expr

            else if precedence == 10 then
                ParenthesizedExpression (nodify expr)

            else
                expr

        OperatorApplication symbol opAssociativity _ _ ->
            if precedence > symbolToPrecedence symbol || (precedence == symbolToPrecedence symbol && associativity /= opAssociativity) then
                ParenthesizedExpression (nodify expr)

            else
                expr

        IfBlock _ _ _ ->
            ParenthesizedExpression (nodify expr)

        LetExpression _ ->
            ParenthesizedExpression (nodify expr)

        CaseExpression _ ->
            ParenthesizedExpression (nodify expr)

        _ ->
            expr


symbolToPrecedence : String -> Int
symbolToPrecedence symbol =
    case symbol of
        ">>" ->
            9

        "<<" ->
            9

        "^" ->
            8

        "*" ->
            7

        "/" ->
            7

        "//" ->
            7

        "%" ->
            7

        "rem" ->
            7

        "+" ->
            6

        "-" ->
            6

        "++" ->
            5

        "::" ->
            5

        "==" ->
            4

        "/=" ->
            4

        "<" ->
            4

        ">" ->
            4

        "<=" ->
            4

        ">=" ->
            4

        "&&" ->
            3

        "||" ->
            2

        "|>" ->
            0

        "<|" ->
            0

        _ ->
            -1
