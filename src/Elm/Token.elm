module Elm.Token exposing (Token(..), comment, keyword, literal, number, operator, signature, statement, type_)

import Pretty exposing (Doc)


type Token
    = Keyword
    | Comment
    | Operator
    | Type
    | Statement
    | Signature
    | Literal
    | Number


keyword : String -> Doc Token
keyword str =
    Pretty.taggedString str Keyword


comment : String -> Doc Token
comment str =
    Pretty.taggedString str Comment

operator : String -> Doc Token
operator str =
    Pretty.taggedString str Operator


type_ : String -> Doc Token
type_ str =
    Pretty.taggedString str Type


statement : String -> Doc Token
statement str =
    Pretty.taggedString str Statement


signature : String -> Doc Token
signature str =
    Pretty.taggedString str Signature


literal : String -> Doc Token
literal str =
    Pretty.taggedString str Literal


number : String -> Doc Token
number str =
    Pretty.taggedString str Number


