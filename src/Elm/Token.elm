module Elm.Token exposing (Token(..), comment, custom, keyword, literal, number, operator, signature, symbol, plain, type_, var)

import Pretty exposing (Doc)


type Token
    = Keyword
    | Symbol
    | Operator
    | Comment
    | Type
    | Var
    | Text
    | Signature
    | Literal
    | Number
    | Custom String


keyword : String -> Doc Token
keyword str =
    Pretty.taggedString str Keyword


symbol : String -> Doc Token
symbol str =
    Pretty.taggedString str Symbol


operator : String -> Doc Token
operator str =
    Pretty.taggedString str Operator


comment : String -> Doc Token
comment str =
    Pretty.taggedString str Comment


type_ : String -> Doc Token
type_ str =
    Pretty.taggedString str Type


var : String -> Doc Token
var str =
    Pretty.taggedString str Var


plain : String -> Doc Token
plain str =
    Pretty.taggedString str Text


signature : String -> Doc Token
signature str =
    Pretty.taggedString str Signature


literal : String -> Doc Token
literal str =
    Pretty.taggedString str Literal


number : String -> Doc Token
number str =
    Pretty.taggedString str Number


custom : { string : String, tag : String } -> Doc Token
custom config =
    Pretty.taggedString config.string (Custom config.tag)
