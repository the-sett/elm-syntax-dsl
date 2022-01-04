module Elm.Token exposing (Token(..), comment, keyword, literal, number, signature, plain, type_)

import Pretty exposing (Doc)


type Token
    = Keyword
    | Comment
    | Type
    | Text
    | Signature
    | Literal
    | Number


keyword : String -> Doc Token
keyword str =
    Pretty.taggedString str Keyword


comment : String -> Doc Token
comment str =
    Pretty.taggedString str Comment


type_ : String -> Doc Token
type_ str =
    Pretty.taggedString str Type


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


