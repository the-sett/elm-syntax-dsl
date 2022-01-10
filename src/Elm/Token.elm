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


toToken : Token -> String -> Doc Token
toToken t str = 
    Pretty.taggedString str t


keyword : String -> Doc Token
keyword  =
    toToken Keyword


comment : String -> Doc Token
comment =
    toToken Comment


operator : String -> Doc Token
operator =
    toToken Operator


type_ : String -> Doc Token
type_ =
    toToken Type


statement : String -> Doc Token
statement =
    toToken Statement


signature : String -> Doc Token
signature =
    toToken Signature


literal : String -> Doc Token
literal =
    toToken Literal


number : String -> Doc Token
number =
    toToken Number
