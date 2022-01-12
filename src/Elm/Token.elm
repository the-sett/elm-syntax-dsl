module Elm.Token exposing
    ( Token(..)
    , comment, keyword, literal, number, operator, signature, statement, type_
    )

{-| Elm.Token is used for tagging string for Elm.Pretty.


# Type

@docs Token


# Functions

@docs comment, keyword, literal, number, operator, signature, statement, type_

-}

import Pretty exposing (Doc)


{-| A custom type defines tags for pretty printing. Elm.Pretty uses this 
to generated tagged strings which you can use Pretty.Renderer to generate 
non-`String` output.
-}
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


{-| Create keywords such as `let`, `if`, `case` and so on
-}
keyword : String -> Doc Token
keyword =
    toToken Keyword


{-| Create comments.
-}
comment : String -> Doc Token
comment =
    toToken Comment


{-| Create operators such as `+`, `-`, `*`, `/` and so on
-}
operator : String -> Doc Token
operator =
    toToken Operator


{-| Create types such as `String`, `Int`, `MyType`.
-}
type_ : String -> Doc Token
type_ =
    toToken Type


{-| Create statements such as `map`, `update`, `view` and so on
-}
statement : String -> Doc Token
statement =
    toToken Statement


{-| Create function signature, either top level or inside of let closure, usually followed by pattern and `=`.
-}
signature : String -> Doc Token
signature =
    toToken Signature


{-| Create string and char literal.
-}
literal : String -> Doc Token
literal =
    toToken Literal


{-| Create number literal.
-}
number : String -> Doc Token
number =
    toToken Number
