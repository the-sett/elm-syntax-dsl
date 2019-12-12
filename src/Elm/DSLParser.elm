module Elm.DSLParser exposing (parse)

{-| An Elm source code parser.

This differes from the parser in Elm.Parser which is part of `stil4m/elm-syntax`,
in that it does not parse comments just as strings, but into a structured format.

The structured format know the difference between different parts of the comment
markdown including descriptive text, code examples and docs tags. The structured
format is used to re-flow comments to fit a page width, and to use doc tags to
determine how to lay out exposing lists to match.

-}

import Elm.CodeGen exposing (File)
import Parser exposing (DeadEnd, Parser)


parse : String -> Result (List DeadEnd) File
parse =
    ()
