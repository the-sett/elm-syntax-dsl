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
import Elm.Parser
import Elm.Processing
import Elm.Syntax.File
import Parser exposing (DeadEnd, Parser)
import Util exposing (denode)


parse : String -> Result (List DeadEnd) File
parse val =
    Elm.Parser.parse val
        |> Result.map (Elm.Processing.process Elm.Processing.init)
        |> Result.map parseFileComments


parseFileComments : Elm.Syntax.File.File -> File
parseFileComments file =
    case List.reverse file.comments |> List.head of
        Nothing ->
            { moduleDefinition = file.moduleDefinition
            , imports = file.imports
            , declarations = []
            , comments = Nothing
            }

        Just fc ->
            { moduleDefinition = file.moduleDefinition
            , imports = file.imports
            , declarations = []
            , comments =
                Just
                    (Elm.CodeGen.emptyFileComment
                        |> Elm.CodeGen.markdown (denode fc)
                    )
            }



--(List Elm.Syntax.Declaration.Declaration -> Elm.Syntax.File.File)
