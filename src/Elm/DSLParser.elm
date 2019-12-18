module Elm.DSLParser exposing (parse)

{-| An Elm source code parser.

This differes from the parser in Elm.Parser which is part of `stil4m/elm-syntax`,
in that it does not parse comments just as strings, but into a structured format.

The structured format know the difference between different parts of the comment
markdown including descriptive text, code examples and docs tags. The structured
format is used to re-flow comments to fit a page width, and to use doc tags to
determine how to lay out exposing lists to match.

@docs parse

-}

--exposing ((|.), (|=))

import Elm.CodeGen as CG exposing (Declaration, File)
import Elm.Comments as Comments exposing (Comment, CommentPart(..))
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as ESD
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.File
import Parser exposing ((|.), (|=), DeadEnd, Parser)
import Util exposing (denode, denodeAll, denodeMaybe, nodify)


{-| Parses a string into a file of Elm code.
-}
parse : String -> Result (List DeadEnd) File
parse val =
    Elm.Parser.parse val
        |> Result.map (Elm.Processing.process Elm.Processing.init)
        |> Result.map parseFileComments


parseFileComments : Elm.Syntax.File.File -> File
parseFileComments file =
    let
        fileComments =
            List.reverse file.comments
                |> denodeAll
                |> findFirstComment
    in
    CG.file (denode file.moduleDefinition)
        (denodeAll file.imports)
        (List.map parseDeclComments (denodeAll file.declarations))
        fileComments


parseDeclComments : ESD.Declaration -> Declaration
parseDeclComments decl =
    let
        docComments =
            docsFromDecl decl
                |> Maybe.andThen tryParseComment
    in
    case docComments of
        Nothing ->
            CG.DeclNoComment (declNoDocs decl)

        Just val ->
            CG.DeclWithComment val (declWithDocs decl)


findFirstComment : List String -> Maybe (Comment a)
findFirstComment comments =
    case comments of
        [] ->
            Nothing

        c :: cs ->
            case Parser.run commentParser c of
                Err _ ->
                    findFirstComment cs

                Ok val ->
                    Just val


tryParseComment : String -> Maybe (Comment a)
tryParseComment comment =
    Parser.run commentParser comment |> Result.toMaybe


commentParser : Parser (Comment a)
commentParser =
    Parser.succeed
        (\val -> Comments.addPart Comments.emptyComment (removeDelims val |> Markdown))
        |= (Parser.multiComment "{-|" "-}" Parser.Nestable |> Parser.getChompedString)


removeDelims : String -> String
removeDelims val =
    String.slice 3 -2 val
        |> String.trim


{-| Replaces the documentation on a declaration with the specified string.
-}
declWithDocs : ESD.Declaration -> String -> ESD.Declaration
declWithDocs decl docs =
    case decl of
        ESD.FunctionDeclaration funDecl ->
            ESD.FunctionDeclaration
                { funDecl | documentation = nodify docs |> Just }

        ESD.AliasDeclaration aliasDecl ->
            ESD.AliasDeclaration { aliasDecl | documentation = nodify docs |> Just }

        ESD.CustomTypeDeclaration customTypeDecl ->
            ESD.CustomTypeDeclaration { customTypeDecl | documentation = nodify docs |> Just }

        _ ->
            decl


{-| Strips the docs from a declaration.
-}
declNoDocs : ESD.Declaration -> ESD.Declaration
declNoDocs decl =
    case decl of
        ESD.FunctionDeclaration funDecl ->
            ESD.FunctionDeclaration
                { funDecl | documentation = Nothing }

        ESD.AliasDeclaration aliasDecl ->
            ESD.AliasDeclaration { aliasDecl | documentation = Nothing }

        ESD.CustomTypeDeclaration customTypeDecl ->
            ESD.CustomTypeDeclaration { customTypeDecl | documentation = Nothing }

        _ ->
            decl


{-| Extracts the documentation from a declaration as a string.
-}
docsFromDecl : ESD.Declaration -> Maybe String
docsFromDecl decl =
    case decl of
        ESD.FunctionDeclaration funDecl ->
            denodeMaybe funDecl.documentation

        ESD.AliasDeclaration aliasDecl ->
            denodeMaybe aliasDecl.documentation

        ESD.CustomTypeDeclaration customTypeDecl ->
            denodeMaybe customTypeDecl.documentation

        _ ->
            Nothing
