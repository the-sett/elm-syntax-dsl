module Elm.DSLParser exposing (parse)

{-| An Elm source code parser.

This differes from the parser in Elm.Parser which is part of `stil4m/elm-syntax`,
in that it does not parse comments just as strings, but into a structured format.

The structured format know the difference between different parts of the comment
markdown including descriptive text, code examples and docs tags. The structured
format is used to re-flow comments to fit a page width, and to use doc tags to
determine how to lay out exposing lists to match.

-}

import Elm.CodeGen as CG exposing (Declaration, File)
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as ESD
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.File
import Parser exposing (DeadEnd, Parser)
import Util exposing (denode, denodeAll, denodeMaybe, nodify)


parse : String -> Result (List DeadEnd) File
parse val =
    Elm.Parser.parse val
        |> Result.map (Elm.Processing.process Elm.Processing.init)
        |> Result.map parseFileComments


parseFileComments : Elm.Syntax.File.File -> File
parseFileComments file =
    let
        fileComments =
            case List.reverse file.comments |> List.head of
                Nothing ->
                    Nothing

                Just fc ->
                    CG.emptyFileComment |> CG.markdown (denode fc) |> Just
    in
    CG.file (denode file.moduleDefinition)
        (denodeAll file.imports)
        (List.map parseDeclComments (denodeAll file.declarations))
        fileComments


parseDeclComments : ESD.Declaration -> Declaration
parseDeclComments decl =
    let
        docComments =
            case docsFromDecl decl of
                Nothing ->
                    Nothing

                Just val ->
                    CG.emptyDocComment |> CG.markdown val |> Just
    in
    case docComments of
        Nothing ->
            CG.DeclNoComment (declNoDocs decl)

        Just val ->
            CG.DeclWithComment val (declWithDocs decl)


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
