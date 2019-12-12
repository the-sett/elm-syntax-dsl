module Elm.Comments exposing
    ( Comment(..), CommentPart(..), DocComment, FileComment
    , prettyDocComment, prettyFileComment
    , docCommentParser, fileCommentParser
    )

{-| A component DSL that helps with building comments.

It is useful to have this in a structured way, so that it can be re-flowed by
the pretty printer, and so that an understanding of the layout of the doc tags
can be extracted to order the exposing clause by.


# Structured comments

@docs Comment, CommentPart, DocComment, FileComment


# Pretty printing of comments

@docs prettyDocComment, prettyFileComment


# Parsing of comments into structured comments

@docs docCommentParser, fileCommentParser

-}

import Parser exposing (Parser)


type DocComment
    = DocComment


type FileComment
    = FileComment


type Comment a
    = Comment (List CommentPart)


type CommentPart
    = Markdown String
    | Code String
    | DocTags (List String)


{-| Pretty prints a document comment.

Where possible the comment will be re-flowed to fit the specified page width.

-}
prettyDocComment : Int -> Comment DocComment -> String
prettyDocComment width (Comment parts) =
    List.foldr
        (\part accum -> accum ++ partToString width part)
        ""
        parts


{-| Pretty prints a file comment.

Where possible the comment will be re-flowed to fit the specified page width.

-}
prettyFileComment : Int -> Comment FileComment -> ( String, List (List String) )
prettyFileComment width (Comment parts) =
    List.foldr
        (\part ( strAccum, tagsAccum ) ->
            let
                ( str, tags ) =
                    partToStringAndTags width part
            in
            ( strAccum ++ str, tags :: tagsAccum )
        )
        ( "", [] )
        parts


partToString : Int -> CommentPart -> String
partToString width part =
    case part of
        Markdown val ->
            val

        Code val ->
            "    " ++ val

        DocTags tags ->
            "@doc " ++ String.join ", " tags


partToStringAndTags : Int -> CommentPart -> ( String, List String )
partToStringAndTags width part =
    case part of
        Markdown val ->
            ( val, [] )

        Code val ->
            ( "    " ++ val, [] )

        DocTags tags ->
            ( "@doc " ++ String.join ", " tags, tags )


docCommentParser : Parser (Comment DocComment)
docCommentParser =
    Parser.getSource
        |> Parser.map (\val -> Comment [ Markdown val ])


fileCommentParser : Parser (Comment FileComment)
fileCommentParser =
    Parser.getSource
        |> Parser.map (\val -> Comment [ Markdown val ])
