module Elm.Comments exposing
    ( Comment(..), CommentPart(..), DocComment, FileComment
    , emptyComment, addPart
    )

{-| A component DSL that helps with building comments.

It is useful to have this in a structured way, so that it can be re-flowed by
the pretty printer, and so that an understanding of the layout of the doc tags
can be extracted to order the exposing clause by.


# Structured comments

@docs Comment, CommentPart, DocComment, FileComment


# Building comments

@docs emptyComment, addPart


# Pretty printing of comments

@docs prettyDocComment, prettyFileComment


# Parsing of comments into structured comments

@docs docCommentParser, fileCommentParser

-}

import Parser exposing (Parser)
import Pretty exposing (Doc)



--import Elm.Token exposing (Token)


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


{-| Creates an empty comment of any type.
-}
emptyComment : Comment a
emptyComment =
    Comment []


{-| Adds a part to a comment.
-}
addPart : Comment a -> CommentPart -> Comment a
addPart (Comment parts) part =
    Comment (part :: parts)
