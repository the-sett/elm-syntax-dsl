module Editor exposing (main)

{-| Sample code for Elm.Pretty with Elm.Token. Use `elm repl` to run this code.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Browser
import Elm.CodeGen exposing (File)
import Elm.Token exposing (Token(..))
import Elm.DSLParser exposing (parse)
import Elm.Pretty exposing (pretty)
import Pretty.Renderer
import Parser exposing (DeadEnd)

main =
    Browser.sandbox 
        { init = init
        , update = update 
        , view = view
        }


type alias Model =
    {sourceCode : String }

type Msg =
    Change String

init : Model
init =
    Model sampleCode

update : Msg -> Model -> Model
update msg model =
    case msg of
        Change str ->
            { model | sourceCode = str}


view : Model -> Html Msg
view model =
    let
        content =
            case parse model.sourceCode of
                Ok file ->
                    render file

                Err err ->
                    div [] [text <| Debug.toString err]

    in
    div [style "font-family" "monospace", style "font-size" "14px", style "display" "flex", style "width" "100%", style "height" "100%"] 
        [ div [style "width" "50%", style "padding" "10px"] [textarea [Html.Events.onInput Change, style "width" "100%", style "height" "100%"] [text model.sourceCode]]
        , div [style "width" "50%", style "padding" "10px", style "background-color" "#222"] [content]
        ]


type alias Context =
    { line : Int
    , column : Int
    , elements : List (List Node)
    , currentLine : List Node
    }
type alias Node =
    { tag : Maybe Token, content : String }



render : File -> Html Msg
render file = 
    let
        tagged : Token -> String -> Context -> Context
        tagged tag str ctx =
            { ctx
                | column = ctx.column + String.length str
                , currentLine = ctx.currentLine ++ [ Node (Just tag) str ]
            }

        untagged : String -> Context -> Context
        untagged str ctx =
            { ctx
                | column = ctx.column + String.length str
                , currentLine = ctx.currentLine ++ [ Node Nothing str ]
            }

        newline : Context -> Context
        newline ctx =
            { ctx
                | line = ctx.line + 1
                , column = 0
                , elements = ctx.elements ++ [ ctx.currentLine ]
                , currentLine = []
            }

        outer : Context -> Html msg
        outer ctx =
            let
                elements =
                    ctx.elements ++ [ ctx.currentLine ]

                toElem node =
                    code [ style "white-space" "pre", style "color" <| tokenColor node.tag ] [text node.content]

                elems =
                    List.map
                        (\nested ->
                            div [ style "height" "15px" ] <| List.map toElem nested
                        )
                        elements
            in
            div [] elems

        renderer =
            { init = Context 0 0 [] []
            , tagged = tagged
            , untagged = untagged
            , newline = newline
            , outer = outer
            }
    in
    Elm.Pretty.prepareLayout 80 file
        |> Pretty.Renderer.pretty 80 renderer



tokenColor : Maybe Token -> String
tokenColor token =
    case token of
        Just t ->
            case t of
                Keyword ->
                    "purple"

                Comment ->
                    "green"

                Operator ->
                    "gray"

                Type ->
                    "lightBlue"

                Statement ->
                    "white"

                Signature ->
                    "pink"

                Literal ->
                    "red"

                Number ->
                    "orange"

        Nothing ->
            "gray"






sampleCode = """module Editor exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Browser
import Elm.CodeGen exposing (File)
import Elm.Token exposing (Token(..))
import Elm.DSLParser exposing (parse)
import Elm.Pretty exposing (pretty)
import Pretty.Renderer
import Parser exposing (DeadEnd)
"""