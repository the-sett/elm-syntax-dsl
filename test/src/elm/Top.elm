port module Top exposing (main)

import Elm.Parser
import Elm.Pretty
import Elm.Processing
import Elm.RawFile exposing (RawFile)
import Pretty



-- Top level construction


main : Program () Model Msg
main =
    Platform.worker { init = init, update = update, subscriptions = subscriptions }



-- Ports for data input and output


port modelInPort : (( String, String ) -> msg) -> Sub msg


port codeOutPort : String -> Cmd msg


subscriptions model =
    case model of
        Error ->
            Sub.none

        _ ->
            Sub.batch
                [ modelInPort (\( name, value ) -> ModelData name value)
                ]



-- State Machine


type Model
    = Error
    | Normal



-- Events


type Msg
    = ModelData String String


init _ =
    ( Normal, Cmd.none )


update msg model =
    case ( model, msg ) of
        ( Normal, ModelData name val ) ->
            let
                elmAstResult =
                    Elm.Parser.parse val
                        |> Result.map (Elm.Processing.process Elm.Processing.init)
            in
            case elmAstResult of
                Err _ ->
                    ( Debug.log "error" Error, Cmd.none )

                Ok file ->
                    let
                        pretty =
                            Elm.Pretty.pretty file
                                |> Pretty.pretty 120
                    in
                    ( model, codeOutPort pretty )

        ( _, _ ) ->
            ( model, Cmd.none )
