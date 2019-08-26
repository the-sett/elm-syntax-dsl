port module Top exposing (main)

import Codec
import Dict exposing (Dict)
import Diff
import Elm.Pretty
import Elm.Writer
import Json.Decode as Decode
import Json.Decode.Generic as Generic
import Pretty
import Random exposing (Seed)
import Task
import Templates.Api
import Time exposing (Posix)



-- Top level construction


main : Program () Model Msg
main =
    Platform.worker { init = init, update = update, subscriptions = subscriptions }



-- Ports for data input and output


port modelInPort : (( String, String ) -> msg) -> Sub msg


port codeOutPort : String -> Cmd msg


subscriptions model =
    case model of
        Error _ ->
            Sub.none

        _ ->
            Sub.batch
                [ modelInPort (\( name, value ) -> ModelData name value)
                ]



-- State Machine


type alias Model =
    ()



-- Events


type Msg
    = ModelData String String


init _ =
    ( (), Cmd.none )


update msg model =
    case ( model, msg ) of
        ( (), ModelData name val ) ->
            ( model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )
