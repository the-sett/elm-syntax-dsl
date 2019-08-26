port module Top exposing (main)

import Elm.Pretty
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
            ( model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )
