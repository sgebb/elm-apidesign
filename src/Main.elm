module Main exposing (..)

import Browser
import Html exposing (..)



-- Main


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


init : () -> ( Model, Cmd Msg )
init _ =
    ( { navn = "Verden!"
      }
    , Cmd.none
    )


type alias Model =
    { navn : String
    }



-- Update


type Msg
    = NoOp
    | Message


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Message ->
            ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div []
        [ text ("Hei  " ++ model.navn)
        ]
