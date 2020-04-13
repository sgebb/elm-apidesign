module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json




-- Main


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- TEST

testResource : Resource
testResource = 
    Resource (SubResources [emptyResource "underressurs"] ) "hovedressurs"
-- Model


init : () -> ( Model, Cmd Msg )
init _ =
    ( {
        ressurser = [testResource], 
        input = ""
    }
    , Cmd.none
    )


type alias Model =
    { ressurser : List Resource,
      input : String
    }

type alias Resource = 
    {
        subressurser : SubResources,
        navn: String
    }

type SubResources = 
    SubResources (List Resource)

emptyResource : String -> Resource
emptyResource name = 
    Resource emptySubResources name

emptySubResources : SubResources
emptySubResources =
    SubResources []
-- Update


type Msg
    = NoOp
    | Message
    | UpdateInput String
    | KeyDown Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        Message ->
            ( model, Cmd.none )
        UpdateInput word -> 
            ( {model | input = word}, Cmd.none)
        KeyDown key ->
            if key == 13 then
                ({ model | 
                    ressurser = emptyResource model.input :: model.ressurser,
                    input = "" }, Cmd.none)
            else
                (model, Cmd.none)


-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode)

-- View


view : Model -> Html Msg
view model =
    div []
        [
            viewResources model.ressurser,
            input [onKeyDown KeyDown, placeholder "A word", value model.input, onInput UpdateInput][],
            h1 [][text model.input]
        ]

viewResources : List Resource -> Html Msg
viewResources resources =
    div[]
    [
        ul [] (List.map viewResource resources)
    ]

viewSubressurser : SubResources -> Html Msg
viewSubressurser subresources =
    case subresources of
        SubResources resources ->
            viewResources resources

            
    

viewResource : Resource -> Html Msg
viewResource resource = 
    li []
    [
        text resource.navn,
        text ":",
        viewSubressurser resource.subressurser
    ]