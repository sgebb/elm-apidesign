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
        input = "",
        movingResource = Maybe.Nothing
    }
    , Cmd.none
    )


type alias Model =
    { ressurser : List Resource,
      input : String,
      movingResource : Maybe Resource
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
    = UpdateInput String
    | KeyDown Int
    | Move Resource
    | DropOn Resource
    | CancelMove
    | DragOver


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput word -> 
            ( {model | input = word}, Cmd.none)
        KeyDown key ->
            if key == 13 then
                ({ model | 
                    ressurser = emptyResource model.input :: model.ressurser,
                    input = "" }, Cmd.none)
            else
                (model, Cmd.none)
        Move resource -> 
            ( {model | movingResource = Just resource}, Cmd.none)
        CancelMove ->
            ({model | movingResource = Nothing}, Cmd.none)
        DropOn destination ->
            ({model | ressurser = moveResource model.ressurser model.movingResource destination, movingResource = Nothing} , Cmd.none)
        DragOver ->
            (model, Cmd.none)

moveResource : List Resource -> Maybe Resource -> Resource -> List Resource
moveResource alleRessurser ressurs destination =
        case ressurs of 
            Just res ->
                let
                    newRessurser = List.filter (areNotTheSame res) alleRessurser
                    withoutDest = List.filter (areNotTheSame destination) newRessurser
                    destinationAfterMoved = 
                        case destination.subressurser of
                            SubResources sub ->
                                {destination | subressurser = SubResources (res :: sub)}
                in
                    destinationAfterMoved :: withoutDest
            _ ->
                alleRessurser
            

areTheSame : Resource -> Resource -> Bool
areTheSame first other =     
        first.navn == other.navn &&  equalAmountSubResources first.subressurser other.subressurser

areNotTheSame : Resource -> Resource -> Bool
areNotTheSame first other = 
    not (areTheSame first other)

equalAmountSubResources : SubResources -> SubResources -> Bool
equalAmountSubResources first other =
    case first of
        SubResources fi->
            case other of 
                SubResources ot -> 
                    List.length fi == List.length ot

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode)

onDragStart : msg -> Attribute msg
onDragStart message =
    on "dragstart" (Json.succeed message)

onDragEnd : msg -> Attribute msg
onDragEnd message =
    on "dragend" (Json.succeed message)


onDragOver : msg -> Attribute msg
onDragOver msg =
    Html.Events.preventDefaultOn "dragover"
        <| Json.succeed (msg, True)

    
onDrop : msg -> Attribute msg
onDrop msg =
    Html.Events.preventDefaultOn "drop"
        <| Json.succeed (msg, True)

-- View


view : Model -> Html Msg
view model =
    div []
        [
            viewResources model.ressurser,
            h1 [][text "Start by writing some words.."],
            input [onKeyDown KeyDown, placeholder "A word", value model.input, onInput UpdateInput][]
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
    div[
        attribute "draggable" "true",
        attribute "ondragover" "return false",
        onDragEnd CancelMove,
        onDragOver DragOver,
        onDragStart <| Move resource,
        onDrop <| DropOn resource]
        [
            li []
        [
            text resource.navn,
            text ":",
            viewSubressurser resource.subressurser
        ]
    ]