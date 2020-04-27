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
    Resource (Resources [emptyResource "underressurs"] ) "hovedressurs"
-- Model


init : () -> ( Model, Cmd Msg )
init _ =
    ( {
        ressurser = Resources([testResource]), 
        input = "",
        movingResource = Maybe.Nothing
    }
    , Cmd.none
    )


type alias Model =
    { ressurser : Resources,
      input : String,
      movingResource : Maybe Resource
    }

type alias Resource = 
    {
        subressurser : Resources,
        navn: String
    }

type Resources = 
    Resources (List Resource)

emptyResource : String -> Resource
emptyResource name = 
    Resource emptyResources name

emptyResources : Resources
emptyResources =
    Resources []

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
                    ressurser = withResource model.ressurser (emptyResource model.input),
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


-- Help

moveResource : Resources -> Maybe Resource -> Resource -> Resources
moveResource alleRessurser mover destination =
    case mover of 
        Just movingRes ->
            let
                withoutRemoved = withoutResource alleRessurser movingRes
            in
                withResourceInTarget withoutRemoved movingRes destination
        _ ->
            alleRessurser

withResourceInTarget : Resources -> Resource -> Resource -> Resources
withResourceInTarget ressurser added targetResource =
    case ressurser of
        Resources res ->
            Resources (List.map (addToResourceIfTarget added targetResource) res)

addToResourceIfTarget : Resource -> Resource -> Resource -> Resource
addToResourceIfTarget added target self =
    if areTheSame target self
        then {self | subressurser = withResource self.subressurser added}
    else 
        case self.subressurser of 
            Resources res ->
                {self | subressurser = Resources (List.map (addToResourceIfTarget added target) res)}

withoutResource : Resources -> Resource -> Resources
withoutResource ressurser removed =
    case ressurser of 
        Resources res ->
            if List.any (areTheSame removed) res
                then Resources (List.filter (areNotTheSame removed) res)
            else Resources (List.map (resourceWithRemovedSubresource removed) res)

resourceWithRemovedSubresource : Resource -> Resource -> Resource
resourceWithRemovedSubresource self removed =
    {self | subressurser = withoutResource self.subressurser removed}


withResource : Resources -> Resource -> Resources
withResource ressurser added  =
    case ressurser of
        Resources res ->
            Resources ( added :: res)

areTheSame : Resource -> Resource -> Bool
areTheSame first other =     
    first == other
        -- trenger en id?

areNotTheSame : Resource -> Resource -> Bool
areNotTheSame first other = 
    not (areTheSame first other)

equalAmountResources : Resources -> Resources -> Bool
equalAmountResources first other =
    case first of
        Resources fi->
            case other of 
                Resources ot -> 
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
    div [class "container d-flex h-100"][
        div[class "row align-self-center w-100"][
            div[class "col-8 mx-auto"][
                div[][
                    viewResources model.ressurser
                ],
                div[][ 
                    h1[][
                        text "Start by writing some words.."
                    ],
                    div[][
                        input [onKeyDown KeyDown, placeholder "A word", value model.input, onInput UpdateInput][]
                    ]
                ]
            ]
        ]
    ]

viewResources : Resources -> Html Msg
viewResources resources =
    case resources of 
        Resources res ->
            div[]
            [
                ul [] (List.map viewResource res)
            ]


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
            viewResources resource.subressurser
        ]
    ]