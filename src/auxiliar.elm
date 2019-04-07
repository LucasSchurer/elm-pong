import Browser
import Browser.Events
import Json.Decode
import Html
import Html.Attributes

main = 
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model = 
    String

init : () -> ( Model, Cmd Msg )
init _ =
    ( "", Cmd.none )

type Msg 
    = KeyPressed String

keyDecoder : Json.Decode.Decoder Msg
keyDecoder = 
    Json.Decode.map toString (Json.Decode.field "key" Json.Decode.string)

toString : String -> Msg
toString string = 
    KeyPressed string

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch 
        [ Browser.Events.onKeyDown keyDecoder
        , Browser.Events.onKeyDown keyDecoder
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        KeyPressed string ->
            ( string, Cmd.none )

view : Model -> Html.Html Msg
view model = 
    Html.div [] 
        [ Html.text model ] 