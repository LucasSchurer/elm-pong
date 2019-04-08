import Browser
import Browser.Events
import Json.Decode
import Svg
import Svg.Attributes
import Time 

-- Main
main = 
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- Model

type State 
    = Play
    | Pause

type alias WindowSettings =
    { width : Int
    , height : Int
    , focus : Bool
    }

type alias GameSettings =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , state : State
    }

type alias Player =    
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , speed : Int
    , score : Int
    }

type alias Ball =
    { x : Int
    , y : Int
    , r : Int
    }

type alias Model = 
    { player1 : Player
    , player2 : Player
    , game : GameSettings
    , window : WindowSettings
    , ball : Ball
    , time : Time
    }

init : () -> ( Model, Cmd Msg )
init _ =
    let
        width = 1920
        height = 1080

    in
    ( Model 
        ( initPlayer 
            ( width // 2 - width // 4 + width // 64 ) 
            ( height // 4 - width // 64 ) 
            width
        ) 
        ( initPlayer 
            ( width // 2 + width // 4 - 2 * ( width // 64 ) ) 
            ( height // 4 - width // 64 )
            width 
        )
        ( initGameSettings width height )
        ( initWindowSettings width height )
        ( initBall 
            ( width // 2  )
            ( height // 4 )
        )
        ( 0 )
    , Cmd.none
    )

initPlayer : Int -> Int -> Int -> Player
initPlayer x y width = 
    Player x y ( width // 86 ) ( width // 28 ) ( width // 94 ) 0

initBall : Int -> Int -> Ball
initBall x y =
    Ball x y ( x * 2 // 128)

initGameSettings : Int -> Int -> GameSettings
initGameSettings width height =
    GameSettings 
        ( width // 2 - width // 4 ) 
        0 
        ( width // 2 ) 
        ( height // 2 ) 
        Play

initWindowSettings : Int -> Int -> WindowSettings
initWindowSettings width height =
    WindowSettings width height True

-- Update

type alias Time =
    Float

type Key 
    = Up
    | Down
    | W
    | S
    | Space
    | None

type Msg 
    = KeyPressed Key
    | GameState State
    | Tick Time.Posix

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of 
        KeyPressed key ->
            case key of
                Up -> 
                    if validMovement model.game model.player1 2 then
                        ( { model | player1 = updatePlayer model.player1 -1 }
                        , Cmd.none 
                        )

                    else 
                        ( model
                        , Cmd.none 
                        )
                
                Down ->
                    if validMovement model.game model.player1 1 then
                        ( { model | player1 = updatePlayer model.player1 1 }
                        , Cmd.none 
                        )

                    else 
                        ( model
                        , Cmd.none 
                        )
                
                W ->
                    if validMovement model.game model.player2 2 then
                        ( { model | player2 = updatePlayer model.player2 -1 }
                        , Cmd.none 
                        )

                    else 
                        ( model
                        , Cmd.none 
                        )
                
                S ->
                    if validMovement model.game model.player2 1 then
                        ( { model | player2 = updatePlayer model.player2 1 }
                        , Cmd.none 
                        )

                    else 
                        ( model
                        , Cmd.none 
                        )
                
                Space ->
                    ( model, Cmd.none ) 
                
                None ->
                    ( model, Cmd.none ) 
   
        
        
        Tick time ->
            ( { model | time = model.time + 1 }
            , Cmd.none ) 

        _ ->
            ( model, Cmd.none )     
    

validMovement : GameSettings -> Player -> Int -> Bool
validMovement game player dir =
    case dir of
        1 ->
            if ( ( player.y + player.speed + player.height ) > game.height ) then False else True
        
        2 ->
            if ( player.y - player.speed < 0 ) then False else True
        
        _ ->
            False

updatePlayer : Player -> Int -> Player
updatePlayer player dir = 
    { player | y = player.y + ( dir * player.speed ) }

-- Subscriptions

keyDecoder : Json.Decode.Decoder Msg
keyDecoder = 
    Json.Decode.map toDirection (Json.Decode.field "key" Json.Decode.string)

toDirection : String -> Msg
toDirection string = 
    case string of
        "ArrowUp" -> 
            KeyPressed Up
        
        "ArrowDown" ->
            KeyPressed Down
        
        "w" ->
            KeyPressed W

        "s" ->
            KeyPressed S

        " " ->
            KeyPressed Space
            
        _ -> 
            KeyPressed None

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch 
        [ Browser.Events.onKeyDown keyDecoder
        , Time.every 0.01 Tick
        ]

-- View

view : Model -> Svg.Svg Msg
view model = 
    Svg.svg 
        [ Svg.Attributes.width ( String.fromInt ( model.window.width - model.window.width // 10) )
        , Svg.Attributes.height ( String.fromInt ( model.window.height - model.window.height // 7 ) )
        , Svg.Attributes.viewBox (
            String.join " " 
                [ "0"
                , "0"
                , ( String.fromInt ( model.window.width - model.window.width // 10 ) )
                , ( String.fromInt ( model.window.height - model.window.height // 7 ) )
                ]
        )
        ]
        [ drawBackground model.game
        , drawPlayer model.player1
        , drawPlayer model.player2
        , drawBall model.ball
        , Svg.text_ [ Svg.Attributes.fontSize "40", Svg.Attributes.x "800", Svg.Attributes.y "600" ] [ Svg.text ( String.fromFloat model.time ) ] 
        ]    
        
drawBackground : GameSettings -> Svg.Svg Msg
drawBackground settings = 
    Svg.rect
        [ Svg.Attributes.x ( String.fromInt settings.x ) 
        , Svg.Attributes.y ( String.fromInt settings.y )
        , Svg.Attributes.rx "10"
        , Svg.Attributes.ry "10"
        , Svg.Attributes.width ( String.fromInt settings.width )
        , Svg.Attributes.height ( String.fromInt settings.height )
        ] []

drawPlayer : Player -> Svg.Svg Msg
drawPlayer player =
    Svg.rect
        [ Svg.Attributes.x ( String.fromInt player.x ) 
        , Svg.Attributes.y ( String.fromInt player.y )
        , Svg.Attributes.rx "10"
        , Svg.Attributes.ry "10"
        , Svg.Attributes.width ( String.fromInt player.width )
        , Svg.Attributes.height ( String.fromInt player.height )
        , Svg.Attributes.fill "white"
        ] []

drawBall : Ball -> Svg.Svg Msg
drawBall ball = 
    Svg.circle
        [ Svg.Attributes.cx ( String.fromInt ball.x )
        , Svg.Attributes.cy ( String.fromInt ball.y )
        , Svg.Attributes.r ( String.fromInt ball.r )
        , Svg.Attributes.fill "white"
        ] []