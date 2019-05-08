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
    { width : Float
    , height : Float
    , scale : Float    
    , focus : Bool
    }

type alias GameSettings =
    { x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    , state : State
    }

type alias Player =    
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , speed : Float
    , direction : Int
    }

type alias Ball =
    { x : Float
    , y : Float
    , r : Float
    , speedx : Float
    , speedy : Float
    }

type alias Score =
    { player1 : Int
    , player2 : Int
    }

type alias Model = 
    { player1 : Player
    , player2 : Player
    , ball : Ball
    , score : Score
    , game : GameSettings
    , window : WindowSettings
    , time : Time
    }

init : () -> ( Model, Cmd Msg )
init _ =
    let
        windowSettings = initWindowSettings 1920 1080
        gameSettings = initGameSettings windowSettings.width windowSettings.height windowSettings.scale

        x1 = gameSettings.x1
        y1 = gameSettings.y1
        x2 = gameSettings.x2
        y2 = gameSettings.y2

        gameWidth = x2 - x1
        gameHeight = y2 - y1

        playerWidth = 20 * windowSettings.scale
        playerHeight = 80 * windowSettings.scale
        ballRadius = 10 * windowSettings.scale

        player1 = 
            initPlayer
                ( x1 + playerWidth*2)
                ( gameHeight / 2 - playerHeight / 2 )
                playerWidth
                playerHeight
                gameWidth

        player2 = 
            initPlayer
                ( x2 - playerWidth*4 )
                ( gameHeight / 2 - playerHeight / 2 )
                playerWidth
                playerHeight
                gameWidth

        ball =
            initBall
                ( windowSettings.width / 2 )
                ( gameHeight / 2 )
                ballRadius
                gameWidth

        score =
            initScore

    in
        ( Model player1 player2 ball score gameSettings windowSettings 0
        , Cmd.none
        )

initPlayer : Float -> Float -> Float -> Float -> Float -> Player
initPlayer x y playerWidth playerHeight gameHeight = 
    Player x y playerWidth playerHeight ( gameHeight / ( playerHeight * 2 ) ) 0

initBall : Float -> Float -> Float -> Float -> Ball
initBall x y ballRadius gameWidth =
    Ball x y ballRadius ( gameWidth / ( ballRadius * 30 ) ) 0

initScore : Score
initScore =
    Score 0 0

initGameSettings : Float -> Float -> Float -> GameSettings
initGameSettings width height scale =
    GameSettings 
        ( width / 2 - width / 4 )
        0
        ( width / 2 + width / 4 )
        ( height / 2 )
        Play

initWindowSettings : Float -> Float -> WindowSettings
initWindowSettings width height =
    WindowSettings width height ( ( width + height ) / ( 1920 + 1080 ) ) True

-- Update

type alias Time =
    Float

type Key 
    = Up
    | Down
    | Stop
    | Space
    | None

type Msg 
    = Tick Time.Posix
    | Player1 Key
    | Player2 Key
    | System Key

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        Player1 direction ->
            case direction of
                Up ->
                    ( { model | player1 = updateDirection model.player1 -1 }
                    , Cmd.none 
                    )

                Down ->
                    ( { model | player1 = updateDirection model.player1 1 }
                    , Cmd.none 
                    )

                Stop ->
                    ( { model | player1 = updateDirection model.player1 0 }
                    , Cmd.none 
                    )

                _ ->
                    ( model
                    , Cmd.none 
                    )

        Player2 direction ->
            case direction of
                Up ->
                    ( { model | player2 = updateDirection model.player2 -1 }
                    , Cmd.none 
                    )

                Down ->
                    ( { model | player2 = updateDirection model.player2 1 }
                    , Cmd.none 
                    )

                Stop ->
                    ( { model | player2 = updateDirection model.player2 0 }
                    , Cmd.none 
                    )

                _ ->
                    ( model
                    , Cmd.none 
                    )

        Tick time ->
            ( updateGame model
            , Cmd.none 
            ) 

        _ ->
            ( model, Cmd.none )
    
validMovement : GameSettings -> Player -> Bool
validMovement game player =
    let 
        dir = String.fromInt ( player.direction )

    in
        case dir of
            "1" ->
                if ( ( player.y + player.speed + player.height ) > game.y2 ) then False else True
            
            "-1" ->
                if ( player.y - player.speed < game.y1 ) then False else True
            
            _ ->
                False

updatePlayer : GameSettings -> Player -> Player
updatePlayer game player = 
    if validMovement game player then 
        { player | y = player.y + ( toFloat ( player.direction ) * player.speed ) }
    
    else 
        player

updateDirection : Player -> Int -> Player
updateDirection player dir =
    { player | direction = dir }

updateBall : Model -> Model
updateBall model =
    let 
        ball = model.ball 
        player1 = model.player1
        player2 = model.player2 
        score = model.score 
        game = model.game

    in

        let 
            collisionSide = checkCollisionBallSides ball game
        
        in
            case collisionSide of
                1 ->
                    { model | score = addPointPlayer 2 model.score, ball = resetBall model.ball model.game model.window }

                2 ->
                    { model | score = addPointPlayer 1 model.score, ball = resetBall model.ball model.game model.window }
                
                3 ->
                    { model | ball = changeBallDirectionSides ball }
                
                4 ->
                    { model | ball = changeBallDirectionSides ball }
                
                _ -> 
                    if checkCollisionBallPlayer ball player1 then
                        { model | ball = changeBallDirection model.ball model.player1 }

                    else if checkCollisionBallPlayer ball player2 then
                        { model | ball = changeBallDirection model.ball model.player2 }
                        
                    else
                        { model | ball = moveBall ball }

updateGame : Model -> Model
updateGame model =
    let
        player1 = updatePlayer model.game model.player1 
        player2 = updatePlayer model.game model.player2
        modelAux = updateBall model
        score = modelAux.score
        ball = modelAux.ball
        game = modelAux.game
        window = modelAux.window
        time = modelAux.time + 1 

    in
        Model 
            player1
            player2
            ball
            score
            game
            window
            time

checkCollisionBallPlayer : Ball -> Player -> Bool
checkCollisionBallPlayer ball player = 
    let 
        r = ball.r
        cx = ball.x
        cy = ball.y
        sx = ball.speedx
        sy = ball.speedy
        x1 = player.x
        y1 = player.y
        x2 = player.x + player.width
        y2 = player.y + player.height

    in
        if ( ( ( cx + sx + r ) >= x1 ) && ( ( cx + sx + r ) <= x2 ) ) || ( ( ( cx + sx - r ) >= x1 ) && ( cx + sx - r ) <= x2 ) then 
            if ( ( ( cy + sy + r ) >= y1 ) && ( ( cy + sy + r ) <= y2 ) ) || ( ( ( cy + sy - r ) >= y1 ) && ( cy + sy - r ) <= y2 ) then
                True
            else
                False
        else
            False

checkCollisionBallSides : Ball -> GameSettings -> Int
checkCollisionBallSides ball game =
    if ( ( ball.x - ball.r ) <= game.x1 ) then 
        1
    else if ( ( ball.x + ball.r >= game.x2 ) ) then 
        2 
    else if ( ( ball.y + ball.r + ball.speedy ) > game.y2 )  then
        3
    else if ( ( ball.y - ball.r + ball.speedy ) < game.y1 ) then
        4
    else
        0

addPointPlayer : Int -> Score -> Score
addPointPlayer player score =
    case player of
        1 ->
            { score | player1 = score.player1 + 1 }

        2 ->
            { score | player2 = score.player2 + 1 }

        _ ->
            score

changeBallDirection : Ball -> Player -> Ball
changeBallDirection ball player =
    let
        middlePaddle = ( player.y + player.height ) / 2
        distanceMiddleBall = abs ( middlePaddle - ball.x )
        speedy = distanceMiddleBall / 20

    in    
        Ball 
            ( ball.x + ball.speedx )
            ( ball.y + ball.speedy )
            ball.r
            ( negate ( ball.speedx ) )
            speedy

changeBallDirectionSides : Ball -> Ball
changeBallDirectionSides ball =
    moveBall { ball | speedy = negate ball.speedy }

moveBall : Ball -> Ball
moveBall ball =
    Ball
        ( ball.x + ball.speedx )
        ( ball.y + ball.speedy )
        ball.r
        ball.speedx
        ball.speedy

resetBall : Ball -> GameSettings -> WindowSettings -> Ball
resetBall ball game window =
    let
        gameHeight = game.y2 - game.y1
    in
    { ball | x = window.width / 2, y = gameHeight / 2, speedx = negate ball.speedx, speedy = 0 }
    

-- Subscriptions

keyDecoder : Json.Decode.Decoder Msg
keyDecoder = 
    Json.Decode.map toDirection ( Json.Decode.field "key" Json.Decode.string )

toDirection : String -> Msg
toDirection string =
    case string of
        "ArrowUp" ->
            Player2 Up

        "ArrowDown" ->
            Player2 Down

        "w" ->
            Player1 Up

        "s" ->
            Player1 Down

        _ ->
            System None

keyDecoder_ : Json.Decode.Decoder Msg
keyDecoder_ = 
    Json.Decode.map toDirection_ ( Json.Decode.field "key" Json.Decode.string )

toDirection_ : String -> Msg
toDirection_ string =
    case string of
        "ArrowUp" ->
            Player2 Stop

        "ArrowDown" ->
            Player2 Stop

        "w" ->
            Player1 Stop

        "s" ->
            Player1 Stop

        _ ->
            System None


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch 
        [ Browser.Events.onKeyDown keyDecoder
        , Browser.Events.onKeyUp keyDecoder_
        , Time.every ( 1000 / 60 ) Tick
        ]

-- View

view : Model -> Svg.Svg Msg
view model = 
    Svg.svg 
        [ Svg.Attributes.width ( String.fromFloat ( model.window.width - model.window.width / ( 10 * model.window.scale ) ) )
        , Svg.Attributes.height ( String.fromFloat ( model.window.height - model.window.height / ( 5 * model.window.scale ) ) )
        ]
        [ drawBackground model.game
        , drawPlayer model.player1
        , drawPlayer model.player2
        , drawBall model.ball
        , drawScore model.score model.window.scale
        , Svg.text_ [ Svg.Attributes.fontSize "40", Svg.Attributes.x "800", Svg.Attributes.y "600" ] [ Svg.text ( String.fromFloat model.time ) ] 
        ]    
        
drawBackground : GameSettings -> Svg.Svg Msg
drawBackground settings = 
    Svg.rect
        [ Svg.Attributes.x ( String.fromFloat settings.x1 ) 
        , Svg.Attributes.y ( String.fromFloat settings.y1 )
        , Svg.Attributes.rx "10"
        , Svg.Attributes.ry "10"
        , Svg.Attributes.width ( String.fromFloat ( settings.x2 - settings.x1 ) )
        , Svg.Attributes.height ( String.fromFloat ( settings.y2 - settings.y1 ) )
        ] []

drawPlayer : Player -> Svg.Svg Msg
drawPlayer player =
    Svg.rect
        [ Svg.Attributes.x ( String.fromFloat player.x ) 
        , Svg.Attributes.y ( String.fromFloat player.y )
        , Svg.Attributes.rx "10"
        , Svg.Attributes.ry "10"
        , Svg.Attributes.width ( String.fromFloat player.width )
        , Svg.Attributes.height ( String.fromFloat player.height )
        , Svg.Attributes.fill "white"
        ] []

drawBall : Ball -> Svg.Svg Msg
drawBall ball = 
    Svg.circle
        [ Svg.Attributes.cx ( String.fromFloat ball.x )
        , Svg.Attributes.cy ( String.fromFloat ball.y )
        , Svg.Attributes.r ( String.fromFloat ball.r )
        , Svg.Attributes.fill "white"
        ] []

drawScore : Score -> Float -> Svg.Svg Msg
drawScore score scale =
    Svg.text_ 
        [ Svg.Attributes.fontSize "10"
        , Svg.Attributes.x "200"
        , Svg.Attributes.y "100"
        ] 
        [
            Svg.text ( String.fromInt score.player1 )
        ]