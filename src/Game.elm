import Browser
import Browser.Events
import Json.Decode
import Html
import Html.Attributes
import Html.Events
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

type alias Flags =
    { width : Float
    , height : Float
    }

type State 
    = Play
    | Waiting
    | Pause
    | Menu
    | GameOver

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
    , fps : Float
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
    }

init : () -> ( Model, Cmd Msg )
init _ =
    let
        windowSettings = initWindowSettings 1280 720
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
        ( Model player1 player2 ball score gameSettings windowSettings
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
        Waiting
        ( toFloat ( round ( 120*scale ) ) )

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

        System key ->
            case key of
                Space ->
                    if model.game.state == GameOver || model.game.state == Menu then
                        ( model
                        , Cmd.none
                        )
                    else
                        ( { model | game = changeState Play model.game } 
                        , Cmd.none
                        )                    
                        
                Stop ->
                    if model.game.state == Play then
                        ( { model | game = changeState Pause model.game }
                        , Cmd.none 
                        )

                    else if model.game.state == Pause then
                        ( { model | game = changeState Play model.game }
                        , Cmd.none 
                        )

                    else 
                        ( model
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
            if ( game.state == Play ) then
                case collisionSide of
                    1 ->
                        { model 
                        | score = addPointPlayer 2 model.score
                          , ball = resetBall model.ball model.game
                          , game = changeState Waiting model.game 
                        }

                    2 ->
                        { model 
                        | score = addPointPlayer 1 model.score
                        , ball = resetBall model.ball model.game
                        , game = changeState Waiting model.game 
                        }
                    
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
                else
                    model

updateGame : Model -> Model
updateGame model =
    if ( model.game.state == Play || model.game.state == Waiting ) then
        let
            modelAux = updateBall model
            player1 = 
                if model.game.state == Play then
                    updatePlayer modelAux.game model.player1 
                
                else
                    modelAux.player1

            player2 =
                if model.game.state == Play then
                    updatePlayer modelAux.game model.player2 
                
                else
                    modelAux.player2

            score = modelAux.score
            ball = modelAux.ball
            game = modelAux.game
            window = modelAux.window

        in
            Model 
                player1
                player2
                ball
                score
                ( { game | state = checkGameOver score game.state } )
                window
    else
        model

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
    Ball 
        ( ball.x + ball.speedx )
        ( ball.y + ball.speedy )
        ball.r
        ( negate ( ball.speedx ) )
        ( calculateYSpeedChange ball player )

calculateYSpeedChange : Ball -> Player -> Float
calculateYSpeedChange ball player =
    let
        middlePaddle = ( player.y + player.height ) / 2
        distanceMiddleBall = ( negate ( middlePaddle - ball.y ) ) / player.height
        speed = ( negate ball.speedy ) + distanceMiddleBall

    in
        speed
        
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

resetBall : Ball -> GameSettings -> Ball
resetBall ball game =
    let
        gameHeight = game.y2 - game.y1
        middleWidth = game.x2 - game.x1
    in
    { ball | x = middleWidth, y = gameHeight / 2, speedx = negate ball.speedx, speedy = 0 }

changeState : State -> GameSettings -> GameSettings
changeState state game =
    { game | state = state }

checkGameOver : Score -> State -> State
checkGameOver score state =
    if score.player1 > 4 || score.player2 > 4 then
        GameOver
    
    else
        state

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

        " " ->
            System Space

        "p" ->
            System Stop

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
subscriptions model =
    Sub.batch 
        [ Browser.Events.onKeyDown keyDecoder
        , Browser.Events.onKeyUp keyDecoder_
        , Time.every ( 1000 / model.game.fps ) Tick
        ]

-- View

view : Model -> Html.Html Msg
view model = 
    if model.game.state == Play then
        drawGame model

    else if model.game.state == Waiting then
        drawWaiting model

    else if model.game.state == Pause then
        drawPause model

    else if model.game.state == GameOver then
        drawGameOver model 

    else 
        drawMenu model  

drawGame : Model -> Html.Html Msg
drawGame model =
    Svg.svg
        [ Svg.Attributes.width ( String.fromFloat ( model.window.width - model.window.width / ( 10 * model.window.scale ) ) )
        , Svg.Attributes.height ( String.fromFloat ( model.window.height - model.window.height / ( 5 * model.window.scale ) ) )
        ]
        [ drawBackground model.game model.window.scale
        , drawBackgroundLine model.game model.window.scale
        , drawPlayer model.player1 model.window.scale
        , drawPlayer model.player2 model.window.scale
        , drawBall model.ball
        , drawScore1 model.score model.game model.window.scale
        , drawScore2 model.score model.game model.window.scale
        ]

drawBackground : GameSettings -> Float -> Html.Html Msg
drawBackground settings scale = 
    Svg.rect
        [ Svg.Attributes.x ( String.fromFloat settings.x1 ) 
        , Svg.Attributes.y ( String.fromFloat settings.y1 )
        , Svg.Attributes.rx ( String.fromFloat ( 10 * scale ) )
        , Svg.Attributes.ry ( String.fromFloat ( 10 * scale ) )
        , Svg.Attributes.width ( String.fromFloat ( settings.x2 - settings.x1 ) )
        , Svg.Attributes.height ( String.fromFloat ( settings.y2 - settings.y1 ) )
        ] []

drawBackgroundLine : GameSettings -> Float -> Html.Html Msg
drawBackgroundLine settings scale =
    Svg.line
        [ Svg.Attributes.x1 ( String.fromFloat ( settings.x2 - settings.x1 ) )
        , Svg.Attributes.y1 ( String.fromFloat settings.y1 )
        , Svg.Attributes.x2 ( String.fromFloat ( settings.x2 - settings.x1 ) )
        , Svg.Attributes.y2 ( String.fromFloat settings.y2 )
        , Svg.Attributes.stroke "white"
        , Svg.Attributes.strokeDasharray ( String.fromFloat ( 10 * scale ) ++ "," ++ String.fromFloat ( 4 * scale ) )
        ] []

drawPlayer : Player -> Float -> Html.Html Msg
drawPlayer player scale =
    Svg.rect
        [ Svg.Attributes.x ( String.fromFloat player.x ) 
        , Svg.Attributes.y ( String.fromFloat player.y )
        , Svg.Attributes.rx ( String.fromFloat ( 10 * scale ) )
        , Svg.Attributes.ry ( String.fromFloat ( 10 * scale ) )
        , Svg.Attributes.width ( String.fromFloat player.width )
        , Svg.Attributes.height ( String.fromFloat player.height )
        , Svg.Attributes.fill "white"
        ] []

drawBall : Ball -> Html.Html Msg
drawBall ball = 
    Svg.circle
        [ Svg.Attributes.cx ( String.fromFloat ball.x )
        , Svg.Attributes.cy ( String.fromFloat ball.y )
        , Svg.Attributes.r ( String.fromFloat ball.r )
        , Svg.Attributes.fill "white"
        ] []

drawScore1 : Score -> GameSettings -> Float -> Html.Html Msg
drawScore1 score game scale =
    let
        fontSize = ( 50 * scale )
        middleScreenX = game.x2 - game.x1
        fontX = middleScreenX - fontSize
        fontY = game.y1 + fontSize

    in
        Svg.text_ 
            [ Svg.Attributes.fontSize ( String.fromFloat fontSize )
            , Svg.Attributes.x ( String.fromFloat fontX )
            , Svg.Attributes.y ( String.fromFloat fontY )
            , Svg.Attributes.fill "white"
            ] 
            [
                Svg.text ( String.fromInt score.player1 )
            ]

drawScore2 : Score -> GameSettings -> Float -> Html.Html Msg
drawScore2 score game scale =
    let
        fontSize = ( 50 * scale )
        middleScreenX = game.x2 - game.x1
        fontX = middleScreenX + ( fontSize/2 )
        fontY = game.y1 + fontSize

    in
        Svg.text_ 
            [ Svg.Attributes.fontSize ( String.fromFloat fontSize )
            , Svg.Attributes.x ( String.fromFloat fontX )
            , Svg.Attributes.y ( String.fromFloat fontY )
            , Svg.Attributes.fill "white"
            ] 
            [
                Svg.text ( String.fromInt score.player2 )
            ]

drawWaiting : Model -> Html.Html Msg
drawWaiting model =
    Svg.svg
        [ Svg.Attributes.width ( String.fromFloat ( model.window.width - model.window.width / ( 10 * model.window.scale ) ) )
        , Svg.Attributes.height ( String.fromFloat ( model.window.height - model.window.height / ( 5 * model.window.scale ) ) )
        ]
        [ drawBackground model.game model.window.scale
        , drawBackgroundLine model.game model.window.scale
        , drawPlayer model.player1 model.window.scale
        , drawPlayer model.player2 model.window.scale
        , drawBall model.ball
        , drawScore1 model.score model.game model.window.scale
        , drawScore2 model.score model.game model.window.scale
        , drawControls model.game model.window.scale
        ]

drawGameOver : Model -> Html.Html Msg
drawGameOver model =
    Svg.svg
        [ Svg.Attributes.width ( String.fromFloat ( model.window.width - model.window.width / ( 10 * model.window.scale ) ) )
        , Svg.Attributes.height ( String.fromFloat ( model.window.height - model.window.height / ( 5 * model.window.scale ) ) )
        ] 
        [ drawGame model
        , blurScreen model.game model.window.scale
        , drawGameOverMessage model.game model.score model.window.scale
        ]

drawGameOverMessage : GameSettings -> Score -> Float -> Html.Html Msg
drawGameOverMessage game score scale =
    let
        winner = 
            if score.player1 > score.player2 then
                "Player 1"
            
            else
                "Player 2"

        fontSize = 30 * scale
        middleWidth = game.x2 - game.x1
        message = winner ++ " won the game!"
        messagePosX = middleWidth - fontSize/2.4 * 11
        messagePosY = 200 * scale

    in
        Svg.text_ 
            [ Svg.Attributes.fontSize ( String.fromFloat fontSize )
            , Svg.Attributes.x ( String.fromFloat messagePosX )
            , Svg.Attributes.y ( String.fromFloat messagePosY )
            , Svg.Attributes.fill "white"
            ] 
            [
                Svg.text message 
            ]

blurScreen : GameSettings -> Float -> Html.Html Msg
blurScreen game scale =
    Svg.rect
        [ Svg.Attributes.x ( String.fromFloat game.x1 ) 
        , Svg.Attributes.y ( String.fromFloat game.y1 )
        , Svg.Attributes.rx ( String.fromFloat ( 10 * scale ) )
        , Svg.Attributes.ry ( String.fromFloat ( 10 * scale ) )
        , Svg.Attributes.width ( String.fromFloat ( game.x2 - game.x1 ) )
        , Svg.Attributes.height ( String.fromFloat ( game.y2 - game.y1 ) )
        , Svg.Attributes.fillOpacity "0.6"
        ] []

drawPause : Model -> Html.Html Msg
drawPause model =
    Svg.svg
        [ Svg.Attributes.width ( String.fromFloat ( model.window.width - model.window.width / ( 10 * model.window.scale ) ) )
        , Svg.Attributes.height ( String.fromFloat ( model.window.height - model.window.height / ( 5 * model.window.scale ) ) )
        ] 
        [ drawGame model
        , blurScreen model.game model.window.scale
        , drawPauseMessage model.game model.window.scale
        , drawControls model.game model.window.scale
        ]

drawPauseMessage : GameSettings -> Float -> Html.Html Msg
drawPauseMessage game scale =
    let
        fontSize = 30 * scale
        middleWidth = game.x2 - game.x1
        message = "Paused"
        messagePosX = middleWidth - fontSize/4.2 * 6
        messagePosY = 200 * scale

    in
        Svg.text_ 
            [ Svg.Attributes.fontSize ( String.fromFloat fontSize )
            , Svg.Attributes.x ( String.fromFloat messagePosX )
            , Svg.Attributes.y ( String.fromFloat messagePosY )
            , Svg.Attributes.fill "white"
            ] 
            [
                Svg.text message 
            ]    

drawControls : GameSettings -> Float -> Html.Html Msg
drawControls game scale =
    let
        fontSize = 20 * scale
        middleWidth = game.x2 - game.x1
        
        message1 = "P to pause and unpause"
        message1PosX = game.x1
        message1PosY = ( game.y2 + fontSize*2) 

        message2 = "Space to release the ball"
        message2PosX = game.x1
        message2PosY = ( game.y2 + fontSize*4 ) 

        message3 = "W/S to control player 1"
        message3PosX = game.x1
        message3PosY = ( game.y2 + fontSize*6 ) 

        message4 = "UpArrow/DownArrow to control player 2"
        message4PosX = game.x1
        message4PosY =  ( game.y2 + fontSize*8 )


    in    
        Svg.svg
        [] 
        [ Svg.text_ 
            [ Svg.Attributes.fontSize ( String.fromFloat fontSize )
            , Svg.Attributes.x ( String.fromFloat message1PosX )
            , Svg.Attributes.y ( String.fromFloat message1PosY )
            , Svg.Attributes.fill "black"
            ] 
            [
                Svg.text message1 
            ]
        , Svg.text_ 
            [ Svg.Attributes.fontSize ( String.fromFloat fontSize )
            , Svg.Attributes.x ( String.fromFloat message2PosX )
            , Svg.Attributes.y ( String.fromFloat message2PosY )
            , Svg.Attributes.fill "black"
            ] 
            [
                Svg.text message2 
            ]
        , Svg.text_ 
            [ Svg.Attributes.fontSize ( String.fromFloat fontSize )
            , Svg.Attributes.x ( String.fromFloat message3PosX )
            , Svg.Attributes.y ( String.fromFloat message3PosY )
            , Svg.Attributes.fill "black"
            ] 
            [
                Svg.text message3
            ]
        , Svg.text_ 
            [ Svg.Attributes.fontSize ( String.fromFloat fontSize )
            , Svg.Attributes.x ( String.fromFloat message4PosX )
            , Svg.Attributes.y ( String.fromFloat message4PosY )
            , Svg.Attributes.fill "black"
            ] 
            [
                Svg.text message4 
            ]
        ]    

drawMenu : Model -> Html.Html Msg
drawMenu model =
    Svg.svg [][]