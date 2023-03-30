module Main exposing (main, Model, GameState)

import Html exposing (Html, div, h1, h3, p, pre, text, button)
import Html.Attributes as Attrs exposing (style)
import Html.Events exposing (onClick)

import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Key

-- import Keyboard.Event.KeyCode exposing (Key(..))
import Browser
import Browser.Events exposing (onKeyDown)
import Json.Decode as Json
import Matrix exposing(Matrix, initialize, repeat, neighbours, set)
import Time
import Process
import Task

import Tablero exposing (Tablero, initTablero, viewTablero)
import Tetramino exposing (..)

type alias Model =

    {   points: Int
      , tablero: Tablero
      , lastEvent : Maybe KeyboardEvent
      , state: GameState
      , active : Maybe Piece
      , next : Maybe Piece
      , hold : Maybe Piece
      , rotations : Rotations
    }


type GameState = Paused | Starting Int | Playing | NotStarted

type Msg =
      HandleKeyboardEvent KeyboardEvent
    -- handle startup
    | Countdown Int
    | Advance
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Countdown n ->
            case n of
                3 ->  ( { model | 
                            tablero = initTablero
                            , points = 0
                            , state = (Starting n)} 
                    , (delay 1000 (Countdown (n - 1))))

                0 -> ( { model | state = Playing, active = Just <| initPiece T model.rotations} , (delay 1000 Advance))

                _ ->  ( { model | state = (Starting n)} , (delay 1000 (Countdown (n - 1))))

        Advance ->
            ({model | active = advancePiece model.active model.tablero },  (delay 1000 Advance))

        HandleKeyboardEvent event ->
            let
                newModel = { model | lastEvent = Just event }
            in
                if model.state == Playing then
                    case event.keyCode of
                        Key.Right ->  ({newModel | active = moveRight model.active model.tablero}, Cmd.none)
                        Key.Left ->   ({newModel | active = moveLeft model.active model.tablero}, Cmd.none)
                        Key.Up -> ({newModel | active = rotateRight model.active model.tablero model.rotations }, Cmd.none)
                        Key.Down ->  ({newModel | active = advancePiece model.active model.tablero}, Cmd.none)
                        Key.C -> (newModel, Cmd.none)
                        Key.Z -> ({newModel | active = rotateLeft model.active model.tablero model.rotations}, Cmd.none)
                        Key.Spacebar -> (newModel, Cmd.none)
                        Key.Escape -> (newModel, Cmd.none)
                        _ -> (newModel, Cmd.none)
                else
                    (newModel, Cmd.none)


        None -> (model, Cmd.none)



delay : Float -> msg -> Cmd msg
delay time msg =    
    -- create a task that sleeps for `time`
    Process.sleep time
        |> -- once the sleep is over, ignore its output (using `always`)
           -- and then we create a new task that simply returns a success, and the msg
           Task.andThen (always <| Task.succeed msg)
        |> -- finally, we ask Elm to perform the Task, which
           -- takes the result of the above task and
           -- returns it to our update function
           Task.perform identity


view : Model -> Html Msg
view model =
    let
        countdown = case model.state of
            Starting n -> n
            _ -> 0
    in    
        Html.main_
            [Attrs.id "tetris"]
            [ viewStartButton model.state
            , viewTablero model.tablero model.active countdown 
            , viewEvent model.lastEvent]


viewStartButton : GameState -> Html Msg
viewStartButton state = 
    case state of
        NotStarted -> button [onClick <| Countdown 3] [ text "Start" ]
        _ -> button [ Attrs.disabled True] [ text "Start" ]


        
viewEvent : Maybe KeyboardEvent -> Html Msg
viewEvent maybeEvent =
    case maybeEvent of
        Just event ->
            pre []
                [ text <|
                    String.join ""
                        [ "altKey: " ++ Debug.toString event.altKey
                        , "ctrlKey: " ++ Debug.toString event.ctrlKey
                        , "key: " ++ Debug.toString event.key
                        , "keyCode: " ++ Debug.toString event.keyCode
                        , "metaKey: " ++ Debug.toString event.metaKey
                        , "repeat: " ++ Debug.toString event.repeat
                        , "shiftKey: " ++ Debug.toString event.shiftKey
                        ]
                ]

        Nothing ->
            p [] [ text "No event yet" ]


-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.
--
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Json.map HandleKeyboardEvent decodeKeyboardEvent)
        ]


init : ( Model, Cmd Msg )
init =
    ( {   lastEvent = Nothing 
        , points = 0
        , tablero = initTablero
        , state = NotStarted
        , active = Nothing
        , next = Nothing
        , hold = Nothing
        , rotations = rotations
      }
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
        