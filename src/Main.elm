module Main exposing (main, Model, GameState)

import Html exposing (Html, div, h1, h3, p, pre, text, button)
import Html.Attributes as Attrs exposing (style)
import Html.Events exposing (onClick)

import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Key

import Browser
import Browser.Events exposing (onKeyDown)
import Json.Decode as Json
import Process
import Task
import Random
import Tablero exposing (Tablero, initTablero, viewTablero, insertPiece, testGrounded)
import Tetramino exposing (..)
import Time

type alias Model =

    {   points: Int
      , tablero: Tablero
      , lastEvent : Maybe KeyboardEvent
      , state: GameState
      , active : Maybe Piece
      , next : Maybe Piece
      , hold : Maybe Piece
      , rotations : Rotations
      , stepTime : Float
    }


type GameState = Paused | Starting Int | Playing | NotStarted

type Msg =
      HandleKeyboardEvent KeyboardEvent
    -- handle startup
    | Countdown Int
    | NewPiece Tetramino
    | Advance
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Countdown n ->
            case n of
                3 ->  ( { model | 
                              tablero = initTablero 20 10
                            , points = 0
                            , state = (Starting n)} 
                    , (delay 1000 (Countdown (n - 1))))

                0 -> ( { model | state = Playing} , newPiece)

                _ ->  ( { model | state = (Starting n)} , (delay 1000 (Countdown (n - 1))))

        NewPiece t -> 
             ({ model | state = Playing, active = Just <| initPiece t model.rotations}, Cmd.none)

        Advance ->
            case model.active of
                Nothing -> (model, Cmd.none)
                Just _ -> ({model | active = advancePiece model.active model.tablero}, Cmd.none)

        HandleKeyboardEvent event ->
            if model.state == Playing then
                case event.keyCode of
                    Key.Right ->  
                        let
                            newModel = {model | active = moveRight model.active model.tablero}
                        in
                            groundPiece newModel
                    Key.Left ->  
                        let
                            newModel = {model | active = moveLeft model.active model.tablero}
                        in
                            groundPiece newModel

                    Key.Up -> 
                        let
                            newModel = {model | active = rotateRight model.active model.tablero model.rotations }
                        in
                            groundPiece newModel
                    Key.Down ->  
                        let
                            newModel = {model | active = advancePiece model.active model.tablero}
                        in
                            groundPiece newModel
                    Key.Z -> 
                        let 
                            newModel = {model | active = rotateLeft model.active model.tablero model.rotations}
                        in
                            groundPiece newModel
                    Key.C -> (model, Cmd.none)
                    Key.Spacebar -> (model, Cmd.none)
                    Key.Escape -> (model, Cmd.none)
                    _ -> (model, Cmd.none)
            else
                (model, Cmd.none)

        None -> (model, Cmd.none)


newPiece : Cmd Msg
newPiece =
    Random.generate NewPiece pieceGenerator
    
  
groundPiece : Model -> ( Model, Cmd Msg )
groundPiece model = 
    case model.active of
        Just piece ->  
            if piece.grounded < 3 then 
               ({ model | active = Just (testGrounded piece model.tablero)}, Cmd.none)
            else
               ({ model | tablero = insertPiece piece model.tablero , active = Nothing}, newPiece)
        Nothing -> (model, Cmd.none)


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
            , viewTablero model.tablero model.active countdown]


viewStartButton : GameState -> Html Msg
viewStartButton state = 
    case state of
        NotStarted -> button [onClick <| Countdown 3] [ text "Start" ]
        _ -> button [ Attrs.disabled True] [ text "Start" ]


-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.
--
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Json.map HandleKeyboardEvent decodeKeyboardEvent)
        , Time.every model.stepTime (\_ -> Advance)]


init : ( Model, Cmd Msg )
init =
    ( {   lastEvent = Nothing 
        , points = 0
        , tablero = initTablero 20 10
        , state = NotStarted
        , active = Nothing
        , next = Nothing
        , hold = Nothing
        , rotations = rotations
        , stepTime = 1000.0
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
        