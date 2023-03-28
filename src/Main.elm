
module Main exposing (main, Model, GameState)

{-| HEADS UP! You can view this example alongside the running code at


We're going to make confetti come out of the party popper emoji: 🎉
([emojipedia](https://emojipedia.org/party-popper/)) Specifically, we're going
to lift our style from [Mutant Standard][ms], a wonderful alternate emoji set,
which is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike
4.0 International License.

[ms]: https://mutant.tech/

-}


import Html exposing (Attribute, Html, div, h1, h3, p, pre, text, button)
import Html.Attributes as Attrs exposing (style)
import Html.Events exposing (onClick)
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Browser
import Browser.Events exposing (onKeyDown)
import Json.Decode as Json
import Matrix exposing(Matrix, initialize, repeat, neighbours, set)
import Time
import Process
import Task

import Tablero exposing (Tablero, initTablero, viewTablero)

type alias Model =

    {   points: Int
      , tablero: Tablero
      , lastEvent : Maybe KeyboardEvent
      , state: GameState
    }


type GameState = Paused | Starting Int | Playing | NotStarted

type Msg =
      HandleKeyboardEvent KeyboardEvent
    -- handle startup
    | Countdown Int
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Countdown n ->
            case n of
                3 ->  ( { model | 
                      tablero =initTablero
                    , points = 0
                    , state = (Starting n)} , (delay 1000 (Countdown (n-1))))

                0 -> ( { model | state = Playing} , Cmd.none)

                _ ->  ( { model | state = (Starting n)} , (delay 1000 (Countdown (n-1))))


        HandleKeyboardEvent event ->
            ( { model | lastEvent = Just event }
            , Cmd.none
            )

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
            [Attrs.id "buscaminas"]
            [ button [ onClick <| Countdown 3 ] [ text "Start" ]
            , viewTablero model.tablero countdown
            , viewEvent model.lastEvent]




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

-- SUBSCRIPTIONS

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
        