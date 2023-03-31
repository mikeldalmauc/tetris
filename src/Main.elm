module Main exposing (main, Model, GameState)

import Html exposing (Html, div, h1, h3, p, pre, text, button)
import Html.Attributes as Attrs exposing (style)
import Html.Events exposing (onClick)
import Set exposing (empty, insert, member)
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Key
import Debug exposing (toString)
import Browser
import Browser.Events exposing (onKeyDown)
import Json.Decode as Json
import Process
import Task
import Random
import Tablero exposing (Tablero, initTablero, viewTablero, insertPiece, testGrounded)
import Tetramino exposing (..)
import Time
import Matrix
import Array 
import Array exposing (length)
import Svg.Attributes exposing (origin)


type alias Model =

    {   points: Int
      , tablero: Tablero
      , pausedState : GameState
      , state: GameState
      , active : Maybe Piece
      , next : Maybe Piece
      , hold : Maybe Piece
      , rotations : Rotations
      , stepTime : Float
      , holdAvailable : Bool
    }


type GameState = Paused | Starting Int | Playing | NotStarted | Finished

type Msg =
      HandleKeyboardEvent KeyboardEvent
    -- handle startup
    | Countdown Int
    | NewPiece Tetramino
    | Advance
    | Pause Bool
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Countdown n ->
            case n of
                3 ->  ( { model | 
                              tablero = initTablero 20 10
                            , points = 0
                            , state = (Starting n)
                            , holdAvailable = True
                            , active = Nothing
                            , next = Nothing
                            , hold = Nothing
                             } 
                    , Cmd.batch [newPiece, (delay 1000 (Countdown (n - 1)))])

                0 -> ( { model | state = Playing} , newPiece)

                _ ->  ( { model | state = (Starting n)} , (delay 1000 (Countdown (n - 1))))

        NewPiece t -> 
            case model.state of
                Playing -> ({ model | state = Playing, active = model.next, next = Just <| initPiece t model.rotations}, Cmd.none)
                Starting s -> ({ model | state = Starting s, active = model.next, next = Just <| initPiece t model.rotations}, Cmd.none)
                _ -> (model, Cmd.none)
                
        Advance ->
            if model.state == Playing then
                case model.active of
                    Nothing -> (model, Cmd.none)
                    Just _ -> groundPiece {model | active = advancePiece model.active model.tablero}
            else
                (model, Cmd.none)
        HandleKeyboardEvent event ->
            if model.state == Playing then
                case event.keyCode of
                    Key.Right ->  
                        groundPiece {model | active = moveRight model.active model.tablero}
                    Key.Left ->  
                        groundPiece {model | active = moveLeft model.active model.tablero}
                    Key.Up -> 
                        groundPiece {model | active = rotateRight model.active model.tablero model.rotations }
                    Key.Down ->  
                        groundPiece {model | active = advancePiece model.active model.tablero}
                    Key.Z -> 
                        groundPiece {model | active = rotateLeft model.active model.tablero model.rotations}
                    Key.C -> 
                        holdPiece model
                    Key.Escape -> 
                        ( {model | state = Paused, pausedState = model.state},  Cmd.none)     
                    Key.Spacebar -> (model, Cmd.none)
                    _ -> (model, Cmd.none)
            else
                case event.keyCode of
                    Key.Escape -> ( {model | state = if model.state == Paused then Playing else model.state },  Cmd.none)
                    _ -> (model, Cmd.none)

        Pause paused -> ( {model | state = if paused then Paused else model.pausedState
                                , pausedState = model.state}, Cmd.none)

        None -> (model, Cmd.none)


holdPiece : Model -> ( Model, Cmd Msg ) 
holdPiece model = 
    if model.holdAvailable then
        case model.active of
            Nothing -> (model, Cmd.none)
            Just piece -> 
                let
                    toBeHolded = Just <| initPiece piece.tetramino model.rotations
                in
                    case model.hold of
                        Nothing -> ({model | hold = toBeHolded, holdAvailable = False}, newPiece)
                        Just holded -> 
                            let
                                translated = rotate model.rotations piece.r {holded | origin = piece.origin, grounded = piece.grounded}
                            in
                                if  (testMovement translated model.tablero) then
                                    ({model | hold = toBeHolded , active = Just translated , holdAvailable = False}, Cmd.none)
                                else
                                    (model, Cmd.none)
    else
        (model, Cmd.none)

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
                let 
                    testedPiece = testGrounded piece model.tablero
                    isEndGame = piece.origin.x == 0
                in
                    if testedPiece.grounded == 0 
                    then  ({ model | active = Just {testedPiece | grounded = 2}}, Cmd.none)
                    else  
                        if isEndGame then
                            ( { model | active = Nothing
                            ,  tablero = insertPiece piece model.tablero
                            , holdAvailable = False, state = Finished}, Cmd.none)
                        else 
                            ( clearRows piece { model | tablero = insertPiece piece model.tablero , active = Nothing, holdAvailable = True}, newPiece)
        Nothing -> (model, Cmd.none)


clearRows :  Piece -> Model -> Model
clearRows piece model =
    let
        -- This is a predicate that checks wehther a certain row is filled of non empty tiles
        -- I case this is true the row could be deleted
        max = \list -> Array.foldl (\l actualMax -> if l > actualMax then l else actualMax) 0 list

        isFull = \row tfull -> 
            Array.foldl (\tile prev -> 
                case tile of
                    Empty -> prev && False
                    Filled _ -> prev && True
                    Shadow _ -> prev && False
            ) True 
            <| Matrix.getXs tfull row 

        clear = \tclear rows -> 
            if (length rows) > 0 then
                (Array.foldl 
                     (\r t -> Array.foldl (\y tt -> (Matrix.set tt r y Empty)) t <| (Array.initialize (Tuple.second (Matrix.size t)) identity) )
                     tclear 
                     rows, Array.length rows, max rows)
            else
                (tclear, Array.length rows, max rows)

        getTopNeighbour = \tneigh x y gap -> 
            case Matrix.get tneigh (x - gap) y of
                Just (Filled p) -> Filled p
                Just _ -> Empty
                Nothing -> Empty

        pushFilled = \(tpush, rows, maxRow) ->Tuple.pair 
            rows
            (if rows > 0 then Matrix.indexedMap (\x y c -> if x > maxRow then c else getTopNeighbour tpush x y rows) tpush else tpush)

        (amount, newTablero) = addOriginOffset piece 
                    |> List.map (\b -> b.x)
                    |> removeDuplicates
                    |> List.filter (\r -> isFull r model.tablero)
                    |> Array.fromList
                    |> clear model.tablero
                    |> pushFilled

        mapPoints = \rows -> case rows of 
            1 -> 40
            2 -> 100
            3 -> 300
            4 -> 1200
            _ -> 0
    in
        { model | tablero = newTablero
            , points = model.points + (mapPoints  amount)
        }


removeDuplicates : List Int -> List Int
removeDuplicates list =
    let
        set = empty
        addIfNew = \item s -> 
            if member item s then
                s
            else
                insert item s
    in
        List.foldr addIfNew Set.empty list |> Set.toList


        
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
            , viewPauseButton model.state
            , viewContador model.points
            , viewTablero model.tablero model.active countdown "t-tablero"
            , viewTablero (initTablero 4 6) model.hold 0 "t-hold"
            , viewTablero (initTablero 4 6 ) model.next 0 "t-next"]


viewStartButton : GameState -> Html Msg
viewStartButton state = 
    case state of
        NotStarted -> button [onClick <| Countdown 3] [ text "Start" ]
        Finished -> button [onClick <| Countdown 3] [ text "Start" ]
        _ -> button [ Attrs.disabled True] [ text "Start" ]

viewPauseButton : GameState -> Html Msg
viewPauseButton state = 
    case state of
        Paused -> button [onClick <| Pause False] [ text "Resume" ]
        Playing -> button [onClick <| Pause True] [ text "Pause" ]
        _ ->  div [] []


viewContador : Int -> Html msg
viewContador puntos = 
    Html.section 
        [Attrs.classList [ ("t-contador", True )]]
        [Html.span [ Attrs.class "t-count-points"] [text (toString puntos)]]


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
    ( {   points = 0
        , tablero = initTablero 20 10
        , pausedState = NotStarted
        , state = NotStarted
        , active = Nothing
        , next = Nothing
        , hold = Nothing
        , rotations = rotations
        , stepTime = 1000.0
        , holdAvailable = False
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
        