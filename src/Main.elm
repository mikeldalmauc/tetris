module Main exposing (main, Model, GameState)

import Html exposing (Html, div, p, text, button)
import Html.Attributes as Attrs 
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
import Tablero exposing (..)
import Tetramino exposing (Tetramino(..), Rotations, tetraminoGenerator, rotations)
import Time
import Matrix
import Array exposing (length)
import Dict exposing (Dict)

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
      , previousMsg : Msg
      , actualMsg : Msg
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
update msg modl =
    let
        model = { modl | previousMsg = modl.previousMsg, actualMsg = msg }
    in
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
                        Key.Spacebar -> 
                            groundPiece {model | active = sinkPiece model.active model.tablero}
                        Key.C -> 
                            holdPiece model
                        Key.Escape -> 
                            ( {model | state = Paused, pausedState = model.state},  Cmd.none)     

                                
                        _ -> (model, Cmd.none)
                                
                else
                    case event.keyCode of
                        Key.Escape -> ( {model | state = if model.state == Paused then Playing else model.state },  Cmd.none)
                        Key.Enter ->  (model, issueMsgAsCmd <| Countdown 3)
                        _ -> (model, Cmd.none)

            Pause paused -> ( {model | state = if paused then Paused else model.pausedState
                                    , pausedState = model.state}, Cmd.none)

            None -> (model, Cmd.none)


pointsTable : Dict String Int
pointsTable = 
    Dict.fromList [
         ("1" , 40)
        ,("2" , 100)
        ,("3" , 300)
        ,("4" , 1200)
    ]

mapPoints : String -> Int
mapPoints i = case Dict.get i pointsTable of
    Just p -> p
    Nothing -> 0
             
issueMsgAsCmd : Msg -> Cmd Msg
issueMsgAsCmd msg =
    Task.perform identity (Task.succeed msg)


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
    Random.generate NewPiece tetraminoGenerator
    
  
groundPiece : Model -> ( Model, Cmd Msg )
groundPiece model = 
    case model.active of
        Just piece ->  
            if piece.grounded < 3 then
                if piece.grounded  > 0 && model.previousMsg == model.actualMsg && model.actualMsg == Advance then
                    groundPiece {model | active = Just {piece | grounded = 3}}
                else
                    ({ model | active = Just (testGrounded piece model.tablero)}, Cmd.none)
            else
                let 
                    testedPiece = testGrounded piece model.tablero
                    isEndGame = piece.origin.x <= 0
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

    in
        { model | tablero = newTablero
            , points = model.points + (mapPoints  <| toString amount)
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
            , viewControls
            , viewTablero model.tablero model.active countdown "t-tablero"
            , viewTablero (initTablero 4 6) model.hold 0 "t-hold"
            , viewTablero (initTablero 4 6 ) model.next 0 "t-next"]


viewStartButton : GameState -> Html Msg
viewStartButton state = 
    case state of
        NotStarted -> button [onClick <| Countdown 3, Attrs.class "t-start"] [ text "Start" ]
        Finished -> button [onClick <| Countdown 3, Attrs.class "t-start"] [ text "Start" ]
        _ -> button [ Attrs.disabled True, Attrs.hidden True] [ text "Start" ]

viewPauseButton : GameState -> Html Msg
viewPauseButton state = 
    case state of
        Paused -> button [onClick <| Pause False, Attrs.class "t-resume"] [ text "Resume" ]
        Playing -> button [onClick <| Pause True, Attrs.class "t-pause"] [ text "Pause" ]
        _ ->  div [] []


viewContador : Int -> Html msg
viewContador puntos = 
    Html.section 
        [Attrs.classList [ ("t-contador", True )]]
        [Html.span [ Attrs.class "t-count-points"] [text (toString puntos)]]


viewControls : Html msg
viewControls =
    Html.section
        [ Attrs.class "t-controls" ]
        [ Html.h4 [] [ Html.text "Controles" ]
        , Html.ul []
            [ Html.li [] [ Html.b [] [text "\u{2190}"], text " Izquierda" ]
            , Html.li [] [ Html.b [] [text "\u{2192}"], text " Derecha" ]
            , Html.li [] [ Html.b [] [text "\u{2193}"], text " Abajo" ]
            , Html.li [] [ Html.b [] [text "\u{2191}"], text " Rotar derecha" ]
            , Html.li [] [ Html.b [] [text "Z"], text " Rotar izquierda" ]
            , Html.li [] [ Html.b [] [text "Space"], text " Fijar pieza" ]
            , Html.li [] [ Html.b [] [text "C"], text " Hold" ]
            , Html.li [] [ Html.b [] [text "Esc"], text " Pause / Restart" ]
            , Html.li [] [ Html.b [] [text "Enter"], text " Start" ]
            ]
        , Html.h4 [] [text "Puntos"] 
        , Html.ul []
             (List.append [Html.li [] [Html.b [] [text "Rows - Points"]]] 
                <| Dict.values
                <| Dict.map ( \key value -> Html.li [] [ Html.b [] [text key],text " - ", text (toString value) ] ) pointsTable)
            
        ]
        

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
        , previousMsg = None
        , actualMsg = None
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
        