module Tablero exposing (..)

import Html exposing (Html, li, ul, text, span)
import Matrix exposing(Matrix)
import Html.Attributes as Attrs exposing (style)
import Array exposing (Array)
import Array exposing (toList)
import List
import Debug exposing (toString)
import Tetramino exposing (Tetramino, cssClass)

type alias Coordinate =
    { x : Int
    , y : Int
    }
    
type alias Tablero = Matrix Tile

type Tile = Empty | Filled Tetramino

initTablero : Tablero
initTablero = Matrix.repeat 20 10 Empty


viewTablero : Tablero -> Int -> Html msg
viewTablero tablero countdown = 
    let
        (x, y) = Matrix.size tablero

    in
        Html.section 
            [Attrs.class "t-tablero"]
            <| List.concat [
                  [viewCountdown countdown]
                , List.map (\row -> viewRow (Matrix.getXs tablero row)) (List.range 0 (x - 1))
            ]


viewCountdown :  Int -> Html msg
viewCountdown n = 
    case n of
        0 -> span [][]
        _ -> 
            span [ Attrs.class "t-countdown"] [text (toString n)]


viewRow : Array Tile -> Html msg
viewRow row = 
    ul 
        [ Attrs.class "t-row"]
        (toList (Array.map viewTile row))


viewTile : Tile -> Html msg
viewTile tile = 
    case tile of
        Filled tetramino ->
            li 
                [ Attrs.classList [("t-tile", True), (cssClass tetramino, True)]]
                []
        Empty -> 
            li
                [Attrs.class "t-tile-empty"]
                []


