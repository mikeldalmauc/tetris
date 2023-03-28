module Tablero exposing (..)

import Html exposing (Html, li, ul, text, span)
import Matrix exposing(Matrix)
import Html.Attributes as Attrs exposing (style)
import Array exposing (Array)
import Array exposing (toList)
import List
import Debug exposing (toString)

type alias Tablero = Matrix Tile

type Tetrad = I | O | T | S | J | Z | L 

type Tile = Empty | Filled Tetrad


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
            span [ style "font-size" "10em"
            ][text (toString n)]


viewRow : Array Tile -> Html msg
viewRow row = 
    ul 
        [ Attrs.class "t-row"]
        (toList (Array.map viewTile row))

viewTile : Tile -> Html msg
viewTile tile = 
    case tile of
        Filled tetrad ->
            li 
                [ Attrs.classList [("t-tile", True), (tileSvg tetrad, True)]]
                []
        Empty -> 
            li
                [Attrs.class "t-tile-empty"]
                []

tileSvg : Tetrad -> String
tileSvg t = 
    case t of
        I -> "t-i"
        O -> "t-o"
        T -> "t-t"
        S -> "t-s"
        J -> "t-j"
        Z -> "t-z"
        L -> "t-l"

