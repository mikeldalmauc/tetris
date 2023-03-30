module Tablero exposing (..)

import Html exposing (Html, li, ul, text, span)
import Matrix exposing(Matrix)
import Html.Attributes as Attrs exposing (style)
import Array exposing (Array)
import Array exposing (toList)
import List
import Debug exposing (toString)
import Tetramino exposing (Tetramino, Piece, cssClass, Tile(..))
import Array exposing (foldl)
import Dict exposing (Dict)


type alias Tablero = Matrix Tile

initTablero : Tablero
initTablero = Matrix.repeat 20 10 Empty


viewTablero : Tablero -> Maybe Piece -> Int -> Html msg
viewTablero tablero piece countdown = 
    let
        (x, y) = Matrix.size tablero
        renderedTablero = 
            case piece of 
                Just p -> List.foldl (\block newTablero -> Matrix.set newTablero (block.x+p.origin.x) (block.y+p.origin.y) (Filled p.tetramino)) tablero p.blocks 
                Nothing -> tablero
    in
        Html.section 
            [Attrs.class "t-tablero"]
            <| List.concat [
                  [viewCountdown countdown]
                , List.map (\row -> viewRow (Matrix.getXs renderedTablero row) row) (List.range 0 (x - 1))
            ]


viewCountdown :  Int -> Html msg
viewCountdown n = 
    case n of
        0 -> span [][]
        _ -> 
            span [ Attrs.class "t-countdown"] [text (toString n)]


viewRow : Array Tile -> Int -> Html msg
viewRow row index = 
    ul 
        [ Attrs.class "t-row"]
        (toList (Array.map (\r -> viewTile r index) row))


viewTile : Tile -> Int -> Html msg
viewTile tile rIndex = 
    case tile of
        Filled tetramino ->
            li 
                [ Attrs.classList [("t-tile", True), (cssClass tetramino, True)]]
                []
        Empty -> 
            li
                [Attrs.class "t-tile-empty"]
                [text <| toString rIndex]


