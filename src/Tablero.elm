module Tablero exposing (..)

import Html exposing (Html, li, ul, text, span)
import Matrix exposing(Matrix)
import Html.Attributes as Attrs exposing (style)
import Array exposing (Array)
import Array exposing (toList)
import List
import Debug exposing (toString)
import Tetramino exposing (Tetramino(..), Piece, Tile(..))
import Array exposing (foldl)
import Dict exposing (Dict)


type alias Tablero = Matrix Tile

initTablero : Int -> Int -> Tablero
initTablero rows cols = Matrix.repeat rows cols Empty


viewTablero : Tablero -> Maybe Piece -> Int -> Html msg
viewTablero tablero piece countdown = 
    let
        (x, y) = Matrix.size tablero
        renderedTablero = 
            case piece of 
                Just p -> insertPiece p tablero
                Nothing -> tablero
    in
        Html.section 
            [Attrs.class "t-tablero"]
            <| List.concat [
                  [viewCountdown countdown]
                , List.map (\row -> viewRow (Matrix.getXs renderedTablero row) row) (List.range 0 (x - 1))
            ]


insertPiece : Piece -> Tablero -> Tablero
insertPiece piece tablero =
    List.foldl (\block newTablero -> Matrix.set newTablero (block.x+piece.origin.x) (block.y+piece.origin.y) (Filled piece.tetramino)) tablero piece.blocks 


testGrounded : Piece -> Tablero -> Piece
testGrounded piece tablero = 
    let 
        piecesInContact = List.any 
            (\b -> case Matrix.get tablero (b.x + piece.origin.x + 1) (b.y + piece.origin.y) of
                        Just (Empty) ->  False
                        Just (Filled _) -> True
                        Nothing -> True
            ) piece.blocks 
    in
        if piecesInContact then 
            {piece | grounded = piece.grounded + 1}
        else
            {piece | grounded = 0}


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



cssClass : Tetramino -> String
cssClass t = 
    case t of
        I -> "t-i"
        O -> "t-o"
        T -> "t-t"
        S -> "t-s"
        J -> "t-j"
        Z -> "t-z"
        L -> "t-l"