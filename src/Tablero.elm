module Tablero exposing (..)

import Html exposing (Html, li, ul, text, span)
import Matrix exposing(Matrix)
import Html.Attributes as Attrs
import Array exposing (toList, Array)
import List
import Debug exposing (toString)
import Dict exposing (Dict)
import Tetramino exposing (Tetramino(..), Rotation(..), Rotations, kickOffset, Block, getBlocks)

type Tile = Empty | Filled Tetramino | Shadow Tetramino


type alias Tablero = Matrix Tile

type alias Piece =
    { tetramino : Tetramino
    , blocks : List Block
    , origin : Block
    , r : Rotation
    , grounded : Int
    }

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

initPiece : Tetramino -> Rotations -> Piece 
initPiece t rts =
        { tetramino = t
        , blocks  = getBlocks t R4 rts 
        , origin = { x = -1, y = 3}
        , r  = R4
        , grounded = 0
    }

initTablero : Int -> Int -> Tablero
initTablero rows cols = Matrix.repeat rows cols Empty


insertPiece : Piece -> Tablero -> Tablero
insertPiece piece tablero =
    List.foldl (\block newTablero -> Matrix.set newTablero (block.x+piece.origin.x) (block.y+piece.origin.y) (Filled piece.tetramino)) tablero piece.blocks 


insertShadow : Piece -> Tablero -> Tablero
insertShadow piece tablero =
    List.foldl 
        (\block newTablero -> 
            let
                x = block.x + piece.origin.x
                y =  block.y + piece.origin.y
            in
                case Matrix.get tablero x y of
                    Nothing -> newTablero
                    Just Empty -> Matrix.set newTablero x y (Shadow piece.tetramino)
                    Just _ -> newTablero
        ) tablero piece.blocks 
    

-- Para cada bloque, mediamos la distancia con el 
-- primer bloque inmediatamente debajo. La distancia mínima entre
-- todas es donde se posaría la pieza y el valor a sumar a cada 
-- elemento de piece para calcular las coordenadas de la sombra
shadowPosition : Piece -> Tablero -> Maybe Piece
shadowPosition piece tablero = 
    if (testGrounded piece tablero).grounded > 0 then 
        Nothing
    else
        let
            -- disctancia desde el top del tablero
            medirAltura = \block -> 
                Matrix.getYs tablero (block.y + piece.origin.y)
                |> Array.foldl (
                        \tile (distance, found) -> 
                            case tile of 
                                Filled _ -> (distance, True)
                                _ -> if found then (distance, found) else (distance + 1, False)
                            ) 
                (0, False) 
                |> distanceToGround block
            
            distanceToGround = \block (distance, found)  -> 
                if found then 
                    (block, distance)
                else 
                    (block, Tuple.first <| Matrix.size tablero)

            calcularDeltas = \blocks -> List.map (\(block, distance) -> distance - (block.x + piece.origin.x)- 1) blocks

            sumarDeltas = \d ->
                case d of
                    Just deltaV -> Just { piece | origin = { x = piece.origin.x + deltaV, y = piece.origin.y}}
                    Nothing -> Nothing

        in
            piece.blocks 
                |> List.map medirAltura
                |> calcularDeltas
                |> List.minimum
                |> sumarDeltas 

sinkPiece : Maybe Piece -> Tablero -> Maybe Piece
sinkPiece piece tablero =
    case piece of 
        Just p -> 
            case (shadowPosition p tablero) of 
                Just shadow -> Just { p | origin = shadow.origin, grounded = 3 } 
                Nothing -> Just p
        Nothing -> Nothing


advancePiece : Maybe Piece -> Matrix Tile -> Maybe Piece 
advancePiece p tablero =
    Maybe.map (\piece -> 
      let
        newPiece = {piece | origin = {x = piece.origin.x + 1, y = piece.origin.y}}
      in
        if (testMovement newPiece tablero) then newPiece else piece
      ) p


moveLeft : Maybe Piece -> Matrix Tile -> Maybe Piece 
moveLeft p tablero =
  Maybe.map (\piece ->
      let
        newPiece = {piece | origin = {x = piece.origin.x, y = piece.origin.y - 1}}
      in
        if (testMovement newPiece tablero) then newPiece else piece
      ) p


moveRight : Maybe Piece -> Matrix Tile  -> Maybe Piece 
moveRight p tablero =
  Maybe.map (\piece ->
      let
        newPiece = {piece | origin = {x = piece.origin.x, y = piece.origin.y + 1}}
      in
        if (testMovement newPiece tablero) then newPiece else piece
    ) p


rotateRight : Maybe Piece -> Matrix Tile -> Rotations -> Maybe Piece
rotateRight piece tablero rts = 
    Maybe.map (\p -> 
            let 
              newPiece = case p.r of
                          R1 -> rotate rts R2 p
                          R2 -> rotate rts R3 p
                          R3 -> rotate rts R4 p
                          R4 -> rotate rts R1 p
           in
            if (testMovement newPiece tablero) then newPiece
            else Maybe.withDefault p (attemptKick p.r newPiece tablero)
      ) piece
      

rotateLeft : Maybe Piece -> Matrix Tile -> Rotations -> Maybe Piece
rotateLeft piece tablero rts = 
    Maybe.map (\p -> 
            let 
              newPiece = case p.r of
                R1 ->  rotate rts R4 p
                R2 ->  rotate rts R1 p
                R3 ->  rotate rts R2 p
                R4 ->  rotate rts R3 p
            in
              if (testMovement newPiece tablero) then newPiece 
              else Maybe.withDefault p (attemptKick p.r newPiece tablero)
           ) piece
        

rotate : Rotations -> Rotation -> Piece -> Piece
rotate rts r p = 
    {p | blocks = (getBlocks p.tetramino r rts), r = r}


addOriginOffset : Piece -> List Block
addOriginOffset piece =
    List.map (\b -> {x = b.x + piece.origin.x, y = b.y + piece.origin.y}) piece.blocks



testMovement : Piece -> Matrix Tile -> Bool
testMovement piece tablero =
    if (testOutsideBounds piece tablero ) then
      testOverlapp piece tablero
    else
      False


testOutsideBounds : Piece -> Matrix Tile -> Bool
testOutsideBounds piece tablero =
  let
    ( x, y ) = Matrix.size tablero 
    blocks = addOriginOffset piece 
    testBlock = \block -> not ( block.y < 0 || block.x >= x || block.y >= y )

    reduceFun = \block prev-> prev && (testBlock block)
  in
    List.foldl reduceFun True blocks


testOverlapp : Piece -> Matrix Tile -> Bool
testOverlapp piece tablero =
    List.foldl 
        (\block prev -> prev && (noOverlap tablero block))
        True 
        <| addOriginOffset piece  


noOverlap : Matrix Tile -> Block -> Bool
noOverlap tablero {x, y} =
    case (Matrix.get tablero x y) of
        Nothing -> True
        Just tile -> case tile of
            Filled _ -> False
            _ -> True

testGrounded : Piece -> Tablero -> Piece
testGrounded piece tablero = 
    let 
        piecesInContact = List.any 
            (\b -> case Matrix.get tablero (b.x + piece.origin.x + 1) (b.y + piece.origin.y) of
                        Just (Filled _) -> True
                        Just (_) ->  False
                        Nothing -> True
            ) piece.blocks 
    in
        if piecesInContact then 
            {piece | grounded = piece.grounded + 1}
        else
            {piece | grounded = 0}


attemptKick : Rotation -> Piece -> Matrix Tile -> Maybe Piece
attemptKick rPrev piece tablero = 
    let
        reduceFunTests = \test prev -> 
            case prev of
                Just p -> Just p
                Nothing -> 
                    let 
                        kick = kickOffset piece.tetramino rPrev piece.r test
                        newPiece = { piece | origin = { x = kick.x + piece.origin.x, y = kick.y + piece.origin.y}}
                    in
                        if (testMovement newPiece tablero) then Just newPiece else Nothing
    in
        List.foldl reduceFunTests Nothing [0, 1, 2, 3]
        -- if (testMovement newPiece tablero) then newPiece else p 
--
--
--  View Section 
-- 
-- 
-- ___________________________________________________________________________________________________

viewTablero : Tablero -> Maybe Piece -> Int -> String -> Html msg
viewTablero tablero piece countdown class = 
    let
        (x, y) = Matrix.size tablero
        tableroWithActive = 
            case piece of 
                Just p -> case class of 
                    "t-tablero" -> insertPiece p tablero
                    _ -> insertPiece {p | blocks =  
                            case p.tetramino of
                                O -> List.map (\b -> {x = b.x + 1 , y= b.y - 3}) p.blocks
                                _ -> List.map (\b -> {x = b.x + 2, y= b.y - 2}) p.blocks} tablero
                Nothing -> tablero
        
        tableroWithShadow = if not ("t-tablero" == class) then tableroWithActive
            else
                case piece of
                    Just p -> 
                        let
                            shadow = shadowPosition p tablero
                        in    
                            case shadow of
                                Nothing -> tableroWithActive
                                Just s -> insertShadow s tableroWithActive
                    Nothing -> tableroWithActive

        
    in
        Html.section 
            [Attrs.class class]
            <| List.concat [
                  [viewCountdown countdown]
                , List.map (\row -> viewRow (Matrix.getXs tableroWithShadow row) row) (List.range 0 (x - 1))
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
        Shadow tetramino -> 
            li
                [Attrs.classList [("t-tile-empty", True), ("t-shadow", True)] ]
                []
        Empty -> 
            li
                [Attrs.class "t-tile-empty"]
                []



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