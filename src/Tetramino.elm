module Tetramino exposing (..)

import Dict exposing (Dict)

import Matrix exposing(Matrix)
import Svg.Attributes exposing (origin)
import List exposing (foldl)
import Array exposing (fromList, get)
import Random

type Tile = Empty | Filled Tetramino | Shadow Tetramino

type Tetramino = I | O | T | S | J | Z | L 

type Rotation = R1 | R2 | R3 | R4

type alias Block =
    { x : Int
    , y : Int
    }
    
type alias Rotations =
    Dict String (Dict String (List Block))

type alias Piece =
    { tetramino : Tetramino
    , blocks : List Block
    , origin : Block
    , r : Rotation
    , grounded : Int
    }

initPiece : Tetramino -> Rotations -> Piece 
initPiece t rts =
        { tetramino = t
        , blocks  = getBlocks t R4 rts 
        , origin = { x = 0, y = 3}
        , r  = R4
        , grounded = 0
    }


pieceGenerator : Random.Generator Tetramino
pieceGenerator =
  Random.uniform I [O,T, S, J, Z, L ]

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
    testBlock = \block -> not ( block.x < 0 || block.y < 0 || block.x > x || block.y > y)
    reduceFun = \block prev-> prev && (testBlock block)
  in
    foldl reduceFun True blocks

testOverlapp : Piece -> Matrix Tile -> Bool
testOverlapp piece tablero =
    foldl 
        (\block prev -> prev && (noOverlap tablero block))
        True 
        <| addOriginOffset piece  

noOverlap : Matrix Tile -> Block -> Bool
noOverlap tablero {x, y} = 
    case (Matrix.get tablero x y) of
        Nothing -> False
        Just tile -> case tile of
            Filled _ -> False
            _ -> True


addOriginOffset : Piece -> List Block
addOriginOffset piece =
    List.map (\b -> {x = b.x + piece.origin.x, y = b.y + piece.origin.y}) piece.blocks


toString_t : Tetramino -> String
toString_t t = 
    case t of
        I -> "I"
        O -> "O"
        T -> "T"
        S -> "S"
        J -> "J"
        Z -> "Z"
        L -> "L"

toString_r : Rotation -> String
toString_r r = 
    case r of
        R1 -> "R1"
        R2 -> "R2"
        R3 -> "R3"
        R4 -> "R4"


-- getBlocks : String -> String -> Rotations -> List Block
getBlocks : Tetramino -> Rotation -> Rotations -> List Block
getBlocks t r rts =
    let
        rs = toString_r r
        rt = toString_t t
    in
        case (Dict.get rt rts) of 
            Just tetraRotations -> 
                case (Dict.get rs tetraRotations) of
                    Just tRotations -> tRotations
                    Nothing -> [ ]
            Nothing -> [ ]

 

rotations : Rotations
rotations = 
    Dict.fromList
        [("I" , rotationsI)
        ,("J" , rotationsJ)
        ,("L" , rotationsL)
        ,("O" , rotationsO)
        ,("T" , rotationsT)
        ,("S" , rotationsS)
        ,("Z" , rotationsZ)
        ]
    



rotationsI : Dict String (List Block)
rotationsI =
     Dict.fromList 
        [("R1", [ { x = 0, y = 2 }
                , { x = 1, y = 2 }
                , { x = 2, y = 2 }
                , { x = 3, y = 2 }])
        ,("R2", [ { x = 2, y = 0 }
                , { x = 2, y = 1 }
                , { x = 2, y = 2 }
                , { x = 2, y = 3 }])
                
        ,("R3", [ { x = 0, y = 1 }
                , { x = 1, y = 1 }
                , { x = 2, y = 1 }
                , { x = 3, y = 1 }])
                
        ,("R4", [ { x = 1, y = 0 }
                , { x = 1, y = 1 }
                , { x = 1, y = 2 }
                , { x = 1, y = 3 }])
        ]
rotationsJ : Dict String (List Block)
rotationsJ =
     Dict.fromList 
        [("R1", [ { x = 0, y = 1 }
                , { x = 0, y = 2 }
                , { x = 1, y = 1 }
                , { x = 2, y = 1 }])
        ,("R2", [ { x = 1, y = 0 }
                , { x = 1, y = 1 }
                , { x = 1, y = 2 }
                , { x = 2, y = 2 }])
        ,("R3", [ { x = 0, y = 1 }
                , { x = 1, y = 1 }
                , { x = 2, y = 1 }
                , { x = 2, y = 0 }])
        ,("R4", [ { x = 0, y = 0 }
                , { x = 1, y = 0 }
                , { x = 1, y = 1 }
                , { x = 1, y = 2 }])
        ]


rotationsL : Dict String (List Block)
rotationsL =
     Dict.fromList 
        [("R1", [ { x = 0, y = 1 }
                , { x = 1, y = 1 }
                , { x = 2, y = 1 }
                , { x = 2, y = 2 }])
        ,("R2", [ { x = 1, y = 0 }
                , { x = 1, y = 1 }
                , { x = 1, y = 2 }
                , { x = 2, y = 0 }])
        ,("R3", [ { x = 0, y = 0 }
                , { x = 0, y = 1 }
                , { x = 1, y = 1 }
                , { x = 2, y = 1 }])
        ,("R4", [ { x = 0, y = 2 }
                , { x = 1, y = 0 }
                , { x = 1, y = 1 }
                , { x = 1, y = 2 }])
        ]

rotationsO : Dict String (List Block)
rotationsO =
     Dict.fromList 
        [("R1", [ { x = 1, y = 1 }
                , { x = 1, y = 2 }
                , { x = 2, y = 1 }
                , { x = 2, y = 2 }])
        ,("R2", [ { x = 1, y = 1 }
                , { x = 1, y = 2 }
                , { x = 2, y = 1 }
                , { x = 2, y = 2 }]) 
        ,("R3", [ { x = 1, y = 1 }
                , { x = 1, y = 2 }
                , { x = 2, y = 1 }
                , { x = 2, y = 2 }])
        ,("R4", [ { x = 1, y = 1 }
                , { x = 1, y = 2 }
                , { x = 2, y = 1 }
                , { x = 2, y = 2 }])
        ]

rotationsS : Dict String (List Block)
rotationsS =
     Dict.fromList 
        [("R1", [ { x = 0, y = 1 }
                , { x = 1, y = 1 }
                , { x = 1, y = 2 }
                , { x = 2, y = 2 }])
        ,("R2", [ { x = 1, y = 1 }
                , { x = 1, y = 2 }
                , { x = 2, y = 1 }
                , { x = 2, y = 0 }])
        ,("R3", [ { x = 0, y = 0 }
                , { x = 1, y = 0 }
                , { x = 1, y = 1 }
                , { x = 2, y = 1 }])
        ,("R4", [ { x = 0, y = 2 }
                , { x = 0, y = 1 }
                , { x = 1, y = 1 }
                , { x = 1, y = 0 }])
        ]

rotationsZ : Dict String (List Block)
rotationsZ =
     Dict.fromList 
        [("R1", [ { x = 0, y = 2 }
                , { x = 1, y = 2 }
                , { x = 1, y = 1 }
                , { x = 2, y = 1 }])
        ,("R2", [ { x = 1, y = 0 }
                , { x = 1, y = 1 }
                , { x = 2, y = 1 }
                , { x = 2, y = 2 }])
        ,("R3", [ { x = 0, y = 1 }
                , { x = 1, y = 1 }
                , { x = 1, y = 0 }
                , { x = 2, y = 0 }])
        ,("R4", [ { x = 0, y = 0 }
                , { x = 0, y = 1 }
                , { x = 1, y = 1 }
                , { x = 1, y = 2 }])
        ]

rotationsT : Dict String (List Block)
rotationsT =
     Dict.fromList 
        [("R1", [ { x = 0, y = 1 }
                , { x = 1, y = 1 }
                , { x = 1, y = 2 }
                , { x = 2, y = 1 }])
        ,("R2", [ { x = 1, y = 0 }
                , { x = 1, y = 1 }
                , { x = 1, y = 2 }
                , { x = 2, y = 1 }])
        ,("R3", [ { x = 0, y = 1 }
                , { x = 1, y = 1 }
                , { x = 1, y = 0 }
                , { x = 2, y = 1 }])
        ,("R4", [ { x = 1, y = 0 }
                , { x = 1, y = 1 }
                , { x = 1, y = 2 }
                , { x = 0, y = 1 }])
        ]


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
        foldl reduceFunTests Nothing [0, 1, 2, 3]
        -- if (testMovement newPiece tablero) then newPiece else p 


kickOffset : Tetramino -> Rotation -> Rotation -> Int -> Block
kickOffset t rPrev rAct test =
    let
        dir = rotationDirection rPrev rAct
        block = case t of 
                    I -> 
                        case dir of
                            "clockwise" -> 
                                case rPrev of
                                    R1 -> get test <| fromList [{y=-1, x=0}, {y=-1, x=1}, {y=0, x=-1}, {y=-1, x=-1}]
                                    R2 -> get test <| fromList [{y=1, x=0}, {y=1, x=-1}, {y=0, x=1}, {y=1, x=1}] 
                                    R3 -> get test <| fromList [{y=1, x=0}, {y=1, x=1}, {y=0, x=-1}, {y=1, x=-1}]
                                    R4 -> get test <| fromList [{y=-1, x=0}, {y=-1, x=-1}, {y=0, x=1}, {y=-1, x=1}]
                            _ -> 
                                case rAct of
                                    R1 -> get test <| fromList [{y=1, x=0}, {y=1, x=-1}, {y=0, x=1}, {y=1, x=1}]
                                    R2 -> get test <| fromList [{y=-1, x=0}, {y=-1, x=1}, {y=0, x=-1}, {y=-1, x=-1}]
                                    R3 -> get test <| fromList [{y=-1, x=0}, {y=-1, x=-1}, {y=0, x=1}, {y=-1, x=1}]
                                    R4 -> get test <| fromList [{y=1, x=0}, {y=1, x=1}, {y=0, x=-1}, {y=1, x=-1}]
                    _ -> 
                        case dir of
                            "clockwise" -> 
                                case rPrev of 
                                    R1 -> get test <| fromList [{y=-1, x= 0}, {y=1, x= 0}, {y=-1, x=-1}, {y=1, x=1}]
                                    R2 -> get test <| fromList [{y=-1, x= 0}, {y=1, x= 0}, {y=-1, x=1}, {y=1, x=-1}]
                                    R3 -> get test <| fromList [{y=1, x= 0}, {y=-1, x= 0}, {y=1, x=1}, {y=-1, x=-1}]
                                    R4 -> get test <| fromList [{y=1, x= 0}, {y=-1, x= 0}, {y=1, x=-1}, {y=-1, x=1}]
                            _ -> 
                                case rAct of
                                    R1 -> get test <| fromList [{y=1, x= 0}, {y=-1, x= 0}, {y=2, x=1}, {y=-1, x=-1}]
                                    R2 -> get test <| fromList [{y=1, x= 0}, {y=-1, x= 0}, {y=1, x=-1}, {y=-1, x=1}]
                                    R3 -> get test <| fromList [{y=-1, x= 0}, {y=1, x= 0}, {y=-2, x=-1}, {y=1, x=1}]
                                    R4 -> get test <| fromList [{y=-1, x= 0}, {y=1, x= 0}, {y=-1, x=1}, {y=1, x=-1}]
    in 
        case block of
            Just b -> b
            Nothing -> {x = 0, y = 0}
        
        
rotationDirection : Rotation -> Rotation -> String
rotationDirection  rPrev rAct  =
    case (rPrev, rAct) of
        ( R1, R2 ) -> "clockwise"
        ( R2, R3 ) -> "clockwise"
        ( R3, R4 ) -> "clockwise"
        ( R4, R1 ) -> "clockwise"
        ( R1, R4 ) -> "counterclockwise"
        ( R4, R3 ) -> "counterclockwise"
        ( R3, R2 ) -> "counterclockwise"
        ( R2, R1 ) -> "counterclockwise"
        _ -> ""