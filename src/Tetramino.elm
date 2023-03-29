module Tetramino exposing (..)

import Dict exposing (Dict)



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
    }

initPiece : Tetramino -> Rotations -> Piece 
initPiece t rts =
        { tetramino = t
        , blocks  = getBlocks t R1 rts 
        , origin = { x = 0, y = 3}
        , r  = R1
    }

advancePiece : Maybe Piece -> Maybe Piece 
advancePiece p =
    Maybe.map (\piece -> {piece | origin = {x = piece.origin.x + 1, y = piece.origin.y}}) p

moveLeft : Maybe Piece -> Maybe Piece 
moveLeft p =
  Maybe.map (\piece -> {piece | origin = {x = piece.origin.x, y = piece.origin.y - 1}}) p
  
moveRight : Maybe Piece -> Maybe Piece 
moveRight p =
  Maybe.map (\piece -> {piece | origin = {x = piece.origin.x, y = piece.origin.y + 1}}) p

rotateRight : Maybe Piece -> Rotations -> Maybe Piece
rotateRight piece rts = 
    Maybe.map (\p -> 
                case p.r of
                    R1 -> (rotate rts R2 p)
                    R2 -> (rotate rts R3 p)
                    R3 -> (rotate rts R4 p)
                    R4 -> (rotate rts R1 p))  
        piece
        

rotateLeft : Maybe Piece -> Rotations -> Maybe Piece
rotateLeft piece rts = 
    Maybe.map (\p -> 
            case p.r of
                R1 ->  (rotate rts R4 p)
                R2 ->  (rotate rts R1 p)
                R3 ->  (rotate rts R2 p)
                R4 ->  (rotate rts R3 p))
        piece
        

rotate : Rotations -> Rotation -> Piece -> Piece
rotate rts r p = 
    {p | blocks = (getBlocks p.tetramino p.r rts), r = r}


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
        ,("R4", [ { x = 0, y = 0 }
                , { x = 1, y = 1 }
                , { x = 1, y = 2 }
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
