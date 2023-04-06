module Tetramino exposing (..)

import Dict exposing (Dict)

import Array exposing (fromList, get)
import Random
import Html exposing (a)

type Tetramino = I | O | T | S | J | Z | L 


type Rotation = R1 | R2 | R3 | R4


type alias Block =
    { x : Int
    , y : Int
    }
    

type alias Rotations =
    Dict String (Dict String (List Block))


tetraminoGenerator : Random.Generator Tetramino
tetraminoGenerator =
  Random.uniform I [O,T, S, J, Z, L ]


duplicados : List Block -> List Block -> Bool
duplicados lista1 lista2 =
    let
        bloques = List.append lista1 lista2
        sinDuplicados = List.foldl (\bloque bloquesSinDuplicados->
            if List.member bloque bloquesSinDuplicados then
                bloquesSinDuplicados
            else
                bloque :: bloquesSinDuplicados)
         [] bloques

    in
    List.length bloques /= List.length sinDuplicados




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
        [ ("I" , rotationsI)
        , ("J" , rotationsJ)
        , ("L" , rotationsL)
        , ("O" , rotationsO)
        , ("T" , rotationsT)
        , ("S" , rotationsS)
        , ("Z" , rotationsZ)
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

