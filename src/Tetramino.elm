module Tetramino exposing (..)


type alias Block =
    { x : Int
    , y : Int
    }


type alias Piece =
    { blocks : List Block
    , center : Block
    }
    
type Tetramino = I Piece 
    | O Piece
    | T Piece
    | S Piece
    | J Piece
    | Z Piece
    | L Piece

pos0 : Tetramino -> Piece
pos0 t 
    case t of
        I ->
            [ { x = 0, y = 1 }
            , { x = 1, y = 1 }
            , { x = 2, y = 1 }
            , { x = 3, y = 1 }
            ]
        O ->
            [ { x = 0, y = 0 }
            , { x = 0, y = 1 }
            , { x = 1, y = 0 }
            , { x = 1, y = 1 }
            ]
        T ->
            [ { x = 0, y = 1 }
            , { x = 1, y = 1 }
            , { x = 2, y = 1 }
            , { x = 1, y = 0 }
            ]
        S ->
            [ { x = 1, y = 0 }
            , { x = 2, y = 0 }
            , { x = 0, y = 1 }
            , { x = 1, y = 1 }
            ]
        J ->
            [ { x = 0, y = 0 }
            , { x = 0, y = 1 }
            , { x = 1, y = 1 }
            , { x = 2, y = 1 }
            ]
        Z ->
            [ { x = 0, y = 0 }
            , { x = 1, y = 0 }
            , { x = 1, y = 1 }
            , { x = 2, y = 1 }
            ]
        L ->
            [ { x = 2, y = 0 }
            , { x = 0, y = 1 }
            , { x = 1, y = 1 }
            , { x = 2, y = 1 }
            ]

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