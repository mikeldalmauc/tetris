module Srs exposing (..)


type alias Coordinate =
    { x : Int
    , y : Int
    }

type alias Tetromino =
    List Coordinate

type alias Board =
    Dict Coordinate Int

insertTetromino : Board -> Tetromino -> Board
insertTetromino board tetromino =
    List.foldl (\coord -> Dict.insert coord 1) board tetromino


type alias Block =
    { x : Int
    , y : Int
    }

tetrominoCoordinates : Tetramino -> List {x : Int, y : Int}
tetrominoCoordinates t =
    case t of
        I ->
            [ { y = 0, x = 1 }
            , { y = 1, x = 1 }
            , { y = 2, x = 1 }
            , { y = 3, x = 1 }
            ]
        O ->
            [ { y = 0, x = 0 }
            , { y = 0, x = 1 }
            , { y = 1, x = 0 }
            , { y = 1, x = 1 }
            ]
        T ->
            [ { y = 0, x = 1 }
            , { y = 1, x = 1 }
            , { y = 2, x = 1 }
            , { y = 1, x = 0 }
            ]
        S ->
            [ { y = 1, x = 0 }
            , { y = 2, x = 0 }
            , { y = 0, x = 1 }
            , { y = 1, x = 1 }
            ]
        J ->
            [ { y = 0, x = 0 }
            , { y = 0, x = 1 }
            , { y = 1, x = 1 }
            , { y = 2, x = 1 }
            ]
        Z ->
            [ { y = 0, x = 0 }
            , { y = 1, x = 0 }
            , { y = 1, x = 1 }
            , { y = 2, x = 1 }
            ]
        L ->
            [ { y = 2, x = 0 }
            , { y = 0, x = 1 }
            , { y = 1, x = 1 }
            , { y = 2, x = 1 }
            ]
            
type alias Piece =
    { blocks : List Block
    , center : Block
    }

type alias RotationState =
    { clockwise : Bool
    , rotationIndex : Int
    }

type alias Tetromino =
    { pieces : List Piece
    , rotationState : RotationState
    }

type alias WallKickData =
    { offset : Block
    , clockwiseOffsets : List Block
    , counterclockwiseOffsets : List Block
    }

type alias WallKickTable =
    Dict Int (Dict Bool WallKickData)

