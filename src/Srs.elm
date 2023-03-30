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

wallKickTable : WallKickTable
wallKickTable =
    Dict.fromList
        [ (0, Dict.fromList
            [ (True, WallKickData { offset = { x = -1, y = 0 }, clockwiseOffsets = [ { x = -1, y = 0 }, { x = -1, y = 1 }, { x = 0, y = -2 }, { x = -1, y = -2 } ], counterclockwiseOffsets = [ { x = 1, y = 0 }, { x = 1, y = 1 }, { x = 0, y = -2 }, { x = 1, y = -2 } ] })
            , (False, WallKickData { offset = { x = 1, y = 0 }, clockwiseOffsets = [ { x = 1, y = 0 }, { x = 1, y = -1 }, { x = 0, y = 2 }, { x = 1, y = 2 } ], counterclockwiseOffsets = [ { x = -1, y = 0 }, { x = -1, y = -1 }, { x = 0, y = 2 }, { x = -1, y = 2 } ] })
            ])
        , (1, Dict.fromList
            [ (True, WallKickData { offset = { x = 0, y = 0 }, clockwiseOffsets = [ { x = 1, y = 0 }, { x = 1, y = -1 }, { x = 0, y = 2 }, { x = 1, y = 2 } ], counterclockwiseOffsets = [ { x = -1, y = 0 }, { x = -1, y = -1 }, { x = 0, y = 2 }, { x = -1, y = 2 } ] })
            , (False, WallKickData { offset = { x = 0, y = 0 }, clockwiseOffsets = [ { x = -1, y = 0 }, { x = -1, y = 1 }, { x = 0, y = -2 }, { x = -1, y = -2 } ], counterclockwiseOffsets = [ { x = 1, y = 0 }, { x = 1, y = 1 }, { x = 0, y = -2 }, { x = 1, y = -2 } ] })
            ])
        , (2, Dict.fromList
            [ (True, WallKickData { offset = { x = 0, y = 0 }, clockwiseOffsets = [ { x = -1, y = 0 }, { x = -1, y = -1 }, { x = 0, y = 2 }, { x =-2, y= } ], counterclockwiseOffsets = [ { x = 1, y = 0 }, { x = 1, y = -1 }, { x = 0, y = 2 }, { x = 1, y = 2 } ] })
            , (False, WallKickData { offset = { x = 0, y = 0 }, clockwiseOffsets = [ { x = 1, y = 0 }, { x = 1, y = -1 }, { x = 0, y = 2 }, { x = 1, y = 2 } ], counterclockwiseOffsets = [ { x = -1, y = 0 }, { x = -1, y = 1 }, { x = 0, y = -2 }, { x = -1, y = -2 } ] })
            ])
        , (3, Dict.fromList
            [ (True, WallKickData { offset = { x = 1, y = 0 }, clockwiseOffsets = [ { x = 1, y = 0 }, { x = 1, y = 1 }, { x = 0, y = -2 }, { x = 1, y = -2 } ], counterclockwiseOffsets = [ { x = -1, y = 0 }, { x = -1, y = 1 }, { x = 0, y = -2 }, { x = -1, y = -2 } ] })
            , (False, WallKickData { offset = { x = -1, y = 0 }, clockwiseOffsets = [ { x = -1, y = 0 }, { x = -1, y = -1 }, { x = 0, y = 2 }, { x = -1, y = 2 } ], counterclockwiseOffsets = [ { x = 1, y = 0 }, { x = 1, y = -1 }, { x = 0, y = 2 }, { x = 1, y = 2 } ] })
            ])
        ]
