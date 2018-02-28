module Direction exposing (..)


type Direction
    = Up
    | Left
    | Down
    | Right


type alias Coordinate =
    { x : Int, y : Int }


fromKeyCode : Int -> Maybe Direction
fromKeyCode code =
    case code of
        37 ->
            Just Left

        38 ->
            Just Up

        39 ->
            Just Right

        40 ->
            Just Down

        _ ->
            Nothing


toCoord : Direction -> Coordinate
toCoord dir =
    case dir of
        Up ->
            { x = 0, y = -1 }

        Left ->
            { x = -1, y = 0 }

        Down ->
            { x = 0, y = 1 }

        Right ->
            { x = 1, y = 0 }


next : Direction -> Direction
next dir =
    case dir of
        Up ->
            Left

        Left ->
            Down

        Down ->
            Right

        Right ->
            Up


{-| If two routes are deemed equidistant to the target, we can select a
    direction based on the weight. Higher numbers are higher priority.
-}
weight : Direction -> Int
weight dir =
    case dir of
        Up ->
            4

        Left ->
            3

        Down ->
            2

        Right ->
            1
