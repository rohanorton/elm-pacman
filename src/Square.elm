module Square exposing (..)

import Direction exposing (Direction(Left, Right, Down, Up))


type alias Square =
    { x : Int, y : Int }


up : Int -> Square -> Square
up n pos =
    { pos | y = pos.y + n }


down : Int -> Square -> Square
down n pos =
    { pos | y = pos.y - n }


right : Int -> Square -> Square
right n pos =
    { pos | x = pos.x + n }


left : Int -> Square -> Square
left n pos =
    { pos | x = pos.x - n }


inDirection : Direction -> Int -> Square -> Square
inDirection dir n pos =
    case dir of
        Left ->
            left n pos

        Down ->
            down n pos

        Right ->
            right n pos

        Up ->
            up n pos


euclideanDistance : Square -> Square -> Int
euclideanDistance posA posB =
    (posB.x - posA.x) * 2 + (posB.y - posA.y) * 2


add : Square -> Square -> Square
add a b =
    { x = a.x + b.x
    , y = a.y + b.y
    }
