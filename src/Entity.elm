module Entity exposing (..)

import Direction exposing (Direction)
import Maze exposing (Maze)
import Position exposing (Position)
import Speed exposing (Speed)


type alias Entity a =
    { a
        | position : Position
        , speed : Speed
        , direction : Direction
    }


{-| Do two entities accupy the same square?
-}
collision : Entity a -> Entity b -> Bool
collision x y =
    Position.squareCollision x.position y.position


move : Maze -> Entity a -> Entity a
move maze entity =
    let
        position =
            Position.move maze entity.direction entity.speed entity.position
    in
        { entity | position = position }


updateDirection : Direction -> Entity a -> Entity a
updateDirection dir entity =
    { entity | direction = dir }
