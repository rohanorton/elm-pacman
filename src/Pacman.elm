module Pacman exposing (Pacman, init, move, svg)

import Direction exposing (Direction(Left, Right, Down, Up))
import Entity exposing (Entity)
import Maze exposing (Maze)
import Position
import Svg as S exposing (Svg)
import Svg.Attributes as A


type alias Pacman =
    -- When pacman eats a pill he is stationary for 3 frames
    { eatingCountdown : Int
    }


init : Entity Pacman
init =
    { position = Position.fromSquare { x = 15, y = 21 }
    , speed = 0.8
    , direction = Right
    , eatingCountdown = 0
    }


{-| Move pacman once eating countdown has reached 0.
-}
move : Maze -> Entity Pacman -> Entity Pacman
move maze pacman =
    if (pacman.eatingCountdown > 0) then
        { pacman | eatingCountdown = pacman.eatingCountdown - 1 }
    else
        Entity.move maze pacman


{-| When pacman eats a pill he stops moving for 3 frames. This function sets
    the countdown.
-}
eatPill : Entity Pacman -> Entity Pacman
eatPill pacman =
    { pacman | eatingCountdown = 3 }


svg : Entity Pacman -> Svg a
svg pacman =
    let
        rotate =
            case pacman.direction of
                Right ->
                    "0"

                Down ->
                    "90"

                Left ->
                    "180"

                Up ->
                    "270"

        yellow =
            "#fdff00"
    in
        S.svg
            [ A.viewBox "0 0 200 200"
            , A.preserveAspectRatio "xMinYMin meet"
            , A.width "10"
            , A.x <| toString <| Position.getX pacman.position
            , A.y <| toString <| Position.getY pacman.position
            ]
            [ S.circle
                [ A.style <| "fill:" ++ yellow
                , A.cx "100"
                , A.cy "100"
                , A.r "100"
                ]
                []
            ]
