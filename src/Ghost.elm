module Ghost exposing (..)

import Entity exposing (Entity)
import Direction exposing (Direction(Left, Right, Up, Down))
import Pacman exposing (Pacman)
import Position
import Square exposing (Square)
import Svg as S exposing (Svg)
import Svg.Attributes as A


type Colour
    = Red
    | Pink
    | LightBlue
    | Orange



-- Ghost Model


{-| This model is used to extend the character model giving it some useful extra properties
-}
type alias Ghost =
    { colour : Colour
    , homeTarget : Square
    }



-- Ghost Initialisers


inky : Entity Ghost
inky =
    { position = Position.fromSquare { x = 14, y = 20 }
    , speed = 0.75
    , direction = Left
    , colour = LightBlue
    , homeTarget = { x = 1, y = 1 }
    }


blinky : Entity Ghost
blinky =
    { position = Position.fromSquare { x = 13, y = 22 }
    , speed = 0.75
    , direction = Left
    , colour = Red
    , homeTarget = { x = 1, y = 1 }
    }


pinky : Entity Ghost
pinky =
    { position = Position.fromSquare { x = 12, y = 11 }
    , speed = 0.75
    , direction = Left
    , colour = Pink
    , homeTarget = { x = 1, y = 1 }
    }


clyde : Entity Ghost
clyde =
    { position = Position.fromSquare { x = 10, y = 13 }
    , speed = 0.75
    , direction = Left
    , colour = Orange
    , homeTarget = { x = 1, y = 1 }
    }



-- Chase Targeting


{-| Of all the ghosts' targeting schemes for chase mode, Blinky's is the most
    simple and direct, using Pac-Man's current tile as his target.
-}
chaseTargetBlinky : Entity pacman -> Square
chaseTargetBlinky pacman =
    Position.toSquare pacman.position


{-| In chase mode, Pinky behaves as he does because he does not target
    Pac-Man's tile directly. Instead, he selects an offset four tiles away from
    Pac-Man in the direction Pac-Man is currently moving. *Except* if Pac-Man
    is moving up, Pinky's target tile will be four tiles up and four tiles to
    the left. This is due to an integer overflow error.

    [Further Reading](http://donhodges.com/pacman_pinky_explanation.htm)
-}
chaseTargetPinky : Entity pacman -> Square
chaseTargetPinky pacman =
    case pacman.direction of
        Up ->
            pacman.position
                |> Position.toSquare
                |> Square.up 4
                |> Square.left 4

        dir ->
            pacman.position
                |> Position.toSquare
                |> Square.inDirection dir 4


{-| Inky uses the most complex targeting scheme of the four ghosts in chase
    mode. He needs Pac-Man's current tile/orientation *and* Blinky's current
    tile to calculate his final target. To determine Inky's target, we must
    first establish an intermediate offset two tiles in front of Pac-Man in the
    direction he is moving. Now imagine drawing a vector from the center of the
    red ghost's current tile to the center of the offset tile, then double the
    vector length by extending it out just as far again beyond the offset tile.

    For the same reasons already discussed in Pinky's case, an overflow error
    occurs with the intermediate offset tile generated for Inky's calculation
    when Pac-Man is moving up resulting in an offset tile that is two tiles
    above and two tiles to the left
-}
chaseTargetInky : Entity Pacman -> Entity Ghost -> Square
chaseTargetInky pacman blinky =
    let
        offsetTile =
            case pacman.direction of
                Up ->
                    pacman.position
                        |> Position.toSquare
                        |> Square.up 2
                        |> Square.left 2

                dir ->
                    pacman.position
                        |> Position.toSquare
                        |> Square.inDirection dir 2

        blinkySquare =
            Position.toSquare blinky.position

        distanceX =
            blinkySquare.x - offsetTile.x

        distanceY =
            blinkySquare.y - offsetTile.y
    in
        { x = offsetTile.x - distanceX
        , y = offsetTile.y - distanceY
        }


{-| Clyde's targeting logic changes based on his proximity to Pac-Man. He first
    calculates the Euclidean distance between his tile and Pac-Man's tile. If
    the distance between them is eight tiles or more, Clyde targets Pac-Man
    directly just as Blinky does. If the distance between them is less than
    eight tiles, however, Clyde switches his target to the tile he normally
    uses during scatter mode and heads for his corner until he gets far enough
    away to start targeting Pac-Man again.
-}
chaseTargetClyde : Entity Pacman -> Entity Ghost -> Square
chaseTargetClyde pacman clyde =
    let
        pacmanSquare =
            Position.toSquare pacman.position

        clydeSquare =
            Position.toSquare clyde.position
    in
        if Square.euclideanDistance pacmanSquare clydeSquare >= 8 then
            pacmanSquare
        else
            clyde.homeTarget


toHex : Colour -> String
toHex colour =
    case colour of
        Red ->
            "#d03e19"

        Pink ->
            "#ea82e5"

        LightBlue ->
            "#46bfee"

        Orange ->
            "#db851c"


svg : Entity Ghost -> Svg msg
svg ghost =
    let
        colour =
            toHex ghost.colour
    in
        S.svg
            [ A.viewBox "0 0 200 200"
            , A.preserveAspectRatio "xMinYMin meet"
            , A.width "10"
            , A.x <| toString <| Position.getX ghost.position
            , A.y <| toString <| Position.getY ghost.position
            ]
            [ S.circle
                [ A.style <| "fill:" ++ colour
                , A.cx "100"
                , A.cy "100"
                , A.r "100"
                ]
                []
            ]
