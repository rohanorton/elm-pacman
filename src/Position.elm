module Position
    exposing
        ( Position
        , fromSquare
        , toSquare
        , squareCollision
        , move
        , getX
        , getY
        )

import Direction exposing (Direction)
import Maze exposing (Maze)
import Speed exposing (Speed)
import Square exposing (Square)


type alias Exact =
    { x : Float, y : Float }


{-| For most of our character interaction we only care about Square, but for
    animation we need a more exact representation of position.

    Holding onto to values to avoid doing lots of flooring of position values.
    I can't tell if this is a premature optimisation.
-}
type Position
    = Position Square Exact


fromSquare : Square -> Position
fromSquare square =
    square
        |> center
        |> Position square


toSquare : Position -> Square
toSquare (Position square _) =
    square


center : Square -> Exact
center square =
    { x = (toFloat square.x + 0.5) * 10
    , y = (toFloat square.y + 0.5) * 10
    }


{-| The move function is called on tick for each entity.
-}
move : Maze -> Direction -> Speed -> Position -> Position
move maze dir speed (Position square exact) =
    let
        nextSquare =
            dir
                |> Direction.toCoord
                |> Square.add square

        nextCell =
            Maze.getCell nextSquare maze

        canMove =
            case nextCell of
                Maze.Wall ->
                    False

                _ ->
                    True

        directionCoord =
            Direction.toCoord dir

        x =
            exact.x + (toFloat directionCoord.x * speed)

        y =
            exact.y + (toFloat directionCoord.y * speed)

        newSquare =
            { x = floor x // 10
            , y = floor y // 10
            }

        newExact =
            { x = x
            , y = y
            }
    in
        if canMove then
            Position newSquare newExact
        else
            Position square exact


squareCollision : Position -> Position -> Bool
squareCollision (Position s1 _) (Position s2 _) =
    s1 == s2


getX : Position -> Float
getX (Position _ exact) =
    exact.x


getY : Position -> Float
getY (Position _ exact) =
    exact.y
