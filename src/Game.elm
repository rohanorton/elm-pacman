module Game exposing (..)

import Direction exposing (Direction)
import Pacman exposing (Pacman)
import Ghost exposing (Ghost)
import Entity exposing (Entity)
import Maze exposing (Maze)


-- Bonus Symbols


type BonusSymbol
    = Cherry
    | Strawberry
    | Peach
    | Apple
    | Grapes
    | Galaxian
    | Bell
    | Key



-- Game


type alias Game =
    { maze : Maze

    -- Entitys
    , pacman : Entity Pacman
    , inky : Entity Ghost
    , pinky : Entity Ghost
    , blinky : Entity Ghost
    , clyde : Entity Ghost

    -- Game State
    , score : Int
    , lives : Int
    , level : Int
    , timer : Int
    , paused : Bool
    , collision : Bool
    , bonusSymbol : Maybe BonusSymbol
    }


init : Game
init =
    { maze = Maze.init

    -- Entitys
    , pacman = Pacman.init
    , inky = Ghost.inky
    , pinky = Ghost.pinky
    , blinky = Ghost.blinky
    , clyde = Ghost.clyde

    -- Game State
    , score = 0
    , lives = 3
    , level = 1
    , timer = 0
    , paused = False
    , collision = False
    , bonusSymbol = Nothing
    }


tick : Game -> Game
tick game =
    game
        |> moveBlinky
        |> movePinky
        |> moveInky
        |> moveClyde
        |> movePacman
        |> eatenPills
        |> pacmanCollidedWithGhost


moveBlinky : Game -> Game
moveBlinky game =
    { game | blinky = Entity.move game.maze game.blinky }


moveInky : Game -> Game
moveInky game =
    { game | inky = Entity.move game.maze game.inky }


movePinky : Game -> Game
movePinky game =
    { game | pinky = Entity.move game.maze game.pinky }


moveClyde : Game -> Game
moveClyde game =
    { game | clyde = Entity.move game.maze game.clyde }


movePacman : Game -> Game
movePacman game =
    { game | pacman = Pacman.move game.maze game.pacman }


pacmanCollidedWithGhost : Game -> Game
pacmanCollidedWithGhost game =
    let
        ghosts =
            [ game.inky, game.blinky, game.pinky, game.clyde ]

        collision =
            List.any (Entity.collision game.pacman) ghosts
    in
        { game | collision = collision }


eatenPills : Game -> Game
eatenPills game =
    game


changeDirection : Int -> Game -> Game
changeDirection keycode game =
    let
        updateGame dir =
            { game | pacman = Entity.updateDirection dir game.pacman }
    in
        keycode
            |> Direction.fromKeyCode
            |> Maybe.map updateGame
            |> Maybe.withDefault game
