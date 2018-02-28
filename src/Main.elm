module Main exposing (..)

import AnimationFrame
import Keyboard as KB
import Html as H exposing (Html)
import Html.Attributes as A
import Game exposing (Game)
import Pacman
import Maze
import Svg


main : Program Never Model Msg
main =
    H.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { game : Game
    , keycode : List Int
    }


init : ( Model, Cmd Msg )
init =
    ( { game = Game.init, keycode = [] }, Cmd.none )



-- UPDATE


type Msg
    = Tick
    | KeyDown Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model | game = Game.tick model.game }, Cmd.none )

        KeyDown code ->
            ( { model | game = Game.changeDirection code model.game }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs (always Tick)
        , KB.downs KeyDown
        ]



-- VIEW


view : Model -> Html msg
view { game, keycode } =
    Svg.svg
        [ A.attribute "namespace" "http://www.w3.org/2000/svg"
        , A.style [ ( "width", "100%" ), ( "height", "100%" ) ]
        ]
        [ Maze.svg game.maze
            [ Pacman.svg game.pacman

            -- Ghost.svg game.blinky,
            -- Ghost.svg game.pinky,
            -- Ghost.svg game.inky,
            -- Ghost.svg game.clyde,
            --
            -- Bonus.srv game.currentBonus
            ]
        ]
