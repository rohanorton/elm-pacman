module Maze exposing (Maze, Cell(..), getCell, init, simpleView, svg)

import Array exposing (Array)
import Html as H exposing (Html)
import Html.Attributes as A
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import String
import Square exposing (Square)


{-| It would probably make sense to represent the Maze as a 2d matrix
-}
type Maze
    = Maze (Array (Array Cell))


type Cell
    = Pill
    | Energizer
    | Wall
    | Empty
    | Decision Cell


init : Maze
init =
    createMazeFromString mazeString


mazeString : String
mazeString =
    """
############################
#............##............#
#.####.#####.##.#####.####.#
#o#``#.#```#.##.#```#.#``#o#
#.####.#####.##.#####.####.#
#..........................#
#.####.##.########.##.####.#
#.####.##.########.##.####.#
#......##....##....##......#
######.#####`##`#####.######
`````#.#####`##`#####.#`````
`````#.##``````````##.#`````
`````#.##`###``###`##.#`````
######.##`#``````#`##.######
``````.```#``````#```.``````
######.##`#``````#`##.######
`````#.##`########`##.#`````
`````#.##``````````##.#`````
`````#.##`########`##.#`````
######.##`########`##.######
#............##............#
#.####.#####.##.#####.####.#
#.####.#####.##.#####.####.#
#o..##................##..o#
###.##.##.########.##.##.###
###.##.##.########.##.##.###
#......##....##....##......#
#.##########.##.##########.#
#.##########.##.##########.#
#..........................#
############################
"""



-- Blah


createMazeFromString : String -> Maze
createMazeFromString str =
    let
        maze =
            str
                |> String.trim
                |> String.split ("\n")
                |> List.map toCols
                |> Array.fromList

        toCols row =
            row
                |> String.split ("")
                |> List.map toCells
                |> Array.fromList

        toCells str =
            case str of
                "." ->
                    Pill

                "o" ->
                    Energizer

                "#" ->
                    Wall

                _ ->
                    Empty
    in
        Maze maze


simpleView : Maze -> Html msg
simpleView (Maze maze) =
    let
        rows =
            maze
                |> Array.map toRow
                |> Array.toList

        toRow a =
            a
                |> Array.map simpleCellView
                |> Array.toList
                |> H.tr []
    in
        H.table [ A.style [ ( "text-align", "center" ) ] ] rows


simpleCellView : Cell -> Html msg
simpleCellView cell =
    let
        basicStyles bg =
            [ ( "background-color", bg )
            , ( "color", "white" )
            , ( "width", "20px" )
            , ( "height", "20px" )
            ]
    in
        case cell of
            Empty ->
                H.td [ A.style <| basicStyles "black" ] []

            Wall ->
                H.td [ A.style <| basicStyles "blue" ] []

            Pill ->
                H.td [ A.style <| basicStyles "black" ] [ H.text "·" ]

            Energizer ->
                H.td [ A.style <| basicStyles "black" ] [ H.text "●" ]

            Decision c ->
                simpleCellView c


svg : Maze -> List (Svg a) -> Svg a
svg _ children =
    Svg.svg
        [ SvgA.height "600", SvgA.width "600" ]
        children



-- <svg xmlns="http://www.w3.org/2000/svg" height="571.11" width="541.6">
-- Get neighboring cells


getCell : Square -> Maze -> Cell
getCell { x, y } (Maze maze) =
    maze
        |> Array.get y
        |> Maybe.withDefault (Array.fromList [])
        |> Array.get x
        |> Maybe.withDefault Empty


getRight : Square -> Maze -> Cell
getRight { x, y } =
    getCell { x = x + 1, y = y }


getLeft : Square -> Maze -> Cell
getLeft { x, y } =
    getCell { x = x - 1, y = y }


getUp : Square -> Maze -> Cell
getUp { x, y } =
    getCell { x = x, y = y - 1 }


getDown : Square -> Maze -> Cell
getDown { x, y } =
    getCell { x = x, y = y + 1 }


generateSvg : Maze -> Svg a
generateSvg maze =
    Svg.text "oh"



--
-- cornerTopLeft : Svg a
-- cornerTopLeft =
--     Debug.crash "TODO"
--
--
-- cornerBottomLeft : Svg a
-- cornerBottomLeft =
--     Debug.crash "TODO"
--
--
-- cornerTopRight : Svg a
-- cornerTopRight =
--     Debug.crash "TODO"
--
--
-- cornerBottomRight : Svg a
-- cornerBottomRight =
--     Debug.crash "TODO"
