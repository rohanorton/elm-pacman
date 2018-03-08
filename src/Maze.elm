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


square : Square -> Svg a
square square =
    Debug.crash "TODO"


svg : Maze -> List (Svg a) -> Svg a
svg (Maze maze) children =
    let
        ( _, _, svgCells ) =
            maze
                |> Array.foldl svgRow ( 0, 0, [] )
    in
        Svg.svg
            [ SvgA.height "600"
            , SvgA.width "600"

            -- , SvgA.viewBox "0 0 900 900"
            ]
            (svgCells ++ children)


svgRow : Array Cell -> ( Int, Int, List (Svg msg) ) -> ( Int, Int, List (Svg msg) )
svgRow cells ( x, y, els ) =
    cells
        |> Array.foldr (\cell ( x_, y_, els_ ) -> ( x_ + 1, y_, svgSquare cell x_ y_ :: els_ )) ( 0, y + 1, els )


empty : Svg msg
empty =
    Svg.text ""


energizer : Svg msg
energizer =
    Svg.g [ SvgA.color "white" ]
        [ Svg.text "•"
        ]


pill : Svg msg
pill =
    Svg.g [ SvgA.color "white" ]
        [ Svg.text "·"
        ]


svgCellContent : Cell -> ( String, Svg msg )
svgCellContent cell =
    case cell of
        Empty ->
            ( "black", empty )

        Pill ->
            ( "black", pill )

        Wall ->
            ( "blue", empty )

        Energizer ->
            ( "black", energizer )

        Decision c ->
            svgCellContent c


svgSquare : Cell -> Int -> Int -> Svg msg
svgSquare cell x y =
    let
        ( colour, child ) =
            svgCellContent cell
    in
        Svg.svg
            [ SvgA.viewBox "0 0 200 200"
            , SvgA.preserveAspectRatio "xMinYMin meet"
            , SvgA.width "10"
            , SvgA.x <| toString <| x * 10
            , SvgA.y <| toString <| y * 10
            ]
            [ Svg.rect
                [ SvgA.style <| "fill:" ++ colour
                , SvgA.width "200"
                , SvgA.height "200"
                ]
                [ child ]
            ]



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
generateSvg (Maze maze) =
    Svg.text "ohj"
