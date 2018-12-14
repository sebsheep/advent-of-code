module Day11 exposing (Point, makeGrid, powerCell, res1, res2, testPowerCell, testsRes1)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import List.Extra as List
import Parser exposing ((|.), (|=), Parser, Step(..))
import Set exposing (Set)


type alias Point =
    ( Int, Int )


type Matrix a
    = Dict Point a


powerCell : Int -> Point -> Int
powerCell serialNumber ( x, y ) =
    let
        rackId =
            x + 10
    in
    (modBy 10 <| ((rackId * y + serialNumber) * rackId) // 100) - 5


testPowerCell =
    [ powerCell 8 ( 3, 5 ) == 4
    , powerCell 57 ( 122, 79 ) == -5
    , powerCell 71 ( 101, 153 ) == 4
    ]


width =
    300


height =
    300


cartesianProduct : List a -> List b -> List ( a, b )
cartesianProduct l1 l2 =
    List.map
        (\e1 ->
            List.map
                (\e2 ->
                    ( e1, e2 )
                )
                l2
        )
        l1
        |> List.concat


makeGrid : Int -> Dict Point Int
makeGrid serialNumber =
    cartesianProduct (List.range 0 (width - 1)) (List.range 0 (height - 1))
        |> List.map (\coords -> ( coords, powerCell serialNumber coords ))
        |> Dict.fromList



--|> (\m -> Debug.log (showMatrix m) m)


to3digits : Int -> String
to3digits i =
    let
        s =
            String.fromInt i
    in
    case String.length s of
        1 ->
            "  " ++ s

        2 ->
            " " ++ s

        3 ->
            s

        _ ->
            "###"


showMatrix : Dict Point Int -> String
showMatrix mat =
    List.foldl
        (\i buffer ->
            List.foldl
                (\j line ->
                    get ( j, i ) mat
                        |> to3digits
                        |> (\s -> line ++ s ++ " ")
                )
                (buffer ++ "\n")
                (List.range 0 (width - 1))
        )
        ""
        (List.range 0 (height - 1))


get : Point -> Dict Point Int -> Int
get coords m =
    Dict.get coords m |> Maybe.withDefault 0


type alias Res =
    { coords : Point, value : Int, size : Int }


sumSquare : Int -> Dict Point Int -> List ( Point, Int )
sumSquare size grid =
    cartesianProduct (List.range 0 (width - size - 1)) (List.range 0 (height - size - 1))
        |> List.map
            (\(( x, y ) as coords) ->
                List.range 0 ((size ^ 2) - 1)
                    |> List.map (\i -> get ( x + (i // size), y + modBy size i ) grid)
                    |> List.sum
                    |> (\sum -> ( coords, sum ))
            )


findSquareMax : Int -> Dict Point Int -> ( Point, Int )
findSquareMax size grid =
    let
        summed =
            sumSquare size grid

        --|> (\m -> Debug.log (showMatrix m) m)
    in
    summed
        |> List.minimumBy Tuple.second
        |> Maybe.withDefault ( ( 0, 0 ), 0 )


updateRes : List ( Point, Int ) -> Int -> Res -> Res
updateRes mat size res =
    let
        ( coords, value ) =
            List.minimumBy Tuple.second mat
                |> Maybe.withDefault ( ( 0, 0 ), 0 )
    in
    if value < res.value then
        { coords = coords, size = size, value = value }

    else
        res


emptyRes =
    Res ( 0, 0 ) 0 0


findSquareRec : Int -> Dict Point Int -> ( Res, Dict Point Int, Dict Point Int )
findSquareRec size grid =
    if size == 2 then
        let
            grid2 =
                sumSquare size grid
        in
        ( updateRes (Dict.toList grid) 1 emptyRes |> updateRes grid2 2
        , grid
        , grid2 |> Dict.fromList
        )

    else
        let
            ( resPrev, gridPrev2, gridPrev ) =
                findSquareRec (size - 1) grid

            newGrid =
                let
                    _ =
                        Debug.log "size" size
                in
                cartesianProduct (List.range 0 (width - size)) (List.range 0 (height - size))
                    |> List.map
                        (\(( x, y ) as coords) ->
                            ( coords
                            , get coords gridPrev
                                + get ( x + 1, y + 1 ) gridPrev
                                + get ( x + size - 1, y ) grid
                                + get ( x, y + size - 1 ) grid
                                - get ( x + 1, y + 1 ) gridPrev2
                            )
                        )
        in
        ( updateRes newGrid size resPrev, gridPrev, Dict.fromList newGrid )


res2 serialNumber =
    let
        grid =
            makeGrid serialNumber
    in
    findSquareRec 300 grid


res1 serialNumber =
    findSquareMax 3 <| makeGrid serialNumber


testsRes1 =
    [ res1 18 == ( ( 33, 45 ), 29 )
    , res1 42 == ( ( 21, 61 ), 30 )
    ]
