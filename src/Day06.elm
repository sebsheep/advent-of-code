module Day06 exposing (Distance, Point, PointId, Zones, cartesianProduct, computedZones, getInfiniteZones, input, inputDict, inputSet, main, manhattan, max, min, res1, res2, view, xmax, xmin, xrange, ymax, ymin, yrange)

import Browser
import Dict exposing (Dict)
import Dict.Extra as Dict
import Hex
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra as List
import Maybe.Extra as Maybe
import Set exposing (Set)


main =
    Browser.sandbox
        { init = computedZones
        , view = view
        , update = \_ m -> m
        }


view : Zones -> Html msg
view zones =
    let
        rows =
            List.map
                (\y ->
                    tr []
                        (List.map
                            (\x -> cell x y)
                            (List.range xmin xmax)
                        )
                )
                (List.range ymin ymax)

        cell x y =
            td
                [ style "width" "2px"
                , style "height" "2px"
                , style "background-color"
                    (case Dict.get ( x, y ) zones of
                        Just (Just ( xc, yc )) ->
                            "#10" ++ Hex.toString (modBy 256 (xc * 31)) ++ Hex.toString (modBy 256 (yc * 31))

                        Just Nothing ->
                            "red"

                        Nothing ->
                            "black"
                    )
                ]
                []
    in
    table [] rows


type alias Point =
    ( Int, Int )


max : List Int -> Int
max =
    List.maximum >> Maybe.withDefault 0


min : List Int -> Int
min =
    List.minimum >> Maybe.withDefault 0


xmax =
    List.map Tuple.first input |> max


ymax =
    List.map Tuple.second input |> max


xmin =
    List.map Tuple.first input |> min


ymin =
    List.map Tuple.second input |> min


res1 =
    let
        infiniteZones =
            getInfiniteZones computedZones
    in
    computedZones
        |> Dict.values
        |> List.filterMap
            (\v ->
                case v of
                    Just p ->
                        if Set.member p infiniteZones then
                            Nothing

                        else
                            Just p

                    Nothing ->
                        Nothing
            )
        |> Dict.frequencies
        |> Dict.values
        |> List.maximum



--|> List.filter (\coords -> not <| Set.member coords inputSet)
--|> List.filterMap (nearest 1)
--|> Dict.frequencies
--|> Dict.values
--|> List.maximum


type alias Zones =
    Dict Point (Maybe Point)


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


manhattan : Point -> Point -> Int
manhattan ( x1, y1 ) ( x2, y2 ) =
    abs (x1 - x2) + abs (y1 - y2)


computedZones =
    cartesianProduct xrange yrange
        |> List.foldr
            (\coords acc ->
                let
                    distances =
                        List.map
                            (\target -> ( manhattan coords target, target ))
                            input
                            |> List.sortBy Tuple.first
                in
                Dict.insert coords
                    (case distances of
                        [] ->
                            Nothing

                        ( 0, target ) :: _ ->
                            Nothing

                        [ ( _, point ) ] ->
                            Just point

                        ( d1, point1 ) :: ( d2, _ ) :: _ ->
                            if d1 == d2 then
                                Nothing

                            else
                                Just point1
                    )
                    acc
            )
            Dict.empty


res2 =
    cartesianProduct xrange yrange
        |> List.map
            (\coords ->
                List.map (manhattan coords) input
                    |> List.sum
            )
        |> List.filter (\d -> d < 32)
        |> List.length


xrange =
    List.range xmin xmax


yrange =
    List.range ymin ymax


getInfiniteZones : Zones -> Set Point
getInfiniteZones zones =
    let
        border =
            List.map (\x -> ( x, ymin )) xrange
                ++ List.map (\x -> ( x, ymax )) xrange
                ++ List.map (\y -> ( xmin, y )) yrange
                ++ List.map (\y -> ( xmax, y )) yrange
    in
    List.foldr
        (\point set ->
            case Dict.get point zones of
                Just (Just p) ->
                    Set.insert p set

                _ ->
                    set
        )
        Set.empty
        border


type alias PointId =
    Int


type alias Distance =
    Int


inputSet : Set Point
inputSet =
    Set.fromList input


inputDict =
    Dict.fromList
        (List.map2
            Tuple.pair
            (List.range 0 (List.length input))
            input
        )


input =
    [ ( 1, 1 )
    , ( 1, 6 )
    , ( 8, 3 )
    , ( 3, 4 )
    , ( 5, 5 )
    , ( 8, 9 )
    ]



--input =
--    [ ( 355, 246 )
--    , ( 259, 215 )
--    , ( 166, 247 )
--    , ( 280, 341 )
--    , ( 54, 91 )
--    , ( 314, 209 )
--    , ( 256, 272 )
--    , ( 149, 313 )
--    , ( 217, 274 )
--    , ( 299, 144 )
--    , ( 355, 73 )
--    , ( 70, 101 )
--    , ( 266, 327 )
--    , ( 51, 228 )
--    , ( 274, 123 )
--    , ( 342, 232 )
--    , ( 97, 100 )
--    , ( 58, 157 )
--    , ( 130, 185 )
--    , ( 135, 322 )
--    , ( 306, 165 )
--    , ( 335, 84 )
--    , ( 268, 234 )
--    , ( 173, 255 )
--    , ( 316, 75 )
--    , ( 79, 196 )
--    , ( 152, 71 )
--    , ( 205, 261 )
--    , ( 275, 342 )
--    , ( 164, 95 )
--    , ( 343, 147 )
--    , ( 83, 268 )
--    , ( 74, 175 )
--    , ( 225, 130 )
--    , ( 354, 278 )
--    , ( 123, 206 )
--    , ( 166, 166 )
--    , ( 155, 176 )
--    , ( 282, 238 )
--    , ( 107, 295 )
--    , ( 82, 92 )
--    , ( 325, 299 )
--    , ( 87, 287 )
--    , ( 90, 246 )
--    , ( 159, 174 )
--    , ( 295, 298 )
--    , ( 260, 120 )
--    , ( 203, 160 )
--    , ( 72, 197 )
--    , ( 182, 296 )
--    ]
