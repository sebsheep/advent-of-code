module Day12 exposing (Zipper5, axiomTest, first, fromList, next, parser, res1, res2, ruleParser, rulesTest, step, test, toBool, toList, toPlants, toString)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import List.Extra as List
import Parser exposing ((|.), (|=), Parser, Step(..))
import Set exposing (Set)


res1 n =
    repeatStep n rules_ (axiom_ n)
        |> List.indexedMap (\i b -> ( i - n - 2, b ))
        |> List.filterMap
            (\( i, b ) ->
                if b then
                    Just i

                else
                    Nothing
            )
        |> List.sum


res2 =
    -- shifting pattern appears after a number of iterations...
    repeatStep 200 rules_ (axiom_ 200)
        |> List.indexedMap (\i b -> ( i - 200 - 2, b ))
        |> List.filterMap
            (\( i, b ) ->
                if b then
                    Just <| i + 50000000000 - 200

                else
                    Nothing
            )
        |> List.sum


toBool : Char -> Bool
toBool c =
    case c of
        '#' ->
            True

        '.' ->
            False

        _ ->
            False


ruleParser : String -> Maybe (List Bool)
ruleParser rule =
    let
        splitted =
            String.split " => " rule
    in
    case splitted of
        [ key, value ] ->
            if value == "#" then
                key |> String.toList |> List.map toBool |> Just

            else
                Nothing

        _ ->
            Nothing


parser : String -> List (List Bool)
parser rules =
    String.lines rules
        |> List.filterMap ruleParser


type alias Zipper5 a =
    { previous : List a
    , current : List a
    , next : List a
    }


fromList : List a -> Zipper5 a
fromList l =
    case l of
        a :: b :: c :: d :: e :: xs ->
            { previous = []
            , current = [ a, b, c, d, e ]
            , next = xs
            }

        _ ->
            { previous = []
            , current = []
            , next = []
            }


toList : Zipper5 a -> List a
toList z =
    List.reverse z.previous ++ z.current ++ z.next


next : Zipper5 a -> Maybe (Zipper5 a)
next z =
    case z.next of
        [] ->
            Nothing

        x :: xs ->
            case z.current of
                a :: b :: c :: d :: e :: [] ->
                    Just
                        { previous = a :: z.previous
                        , current = [ b, c, d, e, x ]
                        , next = xs
                        }

                _ ->
                    -- should not happen
                    Nothing


first : Zipper5 a -> Zipper5 a
first z =
    case toList z of
        a :: b :: c :: d :: e :: xs ->
            { previous = []
            , current = [ a, b, c, d, e ]
            , next = xs
            }

        _ ->
            --  should not happen...
            z


toPlants : Int -> String -> List Bool
toPlants n axiom =
    let
        axiomList =
            axiom
                |> String.toList
                |> List.map toBool

        ends =
            List.repeat (n + 2) False
    in
    ends ++ axiomList ++ ends


step : List (List Bool) -> List Bool -> List Bool
step rules plants =
    let
        loop plantZipper previousPlants =
            let
                newPlant =
                    List.member plantZipper.current rules
            in
            case next plantZipper of
                Just z ->
                    loop z (newPlant :: previousPlants)

                Nothing ->
                    newPlant :: previousPlants
    in
    [ False, False ] ++ List.reverse (loop (fromList plants) []) ++ [ False, False ]


repeatStep : Int -> List (List Bool) -> List Bool -> List Bool
repeatStep n rules plants =
    let
        _ =
            Debug.log "a" (toString <| List.drop 110 plants)
    in
    if n == 0 then
        plants

    else
        repeatStep (n - 1) rules (step rules plants)


toString : List Bool -> String
toString =
    List.map
        (\b ->
            if b then
                '#'

            else
                '.'
        )
        >> String.fromList


test =
    repeatStep 20 rulesTest axiomTest
        |> List.indexedMap (\i b -> ( i - 22, b ))
        |> List.filterMap
            (\( i, b ) ->
                if b then
                    Just i

                else
                    Nothing
            )
        |> List.sum


axiomTest =
    toPlants 20 "#..#.#..##......###...###"


rulesTest =
    parser """...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #"""


axiom_ n =
    toPlants n ".##..#.#..##..##..##...#####.#.....#..#..##.###.#.####......#.......#..###.#.#.##.#.#.###...##.###.#"


rules_ =
    parser """.##.# => #
##.#. => #
##... => #
#.... => .
.#..# => .
#.##. => .
.##.. => .
.#.## => .
###.. => .
..##. => #
##### => #
#...# => #
.#... => #
###.# => #
#.### => #
##..# => .
.###. => #
...## => .
..#.# => .
##.## => #
....# => .
#.#.# => #
#.#.. => .
.#### => .
...#. => #
..### => .
..#.. => #
..... => .
####. => .
#..## => #
.#.#. => .
#..#. => #"""
