module Day09 exposing (main, res1, res2)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import List.Extra as List
import Parser exposing ((|.), (|=), Parser, Step(..))
import Set exposing (Set)


main =
    Browser.sandbox
        { init = ()
        , view = \_ -> text <| res1
        , update = \_ () -> ()
        }


res1 =
    gameScores 465 7194000
        |> Dict.values
        |> List.maximum
        |> Maybe.withDefault 0
        |> String.fromInt


res2 =
    0


get : Int -> Array Int -> Int
get k d =
    Array.get k d |> Maybe.withDefault 0


type alias CyclicList =
    { lefts : Array Int, rights : Array Int, current : Int }


toList : CyclicList -> List Int
toList cl =
    let
        loop current =
            if get current cl.rights == 0 then
                [ current ]

            else
                current :: loop (get current cl.rights)
    in
    loop 0


initCyclicList : Int -> CyclicList
initCyclicList maxSize =
    { lefts =
        Array.repeat (maxSize + 1) 0
            |> Array.set 0 1
            |> Array.set 1 0
    , rights =
        Array.repeat (maxSize + 1) 0
            |> Array.set 0 1
            |> Array.set 1 0
    , current = 1
    }


moveLeft : CyclicList -> CyclicList
moveLeft cl =
    { cl | current = get cl.current cl.lefts }


moveRight : CyclicList -> CyclicList
moveRight cl =
    { cl | current = get cl.current cl.rights }


moveNLeft : Int -> CyclicList -> CyclicList
moveNLeft n =
    if n < 0 then
        identity

    else
        moveNLeft (n - 1) << moveLeft


moveNRight : Int -> CyclicList -> CyclicList
moveNRight n =
    if n < 0 then
        identity

    else
        moveNRight (n - 1) << moveRight


removeCurrent : CyclicList -> ( Int, CyclicList )
removeCurrent cl =
    let
        oldRight =
            get cl.current cl.rights

        oldLef =
            get cl.current cl.lefts
    in
    ( cl.current
    , { current = oldRight
      , rights =
            cl.rights
                |> Array.set oldLef oldRight
      , lefts =
            cl.lefts
                |> Array.set oldRight oldLef
      }
    )


insertRight : Int -> CyclicList -> CyclicList
insertRight new cl =
    let
        oldRight =
            get cl.current cl.rights
    in
    { current = new
    , rights =
        cl.rights
            |> Array.set cl.current new
            |> Array.set new oldRight
    , lefts =
        cl.lefts
            |> Array.set oldRight new
            |> Array.set new cl.current
    }


play : Int -> CyclicList -> ( Int, CyclicList )
play marble game =
    --let
    --    _ =
    --        Debug.log "game" (toList game)
    --    _ =
    --        Debug.log "marble" marble
    --in
    if modBy 23 marble == 0 then
        game
            |> moveNLeft 6
            |> removeCurrent
            |> Tuple.mapFirst (\s -> s + marble)

    else
        game
            |> moveRight
            |> insertRight marble
            |> Tuple.pair 0


incrementBy : Int -> Int -> Dict Int Int -> Dict Int Int
incrementBy value key d =
    Dict.insert key ((Dict.get key d |> Maybe.withDefault 0) + value) d


gameScores : Int -> Int -> Dict Int Int
gameScores nbPlayers nbMarbles =
    let
        loop marble ( scores, game ) =
            if marble > nbMarbles then
                scores

            else
                let
                    ( score, newGame ) =
                        play marble game

                    newScores =
                        incrementBy score (modBy nbPlayers marble) scores
                in
                loop (marble + 1) ( newScores, newGame )
    in
    loop 2 ( Dict.empty, initCyclicList nbMarbles )
