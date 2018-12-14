module Day14 exposing (Board, get, init, repeatStep, res1, res2, step)

import Array exposing (Array)
import Browser
import Collage exposing (Collage)
import Collage.Events as Events
import Collage.Layout as Layout
import Collage.Render as Render
import Color
import Dict exposing (Dict)
import Html exposing (..)
import List.Extra as List
import Parser exposing ((|.), (|=), Parser, Step(..))
import Set exposing (Set)


type alias Board =
    { recipes : Array Int
    , first : Int
    , second : Int
    }


res1 n =
    repeatStep (n + 10) init
        |> .recipes
        |> Array.slice n (n + 10)
        |> Array.toList
        |> List.map String.fromInt
        |> String.concat


res2 =
    searchFirst 5 init


init : Board
init =
    { recipes = Array.fromList [ 3, 7 ]
    , first = 0
    , second = 1
    }


get : Int -> Array Int -> Int
get i a =
    Array.get i a |> Maybe.withDefault 0


step : Board -> Board
step board =
    let
        currentFirst =
            get board.first board.recipes

        currentSecond =
            get board.second board.recipes

        newRecipes =
            let
                sum =
                    currentFirst + currentSecond
            in
            if sum >= 10 then
                Array.push 1 board.recipes
                    |> Array.push (modBy 10 sum)

            else
                Array.push sum board.recipes

        newLength =
            Array.length newRecipes
    in
    { recipes = newRecipes
    , first = modBy newLength <| currentFirst + 1 + board.first
    , second = modBy newLength <| currentSecond + 1 + board.second
    }


repeatStep : Int -> Board -> Board
repeatStep maxSize board =
    if Array.length board.recipes < maxSize then
        repeatStep maxSize (step board)

    else
        board


toString : Array Int -> String
toString =
    Array.toList >> List.map String.fromInt >> String.concat


searchPattern : Int -> Array Int -> Maybe Int
searchPattern begin array =
    if begin + 6 >= Array.length array then
        Nothing

    else if [ 0, 7, 7, 2, 0, 1 ] == [ get begin array, get (begin + 1) array, get (begin + 2) array, get (begin + 3) array, get (begin + 4) array, get (begin + 5) array ] then
        Just begin

    else
        searchPattern (begin + 1) array


searchFirst : Int -> Board -> Int
searchFirst maxSize board =
    case searchPattern (maxSize // 2) board.recipes of
        Nothing ->
            searchFirst (2 * maxSize |> Debug.log "size") (repeatStep (2 * maxSize) board)

        Just i ->
            i
