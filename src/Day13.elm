module Day13 exposing (Cart, Decision(..), Direction(..), Point, Tile(..), initCart, next, parseTrack, res1, res2)

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


res1 =
    parseTrack input
        |> tickUntil


res2 =
    parseTrack testInput2
        |> tickUntil2


testInput2 =
    """/>><\\
|   |
| /<+-\\
| | | v  
\\>+</ |
  |   ^
  \\<->/"""


testInput =
    """/->-\\        
|   |  /----\\
| /-+--+-\\  |
| | |  | v  |
\\-+-/  \\-+--/
  \\------/   
"""


type alias Point =
    ( Int, Int )


type Tile
    = Vertical
    | Horizontal
    | TurnLeft
    | TurnRight
    | Cross


type Decision
    = GoLeft
    | GoStraight
    | GoRight


type Direction
    = Left
    | Right
    | Top
    | Bottom


type alias Track =
    Dict Point Tile


toInt : Direction -> Int
toInt d =
    case d of
        Left ->
            0

        Top ->
            1

        Right ->
            2

        Bottom ->
            3


toDirection : Int -> Direction
toDirection i =
    case i of
        0 ->
            Left

        1 ->
            Top

        2 ->
            Right

        3 ->
            Bottom

        _ ->
            Left


type alias Cart =
    { direction : Direction
    , lastDecision : Decision
    }


initCart : Direction -> Cart
initCart direction =
    Cart direction GoRight


next : Decision -> Decision
next d =
    case d of
        GoLeft ->
            GoStraight

        GoStraight ->
            GoRight

        GoRight ->
            GoLeft


newMove : Decision -> Direction -> Direction
newMove dec dir =
    case dec of
        GoLeft ->
            toDirection <| modBy 4 (toInt dir - 1)

        GoStraight ->
            dir

        GoRight ->
            toDirection <| modBy 4 (toInt dir + 1)



-- in my input, the carts are never on "+", "/" or "\\", I can so safely
-- know which direction is the track


type alias State =
    ( Track, Dict Point Cart )


parseTrack : String -> State
parseTrack inputStr =
    String.lines inputStr
        |> List.indexedMap
            (\i line ->
                List.indexedMap (\j char -> ( ( j, i ), char )) (String.toList line)
            )
        |> List.concat
        |> List.foldr
            (\( coords, char ) ( track, carts ) ->
                case char of
                    '|' ->
                        ( Dict.insert coords Vertical track, carts )

                    '-' ->
                        ( Dict.insert coords Horizontal track, carts )

                    '/' ->
                        ( Dict.insert coords TurnRight track, carts )

                    '\\' ->
                        ( Dict.insert coords TurnLeft track, carts )

                    '+' ->
                        ( Dict.insert coords Cross track, carts )

                    '<' ->
                        ( Dict.insert coords Horizontal track, Dict.insert coords (initCart Left) carts )

                    '>' ->
                        ( Dict.insert coords Horizontal track, Dict.insert coords (initCart Right) carts )

                    '^' ->
                        ( Dict.insert coords Vertical track, Dict.insert coords (initCart Top) carts )

                    'v' ->
                        ( Dict.insert coords Vertical track, Dict.insert coords (initCart Bottom) carts )

                    _ ->
                        ( track, carts )
            )
            ( Dict.empty, Dict.empty )



--        |> Debug.log "track"


insert : Point -> Cart -> State -> Result Point (Dict Point Cart)
insert ( x, y ) cart ( track, carts ) =
    let
        newCoords =
            case cart.direction of
                Left ->
                    ( x - 1, y )

                Right ->
                    ( x + 1, y )

                Top ->
                    ( x, y - 1 )

                Bottom ->
                    ( x, y + 1 )
    in
    case Dict.get newCoords carts of
        Just _ ->
            -- collision detected
            Err newCoords

        Nothing ->
            -- no body is here
            case Dict.get newCoords track of
                Nothing ->
                    Debug.todo
                        ("coords outsite the track : "
                            ++ Debug.toString ( x, y )
                            ++ Debug.toString (Dict.get ( x, y ) track)
                            ++ " new coords: "
                            ++ Debug.toString newCoords
                        )
                        Nothing

                Just tile ->
                    let
                        newCart =
                            case tile of
                                Cross ->
                                    let
                                        newDec =
                                            next cart.lastDecision
                                    in
                                    { direction = newMove newDec cart.direction
                                    , lastDecision = newDec
                                    }

                                TurnLeft ->
                                    { cart
                                        | direction =
                                            case cart.direction of
                                                Top ->
                                                    Left

                                                Bottom ->
                                                    Right

                                                Right ->
                                                    Bottom

                                                Left ->
                                                    Top
                                    }

                                TurnRight ->
                                    { cart
                                        | direction =
                                            case cart.direction of
                                                Top ->
                                                    Right

                                                Bottom ->
                                                    Left

                                                Right ->
                                                    Top

                                                Left ->
                                                    Bottom
                                    }

                                _ ->
                                    cart
                    in
                    Ok <| Dict.insert newCoords newCart carts


insertUntil : List ( Point, Cart ) -> State -> Result Point (Dict Point Cart)
insertUntil coordsCarts ( track, carts ) =
    case coordsCarts of
        ( coords, cart ) :: rest ->
            case insert coords cart ( track, Dict.remove coords carts ) of
                Ok newCarts ->
                    insertUntil rest ( track, newCarts )

                Err p ->
                    Err p

        [] ->
            Ok carts


tick : State -> Result Point (Dict Point Cart)
tick (( track, carts ) as state) =
    Dict.toList carts
        |> (\coordsCart -> insertUntil coordsCart state)


tickUntil : State -> Result Point (Dict Point Cart)
tickUntil (( track, carts ) as state) =
    case tick state of
        Ok newCarts ->
            tickUntil ( track, newCarts )

        Err p ->
            Err p



---------------------------


updateDirections : Tile -> Cart -> Cart
updateDirections tile cart =
    case tile of
        Cross ->
            let
                newDec =
                    next cart.lastDecision
            in
            { direction = newMove newDec cart.direction
            , lastDecision = newDec
            }

        TurnLeft ->
            { cart
                | direction =
                    case cart.direction of
                        Top ->
                            Left

                        Bottom ->
                            Right

                        Right ->
                            Bottom

                        Left ->
                            Top
            }

        TurnRight ->
            { cart
                | direction =
                    case cart.direction of
                        Top ->
                            Right

                        Bottom ->
                            Left

                        Right ->
                            Top

                        Left ->
                            Bottom
            }

        _ ->
            cart


insert2 : Point -> Cart -> State -> ( Maybe Point, Dict Point Cart )
insert2 ( x, y ) cart ( track, carts ) =
    let
        newCoords =
            case cart.direction of
                Left ->
                    ( x - 1, y )

                Right ->
                    ( x + 1, y )

                Top ->
                    ( x, y - 1 )

                Bottom ->
                    ( x, y + 1 )
    in
    case Dict.get newCoords carts of
        Just _ ->
            -- collision detected
            ( Just newCoords, Dict.remove newCoords carts )

        Nothing ->
            -- no body is here
            case Dict.get newCoords track of
                Nothing ->
                    Debug.todo
                        ("coords outsite the track : "
                            ++ Debug.toString ( x, y )
                            ++ Debug.toString (Dict.get ( x, y ) track)
                            ++ " new coords: "
                            ++ Debug.toString newCoords
                        )
                        (Err ( 0, 0 ))

                Just tile ->
                    ( Nothing, Dict.insert newCoords (updateDirections tile cart) carts )


insertUntil2 : List ( Point, Cart ) -> List Point -> State -> Dict Point Cart
insertUntil2 coordsCarts removedList ( track, carts ) =
    case coordsCarts of
        ( coords, cart ) :: rest ->
            if List.member coords removedList then
                --  the current cart is already removed..
                insertUntil2 rest removedList ( track, carts )

            else
                let
                    ( removedMaybe, newCarts ) =
                        insert2 coords cart ( track, Dict.remove coords carts )
                in
                case removedMaybe of
                    Nothing ->
                        insertUntil2 rest removedList ( track, newCarts )

                    Just point ->
                        insertUntil2 rest (point :: removedList) ( track, newCarts )

        [] ->
            carts


tick2 : State -> Dict Point Cart
tick2 (( track, carts ) as state) =
    Dict.toList carts
        |> List.sortBy (\( ( x, y ), _ ) -> ( y, x ))
        |> (\coordsCart -> insertUntil2 coordsCart [] state)


tickUntil2 : State -> Dict Point Cart
tickUntil2 (( track, carts ) as state) =
    let
        newCarts =
            tick2 state

        _ =
            Debug.log "nb carts:" <| Dict.size newCarts
    in
    if Dict.size newCarts == 1 then
        newCarts

    else
        tickUntil2 ( track, newCarts )



--view : Track ->Dict Point Cart -> Html msg
--view track carts =


input =
    """                    /-----------------------------------------------------------------------------------------------------\\                           
                    |/---------------------\\                                                                              |                           
                    ||                   /-+-----------------------------------------------\\     /----------------------\\ |            /-------------\\
                    ||                   | |                    /--------------------------+-----+-----------------\\    | |            |             |
                    ||            /------+-+--------------------+--------------------------+-----+-----------------+----+-+------------+---\\         |
                    ||           /+------+-+--------------------+--------------------------+-----+-----------------+----+-+---\\        |   |         |
                    ||          /++------+-+--------------------+--------------------------+-----+-----------------+----+-+--\\|        |   |         |
                   /++----------+++------+-+--------------------+-------\\                  |     |                 ^    | |  ||        |   |         |
                   |||          |||      |/+--------\\           |  /----+----------------\\ |     |                 |    | |  ||        |   |         |
            /------+++----------+++------+++--------+-----------+--+----+----------------+-+-----+----\\            |   /+-+--++--\\     |   |         |
            |      |||          |||      |||   /----+-----------+--+----+----------------+-+-----+----+------------+---++-+--++--+--\\  |   |         |
            |      |||          |||      |||/--+----+-----------+--+----+----------------+-+-----+----+------------+---++-+--++--+--+\\ |   |         |
            |      |||        /-+++------++++--+----+-----------+--+----+-------------\\  | |     |    |            |/--++-+--++--+--++\\\\---+---------/
            |      |||        | |||      ||v|  |    |           |  |    |             |  | |     |    |          /-++--++-+--++--+--+++---\\|          
            |      |||        | |||      ||||  |   /+-----------+--+----+-------------+--+-+-----+----+----------+-++-\\|| |  ||  |  |||   ||          
            |      |||        | |||      ||||  |   ||           |  |    |             |  | |     |    |          | || ||| |  ||  |  |||   ||          
            |      |||        | |||      ||||/-+---++---\\   /---+--+----+-------------+--+-+-----+----+------\\   | || ||| |  ||  |  |||   ||          
            |      |||        | |||      ||||| |   ||   |   |   |  |    |             |  | |     |    |      |  /+-++-+++-+--++--+--+++--\\||          
  /---------+------+++--------+-+++------+++++-+---++---+---+---+--+----+-------------+\\ | |     |    |      |  || || ||| |  ||  |  |||  |||          
  |         |      |||/-------+-+++------+++++-+---++---+---+---+--+----+-------------++-+-+-----+----+------+--++-++\\||| |  ||  |  |||  |||          
  |         |      |||^       | ||\\------+++++-+---++---+---+---+--+----+-------------++-+-+-----+----+------+--++-++++++-+--++--+--+++--++/          
  |         |      |||| /-----+-++----->-+++++-+---++---+---+---+--+----+----------\\  || | |     |    |   /--+--++-++++++-+--++--+--+++--++----------\\
  | /-------+------++++-+-----+-++-------+++++-+---++---+-\\ |   |  |    |          |  || | |/----+----+---+--+--++-++++++-+--++--+-\\|||  ||          |
  | |       |      |||| |     | ||   /---+++++-+---++---+-+-+---+--+----+----------+--++-+-++----+----+-\\ |  |  || |||||| |  ||  | ||||  ||          |
  | |       |      |||| |     | ||   |   ||||| |   ||   | | |   |  |    |          |  || | ||   /+----+-+-+--+--++-++++++-+--++--+-++++--++-\\        |
/-+-+-------+------++++-+-----+-++---+---+++++-+---++---+-+-+---+--+----+----------+--++-+\\||   ||    | | |  |  || |||||| |  ||  | ||||  || |        |
| | |       |      |||| |     | || /-+---+++++-+---++---+-+\\|   |  |    |          |  || ||||   ||    | | |  |  || |||||| |  ||  | ||||  || |        |
| | |       |      |||| |     | || | |   ||||| |   ||   | ||| /-+--+----+----------+--++-++++---++----+-+-+--+--++-++++++-+--++--+-++++--++-+------\\ |
| | |       |      |||| |     | || | |   \\++++-+---++---+-+++-+-+--+----+----------+--++-++/|   |\\----+-+-+--+--++-+++++/ |  ||  | ||||  || |      | |
| | |       |      ||||/+-----+-++-+-+----++++-+---++---+-+++-+-+--+----+----------+--++-++-+---+-----+-+-+--+--++-+++++-\\|  ||  | ||||  || |      | |
| | |       |      ||||||     | || | |    |||| |   ||   | ||| | |  |    |          |  || || |   |     | | |  |  || |\\+++-++--++--+-+++/  || |      | |
| | |       |      ||||||     | || | |    |||| |   ||   | ||| | \\--+----+----------+--++-++-+---+-----+-+-+--+--++-/ ||| ||  ||  | |||   || |      | |
| | |       |      ||||||     \\-++>+-+----++++-+---++---+-+++-+----+----+----------+--/| || |   |     | | |  |  ||   ||| ||  ||  | |||   || |      | |
| | |       |      ||||||       || | |    |||| |   ||   | ||| |    | /--+----------+---+-++-+---+-----+-+-+--+--++---+++-++--++--+-+++\\  || |      | |
| | |  /----+------++++++-------++-+-+----++++-+---++-\\ | ||| |    | |  | /--------+---+-++-+---+-----+-+-+--+--++-\\ ||\\-++--++--/ ||||  || |      | |
| | |  |    |   /--++++++-------++-+-+----++++\\|   || | | ||| |    | |  | |        |   | || |   |     | | |  |  || | ||  ||  ||    ||||  || |      | |
| | |  |    |   |  ||||||       || | |    ||||||   || | | ||| |    | |  | |        |   | || |   |     | | |  |  || | ||  ||  ||    ||||  || |      | |
| | |  |    |   |  ||||||/------++-+-+----++++++---++-+-+-+++-+----+-+--+-+--------+---+-++-+---+-\\   | | |  |  || | ||  ||  ||    ||||  || |      | |
| | |  |   /+---+--+++++++---\\  || | | /--++++++---++-+-+\\||| |    \\-+--+-+--------+---+-/| |   | |   | | |  |  || | ||  ||  ||    ||||  || |      | |
| |/+--+---++---+\\ |||||||   | /++-+-+-+--++++++---++-+-+++++-+------+--+-+--------+---+--+-+---+-+---+-+-+--+--++-+-++--++--++--\\ ||||  || |      | |
| |||  |   ||   || |||||||   | ||| | | |/-++++++---++-+-+++++-+------+--+-+--------+---+--+-+---+-+---+-+-+--+\\ || | ||  ||  ||  | ||||  || |      | |
| |||  |  /++---++-+++++++\\  | ||| | | || ||||||   || | ||||| |      |  | |        |   |  | |   | |   | | |  || || | ||/-++--++--+\\||||  || |      | |
| |||  |  |||   || ||||||||  | ||| | | || ||||||   || | ||||| |      |  | |        |   |  | |   | |   | | |  || || | ||| ||  ||  ||||||  || |      | |
| |||  |  |||   || ||||||||  | ||| | | || ||||||   || | ||||| |/-----+--+-+-----\\  |   |  | |   | |   | | |  || || | |||/++--++--++++++--++-+\\     | |
| |||  |  ||| /-++-++++++++--+-+++-+-+-++-++++++---++-+\\||||| \\+-----+--+-+-----+--+---+->+-+---+-+---+-+-+--++-++-+-++++++--++--++++++--++-++-----/ |
| |||  |  ||| | || ||||||||  | ||| | | || ||||||   || |||||||  |    /+--+-+-----+--+---+--+-+---+-+\\  | |/+--++-++-+-++++++--++--++++++\\ || ||       |
| |||  |  ||| | || ||||||||  | |\\+-+-+-++-++++++---++-+++++++--+----++--+-+-----+--+---+--+-+---+-++--+-+++--++-++-+-++++++--/|  ||||||| || ||       |
| |||  |  ||| | || ||||||||  | | \\-+-+-++-++++++---++-+++++++--+----++--+-+-----+--+---+--+-+---+-++--+-+++--++-++-+-++++++---/  ||||||| || ||       |
| |||  |  ||| | || ||||||||  | |   | | \\+-++++++---++-+++/|||  |    ||  | |     |  |   |  | |/--+-++--+-+++--++-++-+-++++++------+++++++\\|| ||       |
| |||  |  ||| | || ||||||||  | \\---+-+--+-++++++---++-+++-+++--+----++--+-+-----+--+---+--+-++--+-++--+-+++--++-++-+-++++++------/||||||||| ||       |
| ||\\--+--+++-+-++-++++++++--+-----+-+--+-++++++---++-+++-/||  |    ||  |/+-----+--+---+\\ | ||  | ||  | |||  || || | ||||||       ||||||||| ||       |
| ||   |  ||| | || ||||||||  |     | |  | \\+++++---+/ |||  ||  |    |\\--+++-----+--+---++-+-++--+-++--+-+++--++-++-+-++++++-------++++/|||| ||       |
| ||   |  ||| | || ||||||||  |     | |/-+--+++++\\  |  |||  ||  |    |   |||     |  |   || | ||  | ||  | |||  || || | ||||||       |||| |||| ||       |
| ||   |  ||| | || \\+++++++--+-----+-++-+--++++++--+--+++--++--+----+---/||     |  |   || | ||/-+-++--+-+++--++-++-+-++++++-------++++-++++-++-----\\ |
| ||   \\--+++-+-++--+++++++--+-----+-++-+--++++++--+--/||  ||  |    |    ||     |  |   || | ||| | ||  | |||  || || | ||||||       |||| |||| ||     | |
| ||      ||| | ||  |||||||  |    /+-++-+--++++++--+---++--++--+----+----++-----+--+---++-+-+++-+-++--+-+++--++-++-+-++++++-------++++-++++-++\\    | |
| ||      ||| | ^|  |||\\+++--+----++-++-+--++++++--+---++--++--+----+----++-----+--+---++-+-+++-+-++--+-+++--++-++-+-++++/|       |||| |||| |||    | |
| ||   /--+++-+-++--+++-+++--+----++-++-+--++++++--+---++--++--+----+----++-----+--+---++-+\\||| | ||  | |||  || || | |||| |       |||| |||| |||    | |
| ||   |  \\++-+-++--+++-++/  |    ||/++-+--++++++--+---++--++--+----+----++-\\ /-+--+---++-+++++-+-++--+-+++--++-++-+-++++-+-------++++-++++-+++---\\| |
| ||  /+---++-+-++--+++-++---+----+++++-+--++++++--+---++--++--+---\\|    ||/+-+-+--+---++-+++++-+-++--+-+++--++-++-+-++++-+\\      |||| |||| |||   || |
| ||  ||   || | ||  ||| ||   |/---+++++-+--++++++--+-\\ ||  ||  |   ||    |||| | |  |   || ||||| | ||  | |||  || || | |||| ||      |||| |||| |||   || |
| ||  ||   || | ||  ||| ||   ||   ||||| |  ||||||  | | ||  ||  |   ||    |||| | |  |   || ||||| | ||  | |||  || \\+-+-++++-++------++++-++/| |||   || |
| ||  ||   || | ||  \\++-++---++---+++++-+--++++++--+-+-++--++--+---++----++++-+-+--+---++-+++++-+-++--+-+++--++--+-+-++++-/|      |||| || | |||   || |
| ||  || /-++-+-++---++-++---++---+++++-+--++++++--+-+-++->++--+---++----++++-+-+--+---++-+++++-+-++--+-+++--++\\ | | ||||  |      |||| || | |||   || |
| ||  || | || | ||   || ||   ||   ||||| |  ||||||/-+-+-++--++\\ |  /++----++++-+-+--+---++-+++++-+-++--+-+++--+++-+-+-++++--+---\\  |||| || | |||   || |
| ||  || | || | ||   || ||   ||   ||||| |  ||||||| | | ||  ||| |  |||    |||| | |  |   || ||||| | ||  | |||  ||| | | ||||  |   |  |||| || | |||   || |
|/++--++-+-++-+-++---++-++---++-\\ ||||| |  ||||||| | | ||  ||| |  |||    |||| | |  |   || ||||| | ||  | |||  ||| | | ||||  |   |  |||| || | |||   || |
||||  || | || | ||   || ||   || | ||||| |/-+++++++-+-+-++--+++-+--+++----++++-+-+--+---++-+++++-+-++--+-+++--+++-+-+-++++--+---+--++++-++-+-+++--\\|| |
||||  || | || | ||   || ||   || | ||||| || ||||||| | |/++--+++-+--+++----++++\\| |  |   || ||||| | ||  | |||  ||| | | ||||  |   |  |||| || | |||  ||| |
||||  || | || | ||   || ||   || | ||\\++-++-+++++++-+-++++--+++-+--+++----+++/|| |  |   || ||||| | ||  | |||  ||| | | ||||  |   |  |||| || | |||  ||| |
||||  || | || | ||   || ||   || | || || || ||||||| | ||||  ||| |  |||    \\++-++-+--+---+/ ||||| | ||  | |||  ||| | | ||||  |   |  |||| || | |||  ||| |
||||  || | || | ||   || ||   || | || || || ||||||| | ||||  ||| |  |||     || || |  |   |  ||||| | ||/-+-+++--+++-+-+-++++--+---+--++++-++\\| |||  ||| |
||||  || | || | ||   || ||   ||/+-++-++-++-+++++++\\| ||||  ||| |  |||     || || |  |   |  ||||| | ||| | |||  ||| | | ||||  |   |  |||| |||| |||  ||| |
||||  || | |\\-+-++---++-++---++++-++-++-++-+++++++++-++++--+++-+--+++-----++-++-+--+---+--+++++-+-+++-/ |||  ||| | | |||\\--+---+--++++-++++-+/|  ||| |
||||  || |/+--+-++---++-++---++++-++-++-++-+++++++++-++++\\ ||| |  |||     || || |  |   |  ||||| | |||   |||  ||| | | |||   |   |  |||| |||| | |  ||| |
||||  || |||  | ||   || ||   |||| || || || ||||||||| ||||| ||| \\--+++-----++-++-/  |   |  ||||| | |||   |||  ||| | | |||   |   |  |||| |||| | |  ||| |
|\\++--++-+++--+-++---++-++---+++/ || \\+-++-+++++++++-+++++-+++----+++-----++-++----+---+--+++++-+-+++---/||  ||| | | |||   |   |  |||| |||| | |  ||| |
| ||/-++-+++--+-++---++-++---+++--++--+-++-+++++++++-+++++-+++----+++-----++-++----+---+--+++++\\| |||    ||  ||| | | |||   |   |  |||| |||| | |  ||| |
| ||| || |||  | ||   || ||   |\\+--++--+-++-+++++++++-/|||| |||    |||     || ||    |   |  ||||||| |||    ||  ||| | | |||   |   |  |||| |||| | |  ||| |
| ||| || |||  | ||   || ||   | | /++--+-++-+++++++++--++++-+++\\   |||     || ||    |   |  ||||||| |||    ||  ||| | | |||   |   |  |||| |||| | |  ||| |
| v|| || |||  | ||   || ||   | | |||  | ||/+++++++++-\\|||| ||||   |||     || ||    |  /+--+++++++-+++----++\\ ||| | | |||   |   |  |||| |||| | |  ||| |
| ||| || |||  | ||   || ||   |/+-+++--+-++++++++++++-+++++-++++---+++--\\  || ||  /-+--++--+++++++-+++-\\  \\++-+++-+-+-+++---+---+--++++-/||| | |  ||| |
| ||| || |||  | ||   || ||   ||| |||  | |||||||||||| ||||| ||||   |||  |  || ||  | |  ||  ||||||| ||| |   || ||| \\-+-+++---+---+--++++--++/ | |  ||| |
| ||| || |||  | ||   || ||   ||| |||  | |||||||||||| ||||| ||||   |||  |  || ||  | |  ||  ||||||| ||| |   || |||   | |||   |   |  ||||  ||  | |  ||| |
| ||| || |||  | ||   || ||   ||| |||  | |||||\\++++++-+++/| ||||   |||  |  || ||  | |  ||  ||||||| ||| |   \\+-+++---+-+++---+---+--++++--++--+-+--+++-/
| ||| || |||  | ||   || ||   ||| |||  | ||||| |||||| ||| | ||||   |||  |  || ||  | |  ||  ||||||| ||| | /--+-+++---+\\|||   |   |  ||||  ||  | |  |||  
| ||| || |||  | ||   || ||   ||| |||  | ||||| ||v||| ||| | ||||   \\++--+--++-++--+-+--++--+++++++-+++-+-+--+-+++---+++++---+---/  ||||  ||  | |  |||  
| ||| || |||  | ||   || ||   ||| |||/-+-+++++>++++++-+++-+-++++----++--+--++-++--+-+--++--+++++++-+++-+-+--+-+++---+++++---+-\\    ||||  ||  | |  |||  
| ||| || |||  | ||   \\+-++---+++-++++-+-+++/| |||||| ||| | ||||  /-++--+->++-++--+-+--++--+++++++-+++-+-+--+-+++---+++++---+-+---\\||||  ||  | |  |||  
| ||| || |||  | ||    \\-++---+++-++++-+-+++-+-++++++-+++-+-++++--+-++--+--++-++--+-+--++--+++++++-+++-+-+--+-+++---++/||   | |   |||||  ||  | |  |||  
| ||| || |||  | ||      ||   ||| |||| | ||| | |||||| ||| | ||||  | ||  |  || ||  | |  ||  ||||||| ||| | |  | |||   || ||   | |   |||||  ||  | |  |||  
| \\++-++-+++--+-++------++---+++-++++-+-+++-+-++++++-+++-+-++++--+-++--+--++-++--+-+--+/  ||||||| ||| | |  | |||   || ||   | |   |||||  ||  | |  |||  
|  |\\-++-+++--+-++------++---+++-++++-+-+++-+-++++++-+++-+-++++--+-++--+--++-++--+-+--+---+++++/| ||| | |  | |||   || ||   | |   |||||  ||  | |  |||  
|  |  || |||  | ||   /--++---+++-++++-+-+++-+-++++++\\||| | ||||  | ||  |  || ||  | |  |   ||||| |/+++-+-+--+-+++---++-++---+-+---+++++--++--+-+\\ |||  
|  |  || |||  | ||   |  ||   ||| |||| | ||| | |||||\\++++-+-++++--+-++--+--++-++--+-+--+---+++++-+++++-+-+--+-+++---++-/|   | |   |||||  ||  | || |||  
|  |  || |||  | \\+---+--++---+++-++++-+-+++-+-/\\+++-++++-+-++++--+-++--+--++-++--+-+--+---+++++-+++++-+-+--+-+++---++--+---+-+---+++/|  ||  | || |||  
|  |  || |||  |  |   |  ||   ||| \\+++-+-+++-+---+++-++++-+-+++/  | ||  |  ||/++--+-+--+---+++++-+++++-+-+--+-+++---++--+---+-+--\\||| |  ||  | || |||  
|  |  || |||  |  |   |  ||   |||  ||| | ||| |   ||| |||| | |\\+---+-++--+--+++++--+-+--+---+++++-+++++-+-+--+-/||   ||  |  /+-+--++++-+--++--+\\|| |||  
|  |  ||/+++--+--+---+--++---+++--+++-+-+++-+---+++-++++-+-+-+---+-++--+->+++++-\\| |  |   ||||| \\++++-+-+--+--++---++--+--++-+--++++-+--++--/||| |||  
|  |  ||||||  |  |   |  ||   |||  ||| | |\\+-+---+++-++++-+-+-+---+-++--+--+++++-++-+--+---+++++--++++-+-+--+--++---++--+--++-+--++++-+--++---+++-/||  
|  |  ||||||  |  |   |  ||   |||  ||| | | | |   ||| |||| | | | /-+-++--+-\\||||| || |  |   |||||  |||| | |/-+--++---++-\\|  || |  |||| |  ||   |||  ||  
|  |  ||||||/-+--+---+--++---+++--+++-+-+-+-+---+++-++++-+-+-+-+-+-++--+-++++++-++-+--+---+++++--++++-+-++-+--++\\  || ||  || |  |||| |/-++---+++\\ ||  
|  |  ||||||| |  |   |  ||   |||  ||| | | |/+---+++-++++-+-+-+-+-+-++--+-++++++-++-+--+---+++++--++++-+-++-+--+++--++-++--++-+--++++-++-++-\\ |||| ||  
|  |  ||||||| |  |  /+--++---+++--+++-+-+-+++---+++-++++-+-+-+-+\\| ||  | |||||| || |  |   |||||  |||| | || |  |||  || ||  || |  |||| || || | |||| ||  
|  |  ||||||| |  |  ||  ||   |||  ||| | | |||   ||| |||| | | | ||| ||  | |||||| || |  |   |||||  |||\\-+-++-+--+++--++-++--++-+--++++-++-+/ | |||| ||  
|  |  ||||||| |  |  ||  ||   |||  ||| | | |||   ||| |||| | | | ||| ||  | |||||| || |  |   |||||  |||  | || |  |||  || ||  || |  |||| || |  | |||| ||  
|  |  ||||||| |  |  ||  ||   |||  ||| | | ||| /-+++-++++-+-+-+-+++-++--+-++++++-++-+--+---+++++--+++--+-++-+--+++--++-++--++-+--++++-++-+--+-++++-++\\ 
|  |  ||||||| |  |  ||  ||/--+++--+++-+-+-+++-+-+++-++++-+-+-+-+++-++--+-++++++-++-+--+---+++++--+++--+-++-+--+++--++-++--++-+-\\|||| || |  | |||| ||| 
|  |  ||||||| |  |  ||  |||  |||  ||| | | ||\\-+-+++-++++-+-+-+-+++-++--+-++++++-++-+--+---+++++--+++--+-++-+--+++--++-++--++-+-+++++-/| |  | |||| ||| 
|  |  ||\\++++-+--+--++--+++--+++--+++-+-+-++--+-+++-++++-+-+-+-+++-++--+-++++++-/| |  |   |||||  |||  | || |  ||v  || ||  || | |||||  | |  | |||| ||| 
|  |  || |||| |  |  ||  |||  |||  ||| | \\-++--+-+++-++++-+-+-+-+++-++--+-++++++--+-+--+---+++++--+++--+-++-+--/||  || ||  || | |||||  | |  | |||| ||| 
|  |  || |||| |  |  ||  |||  ||\\--+++-+---++--+-++/ |||| | | | ||| ||  | ||||||  \\-+--+---+++++--+++--/ || |   ||  || ||  || | |||||  \\-+--+-+++/ ||| 
|  |  || |||| |  |  ||  \\++--++---+++-+---++--+-++--++++-+-+-+-+++-++--+-++++++----/  |   |||||  |||    |\\-+---++--++-/\\--++-+-+++/|    |  | |||  ||| 
|  |  || |||| |  |  ||   ||  ||   ||| |   ||  | |\\--++++-+-+-/ ||| ||  | |||\\++-------+---+++++--+++----+--+---++--++-----++-+-+/| |    |  | |||  ||| 
|  |  || |||| |  |  ||   ||  ||   ||| |   ||  | |   |||| | |   ||| ||  | ||| ||       |   ||\\++--+++----+--+---++--++-----++-+-+-+-/    |  | |||  ||| 
|  |  || \\+++-+--+--++---++--++---+++-+---++--+-+---++++-+-+---+++-++--+-+++-++-------+---++-++--+++----+--+---/|  ||     || | | |      |  | |||  ||| 
| /+--++--+++-+--+--++---++--++---+++-+---++--+-+---++++-+-+---+++-++--+-+++-++-------+---++-++--+++----+--+-\\  |  ||     || | | |      |  | |||  ||| 
| ||  ||  ||| |  |  ||   ||  ||   ||| |   ||  | |   ||\\+-+-+---+++-++--+-+++-/|       \\---++-++--+++----+--/ |  |  ||     \\+-+-+-+------+--+-/||  ||| 
| ||  ||  ||| |  |  ||   ||  ||   ||| |   ||/-+-+---++-+-+-+---+++-++--+-+++--+-----------++-++--+++----+----+--+--++------+\\| | |      |  |  ||  ||| 
| ||  ||  ||| |  |  ||   ||  ||   ||| |   ||| | |   || | | |   ||| ||  | |||  |           || ||  |||    |    |  |  ||      ||| | |      |  |  ||  ||| 
| ||  ||  ||| |  |  ||   ||  ||   |\\+-+---+++-+-+---++-+-+-/   ||| ||  | |||  |           || ||  |||    |    |  |  ||      ||| | |      |  |  ||  ||| 
| ||  ||  ||| |  | /++---++-\\||   | | |   ||| | |   || | |     ||| ||  | |\\+--+-----------++-++--+++----+----+--+--/|      ||| | |      |  |  ||  ||| 
| ||  ||  ||| |  | |||   || |||   | | |   ||| | |   || | |     ||| |\\--+-+-+--+-----------++-++--++/    |    |  |   |      ||| | |      |  |  ||  ||| 
| ||  ||  ||| |  | |||   || |||   | | |   ||| | |   || | |     ||| |   | | |  |           || \\+--++-----+----+--+---+------+++-+-+------/  |  ||  ||^ 
| ||  ||  ||| |  | |||   || |||   | \\-+---+++-+-+---++-+-+-----+++-+---+-+-+--+-----------++--+--++-----+----+--+---+------++/ | |         |  ||  ||| 
| ||  ||  ||| |  | ||\\---++-+++---+---+---+++-+-+---/| | |     ||| |   | | |  \\-----------++--+--++-----+----+--+---+------++--+-+---------+--++--/|| 
| ||  \\+--+++-+--+-++----++-+++---+---+---+++-+-+----+-+-+-----+++-/   | | |              ||  \\--++-----+----+--+---+------++--+-+---------+--++---/| 
| ||   |  ||| |  | ||    || |||   |   |   ||| | |    | | |     |||     | | |              ||     ||     |    |  |   |      ||  | |         |  ||    | 
\\-++---+--+++-+--+-++----++-+++---+---+---+++-+-+----+-+-+-----+++-----+-+-+--------------/|     ||/----+----+--+---+------++--+-+-------\\ |  ||    | 
  ||   |  ||| |  | ||    || |||   |   |   ||| | |    | | |     |||     | | |     /---------+-----+++----+----+--+---+------++--+-+-------+-+--++-\\  | 
  ||   |  ||| |  | ||    |\\-+++---+---+---+++-+-+----+-+-+-----+++-----+-+-+-----+---------+-----+++----+----+--+---+------++--/ |       | |  || |  | 
  ||   |  ||| |  | ||    \\--+++---+---+---+++-+-+----+-+-+-----+++-----+-+-+-----+---------+-----+/|    |    |  |   |      ||    |       | |  || |  | 
  ||   \\--+++-+--+-++-------+++---+---+---+++-+-+----+-+-+-----+++-----+-+-+-----+---------/     | |    |    |  |   |      ||    |       | |  || |  | 
  ||      ||| |  | \\+-------/||   |   |   ||| | |    | | |     |||     | | \\-----+---------------+-+----+----+--+---+------/|    |       | |  || |  | 
  ||      |\\+-+--+--+--------/|   |   |   ||| | |    | | |   /-+++-----+-+-------+-----<---------+-+----+--\\ |  |   |       |    |       | |  || |  | 
  ||      | | |  |  |         |   |   |   ||| \\-+----+-+-+---+-+++-----+-+-------+---------------+-+----+--+-+>-+---+-------+----+-------+-+--++-+--/ 
  ||      | | |  |  |         \\---+---+---+++---+----+-+-+---+-+++-----/ |       |               | |    |  | |  |   |       |    |       | |  || |    
  ||      | | |  |  |             |   |   ||\\---+----+-+-+---+-+++-------+-------+---------------+-+----+--+-+--+---+-------/    |       | |  || |    
  ||      | | |  |  |             |   |   \\+----+----/ | |   | |||      /+-------+--------\\      | |    |  | |  |   |            |       | |  || |    
  ||      | | |  |  |             |   |    |    |      | |   | |||      ||       |        |      \\-+----+--+-+--+---+------------+-------+-+--+/ |    
  ||      | \\-+--+--+-------------+---+----+----+------+-+---+-+++------++-------+--------+--------+----+--+-+--/   |            |       | |  |  |    
  |\\------+---+--/  |             |   |    |    |      | |   | ||\\------++-------+--------+--------+----+--+-+------+------------/       | |  |  |    
  \\-------+---+-----+-------------+---+----+----+------+-+---+-++-------++-------+--------+--------+----+--+-/      |                    | |  |  |    
          |   |     |             |   \\----+----/      | |   | ||       ||       |        |        |    |  |        |                    | |  |  |    
          |   |     |             \\--------+-----------+-+---+-++-------++-------+--------+--------+----+--+--------+--------------------+-+--/  |    
          |   \\-----+----------------------+-----------/ |   \\-++-------++-------+--------+--------+----+--/        |                    | |     |    
          |         \\----------------------+-------------+-----+/       ||       \\--------+--------+----+-----------+--------------------+-+-----/    
          |                                |             |     \\--------+/                |        |    \\-----------/                    | |          
          |                                \\-------------+--------------+-----------------+--------+-------------------------------------+-/          
          \\----------------------------------------------/              \\-----------------/        \\-------------------------------------/            
"""
