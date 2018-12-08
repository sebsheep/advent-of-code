module Main exposing (Graph, insert, insertEdge)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import List.Extra as List
import Set exposing (Set)


type alias Graph =
    { outcoming : Dict Char (Set Char)
    , roots : Set Char
    , workInProgress : Set Char
    }


main =
    Browser.sandbox
        { init = ()
        , view = \_ -> text <| Debug.toString res2
        , update = \_ () -> ()
        }


emptyGraph =
    Graph Dict.empty Set.empty Set.empty


insert :
    comparable1
    -> comparable2
    -> Dict comparable1 (Set comparable2)
    -> Dict comparable1 (Set comparable2)
insert k v d =
    case Dict.get k d of
        Nothing ->
            Dict.insert k (Set.singleton v) d

        Just vs ->
            Dict.insert k (Set.insert v vs) d


insertEdge : ( Char, Char ) -> Graph -> Graph
insertEdge ( vertexIn, vertexOut ) graph =
    let
        targets =
            values graph.outcoming

        roots1 =
            if Set.member vertexOut graph.roots then
                Set.remove vertexOut graph.roots

            else
                graph.roots
    in
    Graph
        (insert vertexIn vertexOut graph.outcoming)
        (if
            Dict.member vertexIn graph.outcoming
                || Set.member vertexIn targets
         then
            roots1

         else
            Set.insert vertexIn roots1
        )
        graph.workInProgress


values : Dict comparable1 (Set comparable2) -> Set comparable2
values d =
    List.foldr Set.union Set.empty (Dict.values d)


toGraph : List ( Char, Char ) -> Graph
toGraph =
    List.foldr
        insertEdge
        emptyGraph


roots : Graph -> Set Char
roots graph =
    List.foldr Set.union Set.empty (Dict.values graph.outcoming)
        |> Set.diff (Set.fromList <| Dict.keys graph.outcoming)


pop : Graph -> Maybe ( Char, Graph )
pop graph =
    graph.roots
        |> Set.toList
        |> List.head
        |> Maybe.map
            (\char ->
                ( char
                , let
                    newOutcoming =
                        Dict.remove char graph.outcoming

                    newTargets =
                        values newOutcoming
                  in
                  Graph
                    newOutcoming
                    (Set.union
                        graph.roots
                        (Dict.get char graph.outcoming
                            |> Maybe.withDefault Set.empty
                            |> (\s -> Set.diff s newTargets)
                        )
                        |> Set.remove char
                    )
                    graph.workInProgress
                )
            )


popWip : Graph -> Maybe ( Char, Graph )
popWip graph =
    Set.diff graph.roots graph.workInProgress
        |> Set.toList
        |> List.head
        |> Maybe.map
            (\char ->
                ( char
                , Graph
                    graph.outcoming
                    graph.roots
                    (Set.insert char graph.workInProgress)
                )
            )


completeWork : Char -> Graph -> Graph
completeWork char graph =
    let
        newOutcoming =
            Dict.remove char graph.outcoming

        newTargets =
            values newOutcoming
    in
    Graph
        newOutcoming
        (Set.union
            (Set.remove char graph.roots)
            (Dict.get char graph.outcoming
                |> Maybe.withDefault Set.empty
                |> (\s -> Set.diff s newTargets)
            )
            |> Set.remove char
        )
        (Set.remove char graph.workInProgress)


loopPop : Graph -> List Char
loopPop graph =
    case pop graph of
        Just ( char, newGraph ) ->
            char :: loopPop newGraph

        Nothing ->
            []


duration : Char -> Int
duration char =
    Char.toCode char - 4


res1 =
    toGraph input
        |> loopPop
        |> String.fromList


res2 =
    computeTime 0 (toGraph input) []


type alias Work =
    { worker : Int
    , finishAt : Time
    , workOn : Char
    }


type alias Time =
    Int


type Event
    = WorkerFree ( Work, Graph )
    | WorkDone Work


nbOfWorkers =
    5


allworkers =
    Set.fromList (List.range 1 nbOfWorkers)


nextEvent : Time -> List Work -> Graph -> Event
nextEvent t works graph =
    case ( List.length works < nbOfWorkers, popWip graph ) of
        ( True, Just ( char, newGraph ) ) ->
            WorkerFree
                ( { worker =
                        Set.diff allworkers (Set.fromList (List.map .worker works))
                            |> Set.toList
                            |> List.head
                            |> Maybe.withDefault 0
                  , finishAt = t + duration char
                  , workOn = char
                  }
                , newGraph
                )

        _ ->
            works
                |> List.sortBy .finishAt
                |> List.head
                |> Maybe.withDefault (Work 0 0 'A')
                |> WorkDone


computeTime : Time -> Graph -> List Work -> Time
computeTime t graph works =
    if graph.roots == Set.empty then
        t

    else
        --let
        --    a =
        --        Debug.log "a" (Debug.toString ( t, graph.roots, ( works, nextEvent t works graph ) ))
        --in
        case nextEvent t works graph of
            WorkerFree ( work, newGraph ) ->
                computeTime t
                    newGraph
                    (work :: works)

            WorkDone work ->
                computeTime work.finishAt
                    (completeWork work.workOn graph)
                    (List.remove work works)



--|> loopPop |> Tuple.mapSecond String.fromList
--|> String.fromList
--input =
--    [ ( 'C', 'A' )
--    , ( 'C', 'F' )
--    , ( 'A', 'B' )
--    , ( 'A', 'D' )
--    , ( 'B', 'E' )
--    , ( 'D', 'E' )
--    , ( 'F', 'E' )
--    ]


input =
    [ ( 'I', 'Q' )
    , ( 'B', 'O' )
    , ( 'J', 'M' )
    , ( 'W', 'Y' )
    , ( 'U', 'X' )
    , ( 'T', 'Q' )
    , ( 'G', 'M' )
    , ( 'K', 'C' )
    , ( 'F', 'Z' )
    , ( 'D', 'A' )
    , ( 'N', 'Y' )
    , ( 'Y', 'Q' )
    , ( 'Q', 'Z' )
    , ( 'V', 'E' )
    , ( 'A', 'X' )
    , ( 'E', 'C' )
    , ( 'O', 'R' )
    , ( 'P', 'L' )
    , ( 'H', 'R' )
    , ( 'M', 'R' )
    , ( 'C', 'Z' )
    , ( 'R', 'L' )
    , ( 'L', 'S' )
    , ( 'S', 'X' )
    , ( 'Z', 'X' )
    , ( 'T', 'O' )
    , ( 'D', 'Z' )
    , ( 'P', 'R' )
    , ( 'M', 'Z' )
    , ( 'L', 'Z' )
    , ( 'W', 'N' )
    , ( 'Q', 'R' )
    , ( 'P', 'C' )
    , ( 'U', 'O' )
    , ( 'F', 'O' )
    , ( 'K', 'X' )
    , ( 'G', 'K' )
    , ( 'M', 'C' )
    , ( 'Y', 'Z' )
    , ( 'A', 'O' )
    , ( 'D', 'P' )
    , ( 'K', 'S' )
    , ( 'I', 'E' )
    , ( 'G', 'F' )
    , ( 'S', 'Z' )
    , ( 'N', 'V' )
    , ( 'F', 'D' )
    , ( 'A', 'Z' )
    , ( 'F', 'X' )
    , ( 'T', 'Y' )
    , ( 'W', 'H' )
    , ( 'D', 'H' )
    , ( 'W', 'G' )
    , ( 'J', 'X' )
    , ( 'T', 'X' )
    , ( 'U', 'R' )
    , ( 'O', 'P' )
    , ( 'L', 'X' )
    , ( 'I', 'B' )
    , ( 'M', 'L' )
    , ( 'C', 'R' )
    , ( 'R', 'X' )
    , ( 'F', 'N' )
    , ( 'V', 'H' )
    , ( 'K', 'A' )
    , ( 'W', 'O' )
    , ( 'U', 'Q' )
    , ( 'O', 'C' )
    , ( 'K', 'V' )
    , ( 'R', 'S' )
    , ( 'E', 'S' )
    , ( 'J', 'A' )
    , ( 'E', 'X' )
    , ( 'K', 'Y' )
    , ( 'Y', 'X' )
    , ( 'P', 'Z' )
    , ( 'W', 'X' )
    , ( 'Y', 'A' )
    , ( 'V', 'X' )
    , ( 'O', 'M' )
    , ( 'I', 'J' )
    , ( 'W', 'L' )
    , ( 'I', 'G' )
    , ( 'D', 'O' )
    , ( 'D', 'N' )
    , ( 'M', 'X' )
    , ( 'I', 'R' )
    , ( 'Y', 'M' )
    , ( 'F', 'M' )
    , ( 'U', 'M' )
    , ( 'Y', 'H' )
    , ( 'K', 'D' )
    , ( 'N', 'O' )
    , ( 'H', 'S' )
    , ( 'G', 'L' )
    , ( 'T', 'D' )
    , ( 'K', 'M' )
    , ( 'K', 'P' )
    , ( 'E', 'R' )
    , ( 'N', 'H' )
    ]
