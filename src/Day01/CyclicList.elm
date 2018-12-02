module Day01.CyclicList exposing (CyclicList, foldUntil, fromList, map, step)


type CyclicList a
    = CyclicList (List a) ( a, List a )


step : CyclicList a -> ( a, CyclicList a )
step (CyclicList current (( first, elts ) as mem)) =
    case current of
        [] ->
            ( first, CyclicList elts mem )

        x :: xs ->
            ( x, CyclicList xs mem )


fromList : a -> List a -> CyclicList a
fromList el list =
    CyclicList [] ( el, list )


map : (a -> b) -> CyclicList a -> CyclicList b
map f (CyclicList current ( first, elts )) =
    CyclicList (List.map f current) ( f first, List.map f elts )


foldUntil : (a -> b -> Result b c) -> b -> CyclicList a -> c
foldUntil f start cl =
    let
        ( nextEl, nextCl ) =
            step cl
    in
    case f nextEl start of
        Err newStart ->
            foldUntil f newStart nextCl

        Ok res ->
            res
