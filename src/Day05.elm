module Day05 exposing (cancel, res1, res2)

import InputDay05 exposing (input)


cancel : Char -> Char -> Bool
cancel l1 l2 =
    abs (Char.toCode l1 - Char.toCode l2) == 32


res1 =
    react input


res2 =
    filteredInput
        |> List.map react
        |> List.minimum


react polymer =
    String.foldr
        (\letter stack ->
            case stack of
                [] ->
                    [ letter ]

                x :: xs ->
                    if cancel x letter then
                        xs

                    else
                        letter :: stack
        )
        []
        polymer
        |> List.length


filteredInput =
    List.map Char.fromCode (List.range 65 90)
        |> List.map
            (\letter ->
                input
                    |> String.replace (String.fromChar letter) ""
                    |> String.replace (String.fromChar <| Char.toLower letter) ""
            )
