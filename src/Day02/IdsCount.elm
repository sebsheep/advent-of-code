module Day02.IdsCount exposing (comp2words, res1, res2)

import Dict exposing (Dict)
import List.Extra as List


input =
    [ "cnjxpritdzhubeseewfmqagkul"
    , "cwyxpgitdzhvbosyewfmqagkul"
    , "cnfxpritdzhebosywwfmqagkul"
    , "cnjxpritdzgvbosyawfiqagkul"
    , "cnkxpritdzhvbosyewfmgagkuh"
    , "gnjxprhtdzhebosyewfmqagkul"
    , "cnjxpriedzevbosyewfjqagkul"
    , "cnjxpritdzhpyosyewfsqagkul"
    , "cnjxprltdzhvbosyewfmhagkzl"
    , "cnjxfritdjhvbosyewfmiagkul"
    , "xnjxpritdzhvbosyewfmqagkgn"
    , "cnjxpritdzmvzosyewfhqagkul"
    , "cljxxritdzhvbosyewfmragkul"
    , "cnjxjritdzhvbovyewfmvagkul"
    , "cnjxprdtdzhpbosyewvmqagkul"
    , "cojxprdtdzhzbosyewfmqagkul"
    , "cnjxpritgzhvfgsyewfmqagkul"
    , "knjxprptdzhvbosyecfmqagkul"
    , "cnjxpritdzhvbvsyeyfmqagkuc"
    , "cnjxpritdzhvbosvewfmoagjul"
    , "cnjxpritdzhvbosyfwfmbagkjl"
    , "cnjxpjitazhvbosfewfmqagkul"
    , "cnjtpfitdzhvbosyewfmiagkul"
    , "ckjxpritdzhvbysyewfmqagoul"
    , "cnjxvritdzhvbfsyewfmqalkul"
    , "cnjipqitdzhvbosyewfeqagkul"
    , "cnjhpritdzhvbosyewymqjgkul"
    , "cnjxprrtdzhvbosyewfmlkgkul"
    , "cnjxnritdzhvbopyewfmqaskul"
    , "cxjxpritdzhvtosyewjmqagkul"
    , "cnjxpritdzhvbjsyewfrqagkwl"
    , "cnjxhritdzhubosyewfmqagvul"
    , "cnjxpritdzhvbosyyyfmeagkul"
    , "cnjxkritdzhvaoeyewfmqagkul"
    , "cnjxpritdzhvtotyewfmqazkul"
    , "cnjxoriadzhvbosyewfmqcgkul"
    , "cnjxpritdzhcbosyewfmkapkul"
    , "fnjxprtddzhvbosyewfmqagkul"
    , "cnjxmvitdzhvbosyewfmqagrul"
    , "cnjxpyitdzhibosyewfmqagktl"
    , "cyjxprxtdzhvbosyewbmqagkul"
    , "onjxpditdzhvbosyeofmqagkul"
    , "cnjxprixdzhvbosuewftqagkul"
    , "cnjxpritdrhvaosyewymqagkul"
    , "cnjxpritdzhhbokyewfvqagkul"
    , "cnjxpritczhvbosyewfmqwgxul"
    , "cnjxpribdzqvbnsyewfmqagkul"
    , "ynpxpritdzhvbvsyewfmqagkul"
    , "cnjxprirdzhvboerewfmqagkul"
    , "cnjxpritdxhvbosyewfmgavkul"
    , "cnwxprntdzhvbosyewfmqagkuk"
    , "cnjxpritzzhvbosyewfmcagktl"
    , "bbjxpritdzhvbosyetfmqagkul"
    , "cnjxpbitdzhvbosyewrmqagkui"
    , "cnjxwrildzcvbosyewfmqagkul"
    , "cnqxpoitdzhvbosnewfmqagkul"
    , "cnzxpritdzhvbosyewfmqazkfl"
    , "cnjxpriddzhvoosyewfmhagkul"
    , "znjxpritdzhvbosjewfmqagkur"
    , "cnjxpritdzhvbosyewcmfagkuk"
    , "cnjxpritdzhvbomyywnmqagkul"
    , "cnjxpgitjzhvbosyejfmqagkul"
    , "cnjxpkitdzjvbosyewfmqcgkul"
    , "cnjxpritduhvbosyewfmqfkkul"
    , "cnfxpritdzhvbgsyewfmqwgkul"
    , "cnjxpritdzhvbosywufmqaskul"
    , "cnjxprittzhvboryswfmqagkul"
    , "cndxpritpzrvbosyewfmqagkul"
    , "cnjxpritdzhvbosyewfwqazkum"
    , "cccxprmtdzhvbosyewfmqagkul"
    , "cnjxpzitdzhvlbsyewfmqagkul"
    , "cnjxdrigdzhvbosyewfmqagsul"
    , "fhjxpritdzhvbosyewfmqagkcl"
    , "cnjxpritdzhvdosyewffqagaul"
    , "cnjxprikdztvbosyekfmqagkul"
    , "cnjxpritdzhvbohiewfmqagkue"
    , "cnjxpritdzhvbowyetfmqegkul"
    , "cnuxpritdzhvbosyewmmqapkul"
    , "qnjxpritdzhvbosyewfmjakkul"
    , "cnjxpritdzlvbosyewiaqagkul"
    , "cnjxpritdzhpoosyewfmvagkul"
    , "cdjxpritdzhvboryewfbqagkul"
    , "cnjxppitxzhvbosyewymqagkul"
    , "cnjxpywtdzhvboiyewfmqagkul"
    , "cnjxpritdzpvbosyezfmqaqkul"
    , "cnjppritdghvbosyewfdqagkul"
    , "cmjxpritdzhvbosvewfmqagkup"
    , "cnjxoritdzhvbosylffmqagkul"
    , "cnjxfritdzhvbojyewfmqagkvl"
    , "cnjxpritdzhvbozyewgmqlgkul"
    , "cnjxlritdzhvbosyewfmqalkug"
    , "cnyxprittzhvbosyewfmsagkul"
    , "cnjxprytdzcvdosyewfmqagkul"
    , "ctjxpritdzhvbosyedfmqagkil"
    , "cnjxpvitdzhrbosyewfmqaekul"
    , "cnyxyritdzhvbospewfmqagkul"
    , "cnjxoritwzhvbosyewrmqhgkul"
    , "cnjxpritdzhjbosyqwsmqagkul"
    , "cnjzprindzhvbosyewfmqabkul"
    , "cnjspritdzhvbosysffmqagkul"
    , "cnxxpritdzhvbosyelfmqageul"
    , "bnjhpritdzhvbosyewfmzagkul"
    , "cnjxbhitdzhdbosyewfmqagkul"
    , "cnjxprktdzmvbosyewfmqagkuj"
    , "cnjxprixdzhvbqsyewfmqmgkul"
    , "cnjxpkitdzhvbosyewfmqagbum"
    , "cnjhpritdzhxbosyewfmqagjul"
    , "cnjxpritdzzvbosyewuqqagkul"
    , "cnjxprhtdzhvuopyewfmqagkul"
    , "cnjxpritdzhjqosyewfmqagkgl"
    , "cnzxpritdzhvbosyejfmuagkul"
    , "cnvxpritoohvbosyewfmqagkul"
    , "cnjxpmitdzwvbosyemfmqagkul"
    , "cnjoprittzzvbosyewfmqagkul"
    , "cnjxpgitdzhvbosytwfmqsgkul"
    , "cnjxprrtdehvbosyewfnqagkul"
    , "onjxpjitdzgvbosyewfmqagkul"
    , "cnjxpmitdzhvbopaewfmqagkul"
    , "cnjxpritqzhvbosoewfrqagkul"
    , "cnjxpnitdzhvbosyewfmqagkjy"
    , "cnsxpritdzhvbosyewfmqjykul"
    , "cnjxpriidzhvbosyewfmqxgkut"
    , "cnjxpyitdzhnbosyewfgqagkul"
    , "cnjxpritdzhbboyyewfmqagsul"
    , "cnjxpeitdzsvbosyewfmqabkul"
    , "cnjxpritdzhzvosyewfmragkul"
    , "cnjrpritdzhmbosyewfmqrgkul"
    , "cnjxpritdzhmbosyenfmqaglul"
    , "cnjxqrntdzhvboswewfmqagkul"
    , "cnjxprdtpzhvbosyewfmqagkcl"
    , "cnjxpritdzhvsdsyewfmqagkur"
    , "cnjxpritdzhvvosyewumqhgkul"
    , "cnzxpritdznvhosyewfmqagkul"
    , "ynjypritdzhvbosyewfmqagkuz"
    , "cnjxpnitdzhvbocyezfmqagkul"
    , "vnjxpritdzhvbfsyewfmjagkul"
    , "cnjfpritdzhvbosyewfmqagkzu"
    , "cnjxpritdzhbbosyewfmlegkul"
    , "cnjxpnitdzhvbosyesfmbagkul"
    , "cnjxpritezwvbosyewfmqagkgl"
    , "cujxpritdzhqbosyawfmqagkul"
    , "cnjxprindzhrbosyerfmqagkul"
    , "cntxpritdzhvbosyewfmqauxul"
    , "cnjxpvitdzhvbosyepfmqagkuy"
    , "cnjxdrqtdzhvbosdewfmqagkul"
    , "cnnxpritdzhvvosyenfmqagkul"
    , "lnjxphitdzhvbosyewfaqagkul"
    , "cngxpritdzhhbobyewfmqagkul"
    , "uncxphitdzhvbosyewfmqagkul"
    , "cnjxpribdehvbosfewfmqagkul"
    , "cnjxppitdqhvbmsyewfmqagkul"
    , "gnjxpritkzhvbosyewfbqagkul"
    , "znjxpritdzhvbowycwfmqagkul"
    , "cnjxpgitdzhvbosyewidqagkul"
    , "cnjxhritdzhvbowyswfmqagkul"
    , "injxkritdzhvbjsyewfmqagkul"
    , "cmjupritgzhvbosyewfmqagkul"
    , "cnjxpritdzbvjoeyewfmqagkul"
    , "cnjxpritdkhvbosyewlmuagkul"
    , "cnkxpritdzhebvsyewfmqagkul"
    , "cyjxptitdzhvbosyewfmqagkuv"
    , "cnjxpritdzhvbodrewflqagkul"
    , "cnjxpratdzhvbksyewfhqagkul"
    , "cnjxpoitdzhvbosjewxmqagkul"
    , "cnjxprhidzhvbasyewfmqagkul"
    , "cnjxpritdzhvbosqewvmqagmul"
    , "cnjxoritdzhvbosyzifmqagkul"
    , "mnjxpritdzhvbcsyeyfmqagkul"
    , "cnjhpritgzhvbosyewfmqngkul"
    , "cnjxprijdzevbesyewfmqagkul"
    , "cnexprqtdzhvbosyewvmqagkul"
    , "cnjxpxitdzhvbosyawfmqmgkul"
    , "cnjxpritdzhvbosyirfmqaxkul"
    , "cqjxpcitdzhvboslewfmqagkul"
    , "cmjxpqitdztvbosyewfmqagkul"
    , "cnbxpritdzhvfosyewfmuagkul"
    , "cnjxprrtdzhvbosumwfmqagkul"
    , "cnjxprttdvhvbossewfmqagkul"
    , "cnjxpritdzhvbcsuewfaqagkul"
    , "cbjxpritdzhvbosyewfhqalkul"
    , "cnjxprithzhvbosjcwfmqagkul"
    , "chjxpritdzhvbosyewftcagkul"
    , "cnjxprirdchvdosyewfmqagkul"
    , "cnjxpritdxhvbosyewfmqcgkal"
    , "cnjxpriidchvbosrewfmqagkul"
    , "cnjhprizdzhvbosyewfmqagvul"
    , "cnjwpritdzhpbosyewfmqaqkul"
    , "cnjxpgitfzhvbosyxwfmqagkul"
    , "cnjxpjiedzhvbosywwfmqagkul"
    , "cnjxpritdzhvbosyewfpqynkul"
    , "xnixlritdzhvbosyewfmqagkul"
    , "cnjxoritdznvbosyehfmqagkul"
    , "cnjxpritdzhvbjsyewsmqagcul"
    , "lnjxpritdzhvkosyewjmqagkul"
    , "cnjxpritdzhvbosyedfiqvgkul"
    , "cnjxpritdzhqbdsyerfmqagkul"
    , "cnjxpritdzavbosyywfmqagvul"
    , "dmjxprithzhvbosyewfmqagkul"
    , "cnjxpriqdzhvnosyeofmqagkul"
    , "cnjxpritdxhvboszewfmqkgkul"
    , "cnjxpritdzxvbosjewymqagkul"
    , "cnjxpritdzngbosyewfmqugkul"
    , "cajxpritdnhvbosyerfmqagkul"
    , "cnsxpritdzhvbosymwfmqagcul"
    , "cnjxoritdzhvbosyewrmqhgkul"
    , "cnjxpritdzhvposyewfmqagkwo"
    , "cnjxpriazzhvbosyeufmqagkul"
    , "cnjxrritdzhvbosymhfmqagkul"
    , "cnjxprztdzhvbosyewfmqtgkum"
    , "cnjxpritdzhvbmsyewfmqatkun"
    , "cnuxpritdzhvbosyewfmqagvur"
    , "ctjxxritdzhvbosyewfvqagkul"
    , "cnjxpritdzlvbosyevfmqagkll"
    , "cnjxpritdzhlbosyewfmqagasl"
    , "cnjxpritwzhvbosyewfcxagkul"
    , "cyjxpritdzhfbosyewfmqagcul"
    , "cnjxpritxghvkosyewfmqagkul"
    , "ctjxpritdjhvbosyewfmqkgkul"
    , "cnjxpritxzhvbosyewjmbagkul"
    , "unjxpritdzhkbosyewfmqaghul"
    , "cnjoprqtdzhvbosyewzmqagkul"
    , "rnjxprgtgzhvbosyewfmqagkul"
    , "cnjgpqitdzhvbosyewfaqagkul"
    , "cnjxpritdzuybosyewfmqagful"
    , "cnjxprqtdahvbosyewfnqagkul"
    , "cnjxpritdzhmkhsyewfmqagkul"
    , "wnjxpritdzhvbosiewfmqagkml"
    , "cnjmpritdzhvbosyjwfmqagkdl"
    , "cnjopritdzhvbksyewfmqrgkul"
    , "cnlxpritdzhvbosyewfmomgkul"
    , "cgjxpritdzhvbbsyewfmxagkul"
    , "cnaxpritdvhvnosyewfmqagkul"
    , "cnjxprijdzhvbkmyewfmqagkul"
    , "cnjxpritdzhvposyewzmqagkuz"
    , "cnuxpuitdzdvbosyewfmqagkul"
    , "cnjxprifdzjvbosyewfyqagkul"
    , "cnhspritdzhvbosyewfmqaghul"
    , "cnjxprcbdzfvbosyewfmqagkul"
    , "lnjapritdzhvbosyewfmqegkul"
    , "cnjxprisszhvbosyewqmqagkul"
    , "cnjxpritdzhvbosyeifmsagoul"
    , "cnjxpritrfhvbosyewfmqagkuz"
    , "cnjxkritdzmvboqyewfmqagkul"
    , "cnjxpritdzhvbosyedfmqzgkzl"
    , "cnjxprifdzhvbosyswfmqagksl"
    , "cnjxoritdzhvbosyxwfmhagkul"
    , "cnjhpritdzzvbosfewfmqagkul"
    , "cnjxprityjhvbomyewfmqagkul"
    , "cnjbpritdzhvbosyywfmqagkuf"
    , "cnjxprrtdzhvbosyewgmqagtul"
    ]


type alias IdCounter =
    { two : Int
    , three : Int
    }


counter : String -> Dict Char Int
counter s =
    String.foldr
        (\c acc ->
            case Dict.get c acc of
                Just n ->
                    Dict.insert c (n + 1) acc

                Nothing ->
                    Dict.insert c 1 acc
        )
        Dict.empty
        s


countRepeat : String -> IdCounter
countRepeat s =
    let
        nbOccurences =
            counter s
                |> Dict.values

        boolToInt b =
            if b then
                1

            else
                0
    in
    { two = boolToInt <| List.member 2 nbOccurences
    , three = boolToInt <| List.member 3 nbOccurences
    }


res1 =
    let
        tot =
            List.map countRepeat input
                |> List.foldr
                    (\el acc ->
                        { two = el.two + acc.two
                        , three = el.three + acc.three
                        }
                    )
                    { two = 0, three = 0 }
    in
    tot.two * tot.three


comp2words : String -> String -> Bool
comp2words s1 s2 =
    List.zip (String.toList s1) (String.toList s2)
        |> List.filter (\( c1, c2 ) -> c1 /= c2)
        |> List.length
        |> (\n -> n == 1)


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


res2 =
    cartesianProduct input input
        |> List.filter (\( s1, s2 ) -> comp2words s1 s2)
