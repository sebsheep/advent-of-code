module Main exposing (Model, Move(..), Msg(..), Point, view)

import Browser
import Collage exposing (Collage)
import Collage.Events as Events
import Collage.Layout as Layout
import Collage.Render as Render
import Color
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Set exposing (Set)
import Time


type alias Point =
    ( Int, Int )


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = Backward
    | Forward
    | Pause
    | ChangeDelay String
    | StepForward
    | StepBackward


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.move of
        NoMove ->
            Sub.none

        ForwardMove ->
            Time.every (toFloat model.frameDelay) (\_ -> StepForward)

        BackwardMove ->
            Time.every (toFloat model.frameDelay) (\_ -> StepBackward)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Backward ->
            ( { model | move = BackwardMove }, Cmd.none )

        Forward ->
            ( { model | move = ForwardMove }, Cmd.none )

        Pause ->
            ( { model | move = NoMove }, Cmd.none )

        StepBackward ->
            ( { model | currentTime = model.currentTime - 1 }, Cmd.none )

        StepForward ->
            ( { model | currentTime = model.currentTime + 1 }, Cmd.none )

        ChangeDelay delayString ->
            case String.toInt delayString of
                Just delay ->
                    ( { model | frameDelay = delay }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { frameDelay = 200
      , currentTime = 10645
      , move = NoMove
      , stars = input
      , height = ymax
      , blockSize = 10
      }
    , Cmd.none
    )


type alias Model =
    { frameDelay : Int
    , currentTime : Int
    , move : Move
    , stars : List { pos : Point, vel : Point }
    , height : Int
    , blockSize : Int
    }


type Move
    = ForwardMove
    | BackwardMove
    | NoMove


move star time =
    let
        ( x, y ) =
            star.pos

        ( vx, vy ) =
            star.vel
    in
    ( toFloat <| x + time * vx, toFloat <| y + time * vy )


view : Model -> Html Msg
view model =
    let
        cell =
            Collage.square 0.9
                |> Collage.filled (Collage.uniform Color.red)

        cells =
            model.stars
                |> List.map
                    (\star ->
                        let
                            ( x, y ) =
                                move star model.currentTime
                        in
                        Collage.shift ( x, toFloat model.height - y ) cell
                    )
                |> Collage.group
                |> Collage.scale (toFloat model.blockSize)
    in
    div []
        [ case model.move of
            NoMove ->
                button [ onClick Backward ] [ text "Backward" ]

            BackwardMove ->
                button [ onClick Pause ] [ text "Pause" ]

            ForwardMove ->
                button [ disabled True ] [ text "Backward" ]
        , case model.move of
            NoMove ->
                button [ onClick Forward ] [ text "Forward" ]

            ForwardMove ->
                button [ onClick Pause ] [ text "Pause" ]

            BackwardMove ->
                button [ disabled True ] [ text "Forward" ]
        , node "input"
            [ type_ "range"
            , Attributes.min "20"
            , step "50"
            , Attributes.max "500"
            , value <| String.fromInt model.frameDelay
            , onInput ChangeDelay
            ]
            []
        , button [ onClick StepBackward ] [ text "Step backward" ]
        , button [ onClick StepForward ] [ text "Step forward" ]
        , br [] []
        , Render.svg cells
        ]


xmax =
    List.maximum (List.map (.pos >> Tuple.first) input) |> Maybe.withDefault 0


xmin =
    List.minimum (List.map (.pos >> Tuple.first) input) |> Maybe.withDefault 0


ymax =
    List.maximum (List.map (.pos >> Tuple.second) input) |> Maybe.withDefault 0


ymin =
    List.minimum (List.map (.pos >> Tuple.second) input) |> Maybe.withDefault 0


tests =
    [ { pos = ( 9, 1 ), vel = ( 0, 2 ) }
    , { pos = ( 7, 0 ), vel = ( -1, 0 ) }
    , { pos = ( 3, -2 ), vel = ( -1, 1 ) }
    , { pos = ( 6, 10 ), vel = ( -2, -1 ) }
    , { pos = ( 2, -4 ), vel = ( 2, 2 ) }
    , { pos = ( -6, 10 ), vel = ( 2, -2 ) }
    , { pos = ( 1, 8 ), vel = ( 1, -1 ) }
    , { pos = ( 1, 7 ), vel = ( 1, 0 ) }
    , { pos = ( -3, 11 ), vel = ( 1, -2 ) }
    , { pos = ( 7, 6 ), vel = ( -1, -1 ) }
    , { pos = ( -2, 3 ), vel = ( 1, 0 ) }
    , { pos = ( -4, 3 ), vel = ( 2, 0 ) }
    , { pos = ( 10, -3 ), vel = ( -1, 1 ) }
    , { pos = ( 5, 11 ), vel = ( 1, -2 ) }
    , { pos = ( 4, 7 ), vel = ( 0, -1 ) }
    , { pos = ( 8, -2 ), vel = ( 0, 1 ) }
    , { pos = ( 15, 0 ), vel = ( -2, 0 ) }
    , { pos = ( 1, 6 ), vel = ( 1, 0 ) }
    , { pos = ( 8, 9 ), vel = ( 0, -1 ) }
    , { pos = ( 3, 3 ), vel = ( -1, 1 ) }
    , { pos = ( 0, 5 ), vel = ( 0, -1 ) }
    , { pos = ( -2, 2 ), vel = ( 2, 0 ) }
    , { pos = ( 5, -2 ), vel = ( 1, 2 ) }
    , { pos = ( 1, 4 ), vel = ( 2, 1 ) }
    , { pos = ( -2, 7 ), vel = ( 2, -2 ) }
    , { pos = ( 3, 6 ), vel = ( -1, -1 ) }
    , { pos = ( 5, 0 ), vel = ( 1, 0 ) }
    , { pos = ( -6, 0 ), vel = ( 2, 0 ) }
    , { pos = ( 5, 9 ), vel = ( 1, -2 ) }
    , { pos = ( 14, 7 ), vel = ( -2, 0 ) }
    , { pos = ( -3, 6 ), vel = ( 2, -1 ) }
    ]


input =
    [ { pos = ( 42772, -21149 ), vel = ( -4, 2 ) }
    , { pos = ( 42804, -31790 ), vel = ( -4, 3 ) }
    , { pos = ( -10445, -10502 ), vel = ( 1, 1 ) }
    , { pos = ( -31749, 21438 ), vel = ( 3, -2 ) }
    , { pos = ( -31722, 32074 ), vel = ( 3, -3 ) }
    , { pos = ( 53436, -21147 ), vel = ( -5, 2 ) }
    , { pos = ( -42336, -42437 ), vel = ( 4, 4 ) }
    , { pos = ( -42380, 21435 ), vel = ( 4, -2 ) }
    , { pos = ( 21508, -10506 ), vel = ( -2, 1 ) }
    , { pos = ( -31727, 42725 ), vel = ( 3, -4 ) }
    , { pos = ( -42383, 32083 ), vel = ( 4, -3 ) }
    , { pos = ( 42764, 21438 ), vel = ( -4, -2 ) }
    , { pos = ( -53037, -53086 ), vel = ( 5, 5 ) }
    , { pos = ( 53436, -10506 ), vel = ( -5, 1 ) }
    , { pos = ( -10445, 32083 ), vel = ( 1, -3 ) }
    , { pos = ( -53001, -53081 ), vel = ( 5, 5 ) }
    , { pos = ( -31724, -21151 ), vel = ( 3, 2 ) }
    , { pos = ( -21106, 42720 ), vel = ( 2, -4 ) }
    , { pos = ( 10847, -53083 ), vel = ( -1, 5 ) }
    , { pos = ( 21483, -21147 ), vel = ( -2, 2 ) }
    , { pos = ( 53433, -21146 ), vel = ( -5, 2 ) }
    , { pos = ( 32121, 42719 ), vel = ( -3, -4 ) }
    , { pos = ( 10834, 53364 ), vel = ( -1, -5 ) }
    , { pos = ( -31708, -42434 ), vel = ( 3, 4 ) }
    , { pos = ( -21057, -10497 ), vel = ( 2, 1 ) }
    , { pos = ( -42372, -53082 ), vel = ( 4, 5 ) }
    , { pos = ( 32143, -10502 ), vel = ( -3, 1 ) }
    , { pos = ( -31751, -53078 ), vel = ( 3, 5 ) }
    , { pos = ( -21069, 42720 ), vel = ( 2, -4 ) }
    , { pos = ( -21049, -42432 ), vel = ( 2, 4 ) }
    , { pos = ( -42376, 42727 ), vel = ( 4, -4 ) }
    , { pos = ( 42790, -21151 ), vel = ( -4, 2 ) }
    , { pos = ( 10849, 32082 ), vel = ( -1, -3 ) }
    , { pos = ( -31724, -21142 ), vel = ( 3, 2 ) }
    , { pos = ( 32159, -10498 ), vel = ( -3, 1 ) }
    , { pos = ( 32180, -21151 ), vel = ( -3, 2 ) }
    , { pos = ( 21491, 53368 ), vel = ( -2, -5 ) }
    , { pos = ( 32162, -21149 ), vel = ( -3, 2 ) }
    , { pos = ( 21482, -10505 ), vel = ( -2, 1 ) }
    , { pos = ( -52981, -53079 ), vel = ( 5, 5 ) }
    , { pos = ( 42796, 32079 ), vel = ( -4, -3 ) }
    , { pos = ( -21066, 42720 ), vel = ( 2, -4 ) }
    , { pos = ( 10862, 32083 ), vel = ( -1, -3 ) }
    , { pos = ( 21474, -53083 ), vel = ( -2, 5 ) }
    , { pos = ( -10434, -10497 ), vel = ( 1, 1 ) }
    , { pos = ( -21054, 53364 ), vel = ( 2, -5 ) }
    , { pos = ( 21534, 42723 ), vel = ( -2, -4 ) }
    , { pos = ( 10861, -21147 ), vel = ( -1, 2 ) }
    , { pos = ( 10857, -10502 ), vel = ( -1, 1 ) }
    , { pos = ( 53461, 32079 ), vel = ( -5, -3 ) }
    , { pos = ( 32132, -10505 ), vel = ( -3, 1 ) }
    , { pos = ( -42362, -21142 ), vel = ( 4, 2 ) }
    , { pos = ( -42396, 32074 ), vel = ( 4, -3 ) }
    , { pos = ( 53469, -21151 ), vel = ( -5, 2 ) }
    , { pos = ( -21098, 42723 ), vel = ( 2, -4 ) }
    , { pos = ( -42378, 32080 ), vel = ( 4, -3 ) }
    , { pos = ( -21074, 42726 ), vel = ( 2, -4 ) }
    , { pos = ( 10881, -10500 ), vel = ( -1, 1 ) }
    , { pos = ( 21514, 53367 ), vel = ( -2, -5 ) }
    , { pos = ( 42764, -53080 ), vel = ( -4, 5 ) }
    , { pos = ( 32151, 53368 ), vel = ( -3, -5 ) }
    , { pos = ( -21090, 21437 ), vel = ( 2, -2 ) }
    , { pos = ( 10877, -53078 ), vel = ( -1, 5 ) }
    , { pos = ( -10445, -31796 ), vel = ( 1, 3 ) }
    , { pos = ( 42796, 42722 ), vel = ( -4, -4 ) }
    , { pos = ( -31740, -21147 ), vel = ( 3, 2 ) }
    , { pos = ( -42344, -53083 ), vel = ( 4, 5 ) }
    , { pos = ( -21087, -31794 ), vel = ( 2, 3 ) }
    , { pos = ( -31691, 32082 ), vel = ( 3, -3 ) }
    , { pos = ( 21490, -31790 ), vel = ( -2, 3 ) }
    , { pos = ( 42792, -10506 ), vel = ( -4, 1 ) }
    , { pos = ( 53462, 32074 ), vel = ( -5, -3 ) }
    , { pos = ( -53025, 53366 ), vel = ( 5, -5 ) }
    , { pos = ( -31722, 21438 ), vel = ( 3, -2 ) }
    , { pos = ( 53469, 10786 ), vel = ( -5, -1 ) }
    , { pos = ( 10841, 32081 ), vel = ( -1, -3 ) }
    , { pos = ( 53449, 21431 ), vel = ( -5, -2 ) }
    , { pos = ( -21074, -10501 ), vel = ( 2, 1 ) }
    , { pos = ( 21487, -53084 ), vel = ( -2, 5 ) }
    , { pos = ( -53015, -53082 ), vel = ( 5, 5 ) }
    , { pos = ( 42777, -31787 ), vel = ( -4, 3 ) }
    , { pos = ( 42764, -10502 ), vel = ( -4, 1 ) }
    , { pos = ( 53422, 10787 ), vel = ( -5, -1 ) }
    , { pos = ( 42772, -10503 ), vel = ( -4, 1 ) }
    , { pos = ( 53465, 32081 ), vel = ( -5, -3 ) }
    , { pos = ( -10421, 10793 ), vel = ( 1, -1 ) }
    , { pos = ( 42815, -42432 ), vel = ( -4, 4 ) }
    , { pos = ( -21106, -53079 ), vel = ( 2, 5 ) }
    , { pos = ( -53033, 53369 ), vel = ( 5, -5 ) }
    , { pos = ( 42766, 53368 ), vel = ( -4, -5 ) }
    , { pos = ( 42767, -21151 ), vel = ( -4, 2 ) }
    , { pos = ( 21526, -31789 ), vel = ( -2, 3 ) }
    , { pos = ( -21082, -21148 ), vel = ( 2, 2 ) }
    , { pos = ( 53410, -21147 ), vel = ( -5, 2 ) }
    , { pos = ( -31691, -10500 ), vel = ( 3, 1 ) }
    , { pos = ( 42805, 32078 ), vel = ( -4, -3 ) }
    , { pos = ( -42378, -31790 ), vel = ( 4, 3 ) }
    , { pos = ( -21050, -21143 ), vel = ( 2, 2 ) }
    , { pos = ( -21106, -42436 ), vel = ( 2, 4 ) }
    , { pos = ( -53001, 53365 ), vel = ( 5, -5 ) }
    , { pos = ( 53469, -21145 ), vel = ( -5, 2 ) }
    , { pos = ( 10833, -31796 ), vel = ( -1, 3 ) }
    , { pos = ( -53009, -10498 ), vel = ( 5, 1 ) }
    , { pos = ( 32159, -31789 ), vel = ( -3, 3 ) }
    , { pos = ( -53007, -53077 ), vel = ( 5, 5 ) }
    , { pos = ( 21474, -21149 ), vel = ( -2, 2 ) }
    , { pos = ( 53427, -53083 ), vel = ( -5, 5 ) }
    , { pos = ( 10861, 42720 ), vel = ( -1, -4 ) }
    , { pos = ( -53017, 10786 ), vel = ( 5, -1 ) }
    , { pos = ( -42335, -42441 ), vel = ( 4, 4 ) }
    , { pos = ( -31751, -42432 ), vel = ( 3, 4 ) }
    , { pos = ( 32171, 21429 ), vel = ( -3, -2 ) }
    , { pos = ( -53016, -53086 ), vel = ( 5, 5 ) }
    , { pos = ( 21522, 21436 ), vel = ( -2, -2 ) }
    , { pos = ( -53009, -53079 ), vel = ( 5, 5 ) }
    , { pos = ( 53433, 42721 ), vel = ( -5, -4 ) }
    , { pos = ( 53420, -42441 ), vel = ( -5, 4 ) }
    , { pos = ( 42777, -10504 ), vel = ( -4, 1 ) }
    , { pos = ( -10416, 32074 ), vel = ( 1, -3 ) }
    , { pos = ( -42396, 10788 ), vel = ( 4, -1 ) }
    , { pos = ( 21490, 10791 ), vel = ( -2, -1 ) }
    , { pos = ( 32128, -21147 ), vel = ( -3, 2 ) }
    , { pos = ( 53461, -21143 ), vel = ( -5, 2 ) }
    , { pos = ( -21063, -21149 ), vel = ( 2, 2 ) }
    , { pos = ( -31743, -31792 ), vel = ( 3, 3 ) }
    , { pos = ( 21514, 21432 ), vel = ( -2, -2 ) }
    , { pos = ( 10861, 10791 ), vel = ( -1, -1 ) }
    , { pos = ( 10881, -31792 ), vel = ( -1, 3 ) }
    , { pos = ( 10889, -31795 ), vel = ( -1, 3 ) }
    , { pos = ( 32143, -21145 ), vel = ( -3, 2 ) }
    , { pos = ( -21093, -42439 ), vel = ( 2, 4 ) }
    , { pos = ( -10450, 21433 ), vel = ( 1, -2 ) }
    , { pos = ( -31719, 21430 ), vel = ( 3, -2 ) }
    , { pos = ( 10865, 53373 ), vel = ( -1, -5 ) }
    , { pos = ( -10405, -21144 ), vel = ( 1, 2 ) }
    , { pos = ( 21534, -42433 ), vel = ( -2, 4 ) }
    , { pos = ( -31727, -53083 ), vel = ( 3, 5 ) }
    , { pos = ( -53021, 21437 ), vel = ( 5, -2 ) }
    , { pos = ( 53449, -42434 ), vel = ( -5, 4 ) }
    , { pos = ( -21046, 10789 ), vel = ( 2, -1 ) }
    , { pos = ( 21518, 32082 ), vel = ( -2, -3 ) }
    , { pos = ( -31727, -10506 ), vel = ( 3, 1 ) }
    , { pos = ( -53033, 10793 ), vel = ( 5, -1 ) }
    , { pos = ( 10842, -10498 ), vel = ( -1, 1 ) }
    , { pos = ( 53425, 32075 ), vel = ( -5, -3 ) }
    , { pos = ( -53004, 32082 ), vel = ( 5, -3 ) }
    , { pos = ( -53016, -53082 ), vel = ( 5, 5 ) }
    , { pos = ( -21098, 21435 ), vel = ( 2, -2 ) }
    , { pos = ( -10417, 42720 ), vel = ( 1, -4 ) }
    , { pos = ( -42363, 21429 ), vel = ( 4, -2 ) }
    , { pos = ( 32151, 10790 ), vel = ( -3, -1 ) }
    , { pos = ( -53025, 21431 ), vel = ( 5, -2 ) }
    , { pos = ( -10437, -42435 ), vel = ( 1, 4 ) }
    , { pos = ( -31735, -10502 ), vel = ( 3, 1 ) }
    , { pos = ( -21098, -21149 ), vel = ( 2, 2 ) }
    , { pos = ( 10830, 10784 ), vel = ( -1, -1 ) }
    , { pos = ( 10838, -21151 ), vel = ( -1, 2 ) }
    , { pos = ( -21085, -21142 ), vel = ( 2, 2 ) }
    , { pos = ( 32119, -53079 ), vel = ( -3, 5 ) }
    , { pos = ( -31700, 21438 ), vel = ( 3, -2 ) }
    , { pos = ( -31714, -31795 ), vel = ( 3, 3 ) }
    , { pos = ( 32135, -10504 ), vel = ( -3, 1 ) }
    , { pos = ( 10889, -21149 ), vel = ( -1, 2 ) }
    , { pos = ( -31742, -53086 ), vel = ( 3, 5 ) }
    , { pos = ( -31726, -42432 ), vel = ( 3, 4 ) }
    , { pos = ( -31727, -53079 ), vel = ( 3, 5 ) }
    , { pos = ( -42369, -31792 ), vel = ( 4, 3 ) }
    , { pos = ( 10830, -42437 ), vel = ( -1, 4 ) }
    , { pos = ( 32179, 42725 ), vel = ( -3, -4 ) }
    , { pos = ( 21483, -31796 ), vel = ( -2, 3 ) }
    , { pos = ( 53435, 32074 ), vel = ( -5, -3 ) }
    , { pos = ( 32132, -31788 ), vel = ( -3, 3 ) }
    , { pos = ( 53449, 10787 ), vel = ( -5, -1 ) }
    , { pos = ( -21066, -42439 ), vel = ( 2, 4 ) }
    , { pos = ( -21065, 10789 ), vel = ( 2, -1 ) }
    , { pos = ( -31693, -31787 ), vel = ( 3, 3 ) }
    , { pos = ( -42380, 42727 ), vel = ( 4, -4 ) }
    , { pos = ( 42788, 10785 ), vel = ( -4, -1 ) }
    , { pos = ( -21054, -21150 ), vel = ( 2, 2 ) }
    , { pos = ( 42772, -42434 ), vel = ( -4, 4 ) }
    , { pos = ( -31722, 42719 ), vel = ( 3, -4 ) }
    , { pos = ( 10837, -31794 ), vel = ( -1, 3 ) }
    , { pos = ( 21498, -42436 ), vel = ( -2, 4 ) }
    , { pos = ( 42797, 10793 ), vel = ( -4, -1 ) }
    , { pos = ( -42372, 42724 ), vel = ( 4, -4 ) }
    , { pos = ( 32171, 10787 ), vel = ( -3, -1 ) }
    , { pos = ( -21085, -42441 ), vel = ( 2, 4 ) }
    , { pos = ( -21046, 10791 ), vel = ( 2, -1 ) }
    , { pos = ( -31709, 53370 ), vel = ( 3, -5 ) }
    , { pos = ( 21503, 21438 ), vel = ( -2, -2 ) }
    , { pos = ( -31695, -10498 ), vel = ( 3, 1 ) }
    , { pos = ( -10411, -53077 ), vel = ( 1, 5 ) }
    , { pos = ( 32169, 21438 ), vel = ( -3, -2 ) }
    , { pos = ( 53454, -42441 ), vel = ( -5, 4 ) }
    , { pos = ( 21487, 10785 ), vel = ( -2, -1 ) }
    , { pos = ( -31711, 10792 ), vel = ( 3, -1 ) }
    , { pos = ( -21085, -10497 ), vel = ( 2, 1 ) }
    , { pos = ( -42360, -42432 ), vel = ( 4, 4 ) }
    , { pos = ( -21090, 21434 ), vel = ( 2, -2 ) }
    , { pos = ( -31739, 32074 ), vel = ( 3, -3 ) }
    , { pos = ( 53417, -10502 ), vel = ( -5, 1 ) }
    , { pos = ( -42364, -21149 ), vel = ( 4, 2 ) }
    , { pos = ( 42767, -21147 ), vel = ( -4, 2 ) }
    , { pos = ( 21502, 10784 ), vel = ( -2, -1 ) }
    , { pos = ( -53016, -53077 ), vel = ( 5, 5 ) }
    , { pos = ( -10401, 10787 ), vel = ( 1, -1 ) }
    , { pos = ( -31715, 53373 ), vel = ( 3, -5 ) }
    , { pos = ( 10869, 53364 ), vel = ( -1, -5 ) }
    , { pos = ( 10848, -10499 ), vel = ( -1, 1 ) }
    , { pos = ( 10880, -10497 ), vel = ( -1, 1 ) }
    , { pos = ( -31749, 32074 ), vel = ( 3, -3 ) }
    , { pos = ( -42388, -31788 ), vel = ( 4, 3 ) }
    , { pos = ( -10440, 42719 ), vel = ( 1, -4 ) }
    , { pos = ( 10857, -53082 ), vel = ( -1, 5 ) }
    , { pos = ( 53461, -21147 ), vel = ( -5, 2 ) }
    , { pos = ( -21063, -10499 ), vel = ( 2, 1 ) }
    , { pos = ( 32124, 42728 ), vel = ( -3, -4 ) }
    , { pos = ( -21102, 10788 ), vel = ( 2, -1 ) }
    , { pos = ( 32179, 32079 ), vel = ( -3, -3 ) }
    , { pos = ( 10837, 32075 ), vel = ( -1, -3 ) }
    , { pos = ( -42383, 32077 ), vel = ( 4, -3 ) }
    , { pos = ( 42769, 21429 ), vel = ( -4, -2 ) }
    , { pos = ( 21495, -10497 ), vel = ( -2, 1 ) }
    , { pos = ( 32151, -42436 ), vel = ( -3, 4 ) }
    , { pos = ( 32129, 10784 ), vel = ( -3, -1 ) }
    , { pos = ( -31750, 10793 ), vel = ( 3, -1 ) }
    , { pos = ( 53449, -42432 ), vel = ( -5, 4 ) }
    , { pos = ( 32128, -42437 ), vel = ( -3, 4 ) }
    , { pos = ( -42377, -21144 ), vel = ( 4, 2 ) }
    , { pos = ( 10833, 53373 ), vel = ( -1, -5 ) }
    , { pos = ( 32143, 32083 ), vel = ( -3, -3 ) }
    , { pos = ( 42816, -31792 ), vel = ( -4, 3 ) }
    , { pos = ( -31727, 42728 ), vel = ( 3, -4 ) }
    , { pos = ( -21103, 42719 ), vel = ( 2, -4 ) }
    , { pos = ( -42368, 21438 ), vel = ( 4, -2 ) }
    , { pos = ( -21098, -10500 ), vel = ( 2, 1 ) }
    , { pos = ( -31735, -10506 ), vel = ( 3, 1 ) }
    , { pos = ( -42391, 32074 ), vel = ( 4, -3 ) }
    , { pos = ( -21095, -53081 ), vel = ( 2, 5 ) }
    , { pos = ( -21079, -21147 ), vel = ( 2, 2 ) }
    , { pos = ( 32151, 53366 ), vel = ( -3, -5 ) }
    , { pos = ( -42396, 10785 ), vel = ( 4, -1 ) }
    , { pos = ( 32119, 21434 ), vel = ( -3, -2 ) }
    , { pos = ( -31716, -42432 ), vel = ( 3, 4 ) }
    , { pos = ( 32162, 32081 ), vel = ( -3, -3 ) }
    , { pos = ( 10845, 21430 ), vel = ( -1, -2 ) }
    , { pos = ( 32119, -21149 ), vel = ( -3, 2 ) }
    , { pos = ( 53461, -10504 ), vel = ( -5, 1 ) }
    , { pos = ( -21074, -42438 ), vel = ( 2, 4 ) }
    , { pos = ( 10873, 53372 ), vel = ( -1, -5 ) }
    , { pos = ( -42380, -31792 ), vel = ( 4, 3 ) }
    , { pos = ( -31751, 10788 ), vel = ( 3, -1 ) }
    , { pos = ( -31735, -53086 ), vel = ( 3, 5 ) }
    , { pos = ( 21499, 10788 ), vel = ( -2, -1 ) }
    , { pos = ( 32147, 10784 ), vel = ( -3, -1 ) }
    , { pos = ( -10409, 42721 ), vel = ( 1, -4 ) }
    , { pos = ( 21492, -42435 ), vel = ( -2, 4 ) }
    , { pos = ( 10885, -42434 ), vel = ( -1, 4 ) }
    , { pos = ( 53409, -42433 ), vel = ( -5, 4 ) }
    , { pos = ( -31699, 10784 ), vel = ( 3, -1 ) }
    , { pos = ( 53459, -31787 ), vel = ( -5, 3 ) }
    , { pos = ( -21093, -42433 ), vel = ( 2, 4 ) }
    , { pos = ( 53436, -42432 ), vel = ( -5, 4 ) }
    , { pos = ( 42764, 42727 ), vel = ( -4, -4 ) }
    , { pos = ( -52982, 10793 ), vel = ( 5, -1 ) }
    , { pos = ( -21061, -53077 ), vel = ( 2, 5 ) }
    , { pos = ( 21525, 10784 ), vel = ( -2, -1 ) }
    , { pos = ( -10401, -53083 ), vel = ( 1, 5 ) }
    , { pos = ( 10833, 10793 ), vel = ( -1, -1 ) }
    , { pos = ( 42767, 10793 ), vel = ( -4, -1 ) }
    , { pos = ( 10869, 21429 ), vel = ( -1, -2 ) }
    , { pos = ( -42370, -53082 ), vel = ( 4, 5 ) }
    , { pos = ( 10881, 53372 ), vel = ( -1, -5 ) }
    , { pos = ( -10461, 21429 ), vel = ( 1, -2 ) }
    , { pos = ( -21080, -42432 ), vel = ( 2, 4 ) }
    , { pos = ( 42817, 10784 ), vel = ( -4, -1 ) }
    , { pos = ( 42805, -10501 ), vel = ( -4, 1 ) }
    , { pos = ( -42371, -31796 ), vel = ( 4, 3 ) }
    , { pos = ( -42388, 32079 ), vel = ( 4, -3 ) }
    , { pos = ( 21490, 21436 ), vel = ( -2, -2 ) }
    , { pos = ( -10421, 42726 ), vel = ( 1, -4 ) }
    , { pos = ( 21492, 42722 ), vel = ( -2, -4 ) }
    , { pos = ( 42799, 42728 ), vel = ( -4, -4 ) }
    , { pos = ( -53025, 10787 ), vel = ( 5, -1 ) }
    , { pos = ( 21474, -42441 ), vel = ( -2, 4 ) }
    , { pos = ( 53433, 10792 ), vel = ( -5, -1 ) }
    , { pos = ( 32130, -31796 ), vel = ( -3, 3 ) }
    , { pos = ( 53438, -10506 ), vel = ( -5, 1 ) }
    , { pos = ( -10445, -21146 ), vel = ( 1, 2 ) }
    , { pos = ( 21518, -10505 ), vel = ( -2, 1 ) }
    , { pos = ( 32151, 42722 ), vel = ( -3, -4 ) }
    , { pos = ( 21514, 42723 ), vel = ( -2, -4 ) }
    , { pos = ( -31739, 21433 ), vel = ( 3, -2 ) }
    , { pos = ( -21094, -10506 ), vel = ( 2, 1 ) }
    , { pos = ( 32129, -31792 ), vel = ( -3, 3 ) }
    , { pos = ( 42824, -53084 ), vel = ( -4, 5 ) }
    , { pos = ( -52999, -42435 ), vel = ( 5, 4 ) }
    , { pos = ( 53409, -31794 ), vel = ( -5, 3 ) }
    , { pos = ( -42353, -10504 ), vel = ( 4, 1 ) }
    , { pos = ( 32132, -42432 ), vel = ( -3, 4 ) }
    , { pos = ( 42780, -42436 ), vel = ( -4, 4 ) }
    , { pos = ( 10888, -31796 ), vel = ( -1, 3 ) }
    , { pos = ( -10421, -53077 ), vel = ( 1, 5 ) }
    , { pos = ( -53033, -21150 ), vel = ( 5, 2 ) }
    , { pos = ( 21494, -53085 ), vel = ( -2, 5 ) }
    , { pos = ( 21533, -10506 ), vel = ( -2, 1 ) }
    , { pos = ( 53421, 32080 ), vel = ( -5, -3 ) }
    , { pos = ( 32159, -10501 ), vel = ( -3, 1 ) }
    , { pos = ( 10881, 21436 ), vel = ( -1, -2 ) }
    , { pos = ( 21493, 32081 ), vel = ( -2, -3 ) }
    , { pos = ( -42363, 53364 ), vel = ( 4, -5 ) }
    , { pos = ( -52993, 42727 ), vel = ( 5, -4 ) }
    , { pos = ( 10841, -31792 ), vel = ( -1, 3 ) }
    , { pos = ( 10871, -53083 ), vel = ( -1, 5 ) }
    , { pos = ( -42379, 53369 ), vel = ( 4, -5 ) }
    , { pos = ( 21518, 21430 ), vel = ( -2, -2 ) }
    , { pos = ( -31727, 32078 ), vel = ( 3, -3 ) }
    , { pos = ( -42361, -31796 ), vel = ( 4, 3 ) }
    , { pos = ( 32123, -21142 ), vel = ( -3, 2 ) }
    , { pos = ( 10856, 10784 ), vel = ( -1, -1 ) }
    , { pos = ( 10853, 21430 ), vel = ( -1, -2 ) }
    , { pos = ( 21498, -31787 ), vel = ( -2, 3 ) }
    , { pos = ( -21098, -10506 ), vel = ( 2, 1 ) }
    , { pos = ( -42388, -10501 ), vel = ( 4, 1 ) }
    , { pos = ( -42395, -31796 ), vel = ( 4, 3 ) }
    , { pos = ( 10870, -42436 ), vel = ( -1, 4 ) }
    , { pos = ( 42824, -10501 ), vel = ( -4, 1 ) }
    , { pos = ( 42800, 10784 ), vel = ( -4, -1 ) }
    ]
