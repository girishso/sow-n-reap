module Main exposing (Model, Msg(..), init, main, nHoles, update, view)

import Browser
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events as HE exposing (..)
import Json.Encode as Encode
import List.Extra
import Time



---- MODEL ----


type alias Model =
    { holes : List Hole }


type alias Hole =
    { seeds : List Seed, mine : Bool, ix : Int, hilite : Bool }


type Seed
    = Disappearing
    | Appearing
    | Normal


createSeeds : Int -> List Seed
createSeeds n =
    List.repeat n Normal


nHoles =
    14


init : ( Model, Cmd Msg )
init =
    let
        holes =
            List.repeat nHoles 3
                |> List.indexedMap
                    (\ix n ->
                        if ix == 3 || ix == 10 then
                            Hole (createSeeds 1) True ix False

                        else
                            Hole (createSeeds n) True ix False
                    )
                |> List.indexedMap
                    (\ix hole ->
                        if ix > 6 then
                            { hole | mine = False }

                        else
                            hole
                    )
    in
    ( { holes = holes }, Cmd.none )



---- UPDATE ----


type Msg
    = OnHoleClick Hole
    | HideDisappearingSeeds


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update" msg of
        OnHoleClick currentHole ->
            let
                holes =
                    model.holes
                        -- stop highliting holes
                        |> List.map (\hole -> { hole | hilite = False })
                        -- current hole zero seeds, make them Disappearing
                        |> List.indexedMap
                            (\ix hole ->
                                if ix == currentHole.ix then
                                    { hole | seeds = List.map (\_ -> Disappearing) hole.seeds, hilite = True }

                                else
                                    hole
                            )
                        |> List.indexedMap
                            (\ix hole ->
                                let
                                    nholesToFill =
                                        currentHole.ix + List.length currentHole.seeds
                                in
                                if ix <= nholesToFill && ix > currentHole.ix then
                                    -- next (n seeds) holes, add one seed each
                                    { hole | seeds = hole.seeds ++ [ Appearing ], hilite = True }

                                else if nholesToFill >= nHoles && ix <= (nholesToFill - nHoles) then
                                    -- (rotate) fill n initial holes if currentHole spills over
                                    { hole | seeds = hole.seeds ++ [ Appearing ], hilite = True }

                                else
                                    hole
                            )
            in
            ( { model | holes = holes }, Cmd.none )

        HideDisappearingSeeds ->
            let
                newHoles =
                    model.holes
                        |> List.map
                            (\hole ->
                                if List.any ((==) Disappearing) hole.seeds then
                                    { hole | seeds = [] }

                                else
                                    hole
                            )
                        |> List.map (\hole -> { hole | seeds = List.map (\_ -> Normal) hole.seeds })
            in
            ( { model | holes = newHoles }, Cmd.none )


seedEncoder : Seed -> Encode.Value
seedEncoder v =
    case v of
        Disappearing ->
            Encode.string "Disappearing"

        Appearing ->
            Encode.string "Appearing"

        Normal ->
            Encode.string "Normal"


seedStr : Seed -> String
seedStr seed =
    case seed of
        Disappearing ->
            "disappearing"

        Appearing ->
            "appearing"

        Normal ->
            "normal"



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        holesPerRow =
            nHoles // 2

        firstRow =
            List.take holesPerRow model.holes

        secondRow =
            List.drop holesPerRow model.holes |> List.reverse

        renderSeeds seeds =
            seeds
                |> List.map
                    (\seed -> div [ class "seed", class (seedStr seed) ] [])

        printHole hole =
            ( hole.seeds |> List.map (seedStr >> String.slice 0 1) |> String.join "", hole.mine, hole.ix )
                |> Debug.toString

        renderHole hole =
            td []
                [ div [ class "hole-container" ]
                    [ div [ classList [ ( "hole", True ), ( "hilite-hole", hole.hilite ) ], onClick (OnHoleClick hole) ]
                        (div [ class "quiet" ]
                            [ printHole hole |> text ]
                            :: renderSeeds hole.seeds
                        )
                    ]
                ]

        renderRow row =
            tr [] (List.map renderHole row)
    in
    div [ style "text-align" "center" ]
        [ h1 [] [ text "Sow n Reap" ]
        , table [ id "mainTbl", HA.attribute "cellpadding" "10", HA.attribute "cellspacing" "10" ]
            [ renderRow secondRow
            , tr [ class "middle-line" ] [ td [ colspan holesPerRow ] [ hr [] [] ] ]
            , renderRow firstRow
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        anyDisappearingSeeds =
            model.holes
                |> List.map (\hole -> List.any (\seed -> seed == Disappearing) hole.seeds)
                |> List.any ((==) True)
    in
    if anyDisappearingSeeds then
        Time.every 3000 (always HideDisappearingSeeds)

    else
        Sub.none



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions

        -- , subscriptions = always Sub.none
        }
