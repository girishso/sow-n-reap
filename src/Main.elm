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
    { gameState : GameState, thisPlayer : Player }


type alias GameState =
    { holes : List Hole, player1Seeds : Int, player2Seeds : Int, currentPlayer : Player }


type alias Hole =
    { seeds : List Seed, ix : Int, hilite : Bool }


type Player
    = PlayerOne
    | PlayerTwo


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
                            Hole (createSeeds 1) ix False

                        else
                            Hole (createSeeds n) ix False
                    )

        -- |> List.indexedMap
        --     (\ix hole ->
        --         if ix > 6 then
        --             { hole | mine = False }
        --
        --         else
        --             hole
        --     )
        gameState =
            { holes = holes, player1Seeds = 3, player2Seeds = 4, currentPlayer = PlayerOne }
    in
    ( { gameState = gameState, thisPlayer = PlayerOne }, Cmd.none )



---- UPDATE ----


type Msg
    = OnHoleClick Hole
    | HideDisappearingSeeds
    | StopHilite


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        gameState =
            model.gameState
    in
    case Debug.log "update" msg of
        StopHilite ->
            ( model, Cmd.none )

        OnHoleClick currentHole ->
            let
                stopHilitingHoles =
                    List.map (\hole -> { hole | hilite = False })

                markCurrentHoleDisappering =
                    List.indexedMap
                        (\ix hole ->
                            if ix == currentHole.ix then
                                { hole | seeds = List.map (\_ -> Disappearing) hole.seeds, hilite = True }

                            else
                                hole
                        )

                fillNextHolesRotateIfNecessary =
                    List.indexedMap
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

                newHoles =
                    if anyDisappearingSeeds gameState || isHoleEmpty currentHole then
                        gameState.holes

                    else
                        gameState.holes
                            -- stop highliting holes
                            |> stopHilitingHoles
                            -- current hole zero seeds, mark them Disappearing
                            |> markCurrentHoleDisappering
                            |> fillNextHolesRotateIfNecessary

                newGameState =
                    { gameState | holes = newHoles }
            in
            ( { model | gameState = newGameState }, Cmd.none )

        HideDisappearingSeeds ->
            let
                removeDisappearingSeeds =
                    List.map
                        (\hole ->
                            if List.any ((==) Disappearing) hole.seeds then
                                { hole | seeds = [] }

                            else
                                hole
                        )

                markAllSeedsNormal =
                    List.map (\hole -> { hole | seeds = List.map (\_ -> Normal) hole.seeds, hilite = False })

                newGameState =
                    { gameState
                        | holes =
                            gameState.holes
                                |> removeDisappearingSeeds
                                |> markAllSeedsNormal
                    }
            in
            ( { model | gameState = newGameState }, Cmd.none )


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
        gameState =
            model.gameState

        holesPerRow =
            nHoles // 2

        firstRow =
            List.take holesPerRow gameState.holes

        secondRow =
            List.drop holesPerRow gameState.holes |> List.reverse

        renderSeeds seeds =
            seeds
                |> List.map
                    (\seed -> div [ class "seed", class (seedStr seed) ] [])

        debugHole hole =
            ( hole.seeds |> List.map (seedStr >> String.slice 0 1) |> String.join "", hole.ix )
                |> Debug.toString

        renderHole hole =
            td []
                [ div [ class "hole-container" ]
                    [ div
                        [ classList [ ( "hole", True ), ( "hilite-hole", hole.hilite ) ]
                        , onClick (OnHoleClick hole)
                        , onMouseEnter StopHilite
                        ]
                        (div [ class "quiet" ]
                            [ debugHole hole |> text ]
                            :: renderSeeds hole.seeds
                        )
                    ]
                ]

        renderRow row =
            tr [] (List.map renderHole row)
    in
    div []
        [ h1 [] [ text "Sow n Reap" ]
        , renderCurrentPlayer gameState
        , renderPlayer gameState.player1Seeds
        , table [ id "mainTbl", HA.attribute "cellpadding" "10", HA.attribute "cellspacing" "10" ]
            [ renderRow secondRow
            , tr [ class "middle-line" ] [ td [ colspan holesPerRow ] [ hr [] [] ] ]
            , renderRow firstRow
            ]
        , renderPlayer gameState.player2Seeds
        ]


renderPlayer : Int -> Html Msg
renderPlayer n =
    div []
        (List.range 1 n |> List.map (\_ -> div [ class "seed appearing" ] []))


renderCurrentPlayer : GameState -> Html Msg
renderCurrentPlayer gameState =
    h3 []
        [ text <|
            "Current Player: "
                ++ Debug.toString gameState.currentPlayer
        ]


isHoleEmpty : Hole -> Bool
isHoleEmpty hole =
    List.isEmpty hole.seeds


anyDisappearingSeeds : GameState -> Bool
anyDisappearingSeeds gameState =
    gameState.holes
        |> List.map (\hole -> List.any ((==) Disappearing) hole.seeds)
        |> List.any ((==) True)


subscriptions : Model -> Sub Msg
subscriptions model =
    if anyDisappearingSeeds model.gameState then
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
