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
    { holes : List Hole, player1Seeds : Int, player2Seeds : Int, gameState : GameState, thisPlayer : Player }


type GameState
    = Playing Player
    | StartTurn Player
    | Collect Int
    | Finished


type alias Hole =
    { seeds : List Seed, ix : Int, hilite : Bool, owner : Player, nextTurn : Bool }


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
                            Hole (createSeeds 1) ix False PlayerOne True

                        else
                            Hole (createSeeds n) ix False PlayerOne True
                    )
                |> List.indexedMap
                    (\ix hole ->
                        if ix > 6 then
                            { hole | owner = PlayerTwo, nextTurn = False }

                        else
                            hole
                    )
    in
    ( { holes = holes, player1Seeds = 0, player2Seeds = 0, gameState = StartTurn PlayerOne, thisPlayer = PlayerOne }, Cmd.none )



---- UPDATE ----


type Msg
    = OnHoleClick Hole
    | HideDisappearingSeeds


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update" msg of
        OnHoleClick currentHole ->
            let
                modelWithNewHoles =
                    { model | holes = handleHoleClick currentHole model }
            in
            ( { modelWithNewHoles | gameState = calculateNextGameState modelWithNewHoles }, Cmd.none )

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
            in
            ( { model
                | holes =
                    model.holes
                        |> removeDisappearingSeeds
                        |> markAllSeedsNormal
              }
            , Cmd.none
            )


calculateNextGameState : Model -> GameState
calculateNextGameState model =
    -- detect collect mode
    let
        nextHoleHasSeeds ix =
            List.Extra.getAt (ix + 1) model.holes
                |> Maybe.map
                    (\hole ->
                        if List.isEmpty hole.seeds then
                            -1

                        else
                            ix + 1
                    )
                |> Debug.log "nextHoleHasSeeds"
                |> Maybe.withDefault -1

        isNextHoleEmpty =
            model.holes
                |> List.Extra.findIndex .nextTurn
                |> Debug.log "ix"
                |> Maybe.map (\ix -> nextHoleHasSeeds ix)
                |> Maybe.withDefault -1

        nextIx =
            isNextHoleEmpty

        -- TODO: rotate if necessary
    in
    if nextIx /= -1 then
        let
            _ =
                Debug.log "isNextHoleEmpty" True
        in
        -- Collect mode
        Collect nextIx

    else
        model.gameState


handleHoleClick : Hole -> Model -> List Hole
handleHoleClick currentHole model =
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
                    if ix <= nholesToFill currentHole && ix > currentHole.ix then
                        -- next (n seeds) holes, add one seed each
                        { hole | seeds = hole.seeds ++ [ Appearing ], hilite = True }

                    else if nholesToFill currentHole >= nHoles && ix <= (nholesToFill currentHole - nHoles) then
                        -- (rotate) fill n initial holes if currentHole spills over
                        { hole | seeds = hole.seeds ++ [ Appearing ], hilite = True }

                    else
                        hole
                )

        markNextPlayableHole =
            List.indexedMap
                (\ix hole ->
                    let
                        nextHole =
                            nholesToFill currentHole + 1
                    in
                    if ix == nextHole then
                        { hole | nextTurn = True }

                    else if nextHole > nHoles && ix == nextHole - nHoles then
                        { hole | nextTurn = True }

                    else
                        { hole | nextTurn = False }
                )
    in
    -- if anyDisappearingSeeds model || isHoleEmpty currentHole || isNotCurrentPlayersTurnOrHole currentHole model then
    if anyDisappearingSeeds model || isHoleEmpty currentHole || currentHole.nextTurn == False then
        model.holes

    else
        model.holes
            -- stop highliting holes
            |> stopHilitingHoles
            -- current hole zero seeds, mark them Disappearing
            |> markCurrentHoleDisappering
            |> fillNextHolesRotateIfNecessary
            |> markNextPlayableHole


nholesToFill : Hole -> Int
nholesToFill currentHole =
    currentHole.ix + List.length currentHole.seeds



-- nextGameState : GameModel -> GameState
-- nextGameState model =
--     case model.gameState of
--         Playing player ->
--
--
--         StartTurn player ->
--
--
--         Collect ->
--
--
--         Finished ->
-- nextActiveHole =
--     if ix <= nholesToFill currentHole && ix > currentHole.ix then
--         -- next (n seeds) holes, add one seed each
--         ix + 1
--
--     else if nholesToFill currentHole >= nHoles && ix <= (nholesToFill currentHole - nHoles) then
--         -- (rotate) fill n initial holes if currentHole spills over
--         ix + 1
--
--     else
--         hole
--


isNotCurrentPlayersTurnOrHole : Hole -> GameState -> Bool
isNotCurrentPlayersTurnOrHole currentHole gameState =
    case gameState of
        Playing player ->
            False

        StartTurn player ->
            currentHole.owner /= player

        Collect ix ->
            False

        Finished ->
            False



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ h1 [] [ text "Sow n Reap" ]
        , h3 [] [ model.gameState |> Debug.toString |> text ]
        , renderCurrentPlayer model.gameState
        , div [ class "board-container" ]
            [ renderPlayer model.player1Seeds "player1"
            , renderBoard model
            , renderPlayer model.player2Seeds "player2"
            ]
        ]


renderBoard : Model -> Html Msg
renderBoard model =
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

        debugHole hole =
            -- ( hole.seeds |> List.map (seedStr >> String.slice 0 1) |> String.join "", hole.ix )
            ( hole.ix, hole.nextTurn )
                |> Debug.toString

        renderHole hole =
            td []
                [ div [ class "hole-container" ]
                    [ div
                        [ classList [ ( "hole", True ), ( "hilite-hole", hole.hilite ), ( "next-turn", hole.nextTurn ) ]
                        , onClick (OnHoleClick hole)
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
    table [ id "mainTbl", class "flex-item", HA.attribute "cellpadding" "10", HA.attribute "cellspacing" "10" ]
        [ renderRow secondRow
        , tr [ class "middle-line" ] [ td [ colspan holesPerRow ] [ hr [ class "middle" ] [] ] ]
        , renderRow firstRow
        ]


renderPlayer : Int -> String -> Html Msg
renderPlayer n whichPlayer =
    div [ class "flex-item player", class whichPlayer ]
        (List.range 1 n |> List.map (\_ -> div [ class "seed appearing" ] []))


renderCurrentPlayer : GameState -> Html Msg
renderCurrentPlayer gameState =
    h3 []
        [ text <|
            "Current Player: "
                ++ Debug.toString gameState
        ]


isHoleEmpty : Hole -> Bool
isHoleEmpty hole =
    List.isEmpty hole.seeds


anyDisappearingSeeds : Model -> Bool
anyDisappearingSeeds model =
    model.holes
        |> List.map (\hole -> List.any ((==) Disappearing) hole.seeds)
        |> List.any ((==) True)


subscriptions : Model -> Sub Msg
subscriptions model =
    if anyDisappearingSeeds model then
        Time.every 1000 (always HideDisappearingSeeds)

    else
        Sub.none


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
