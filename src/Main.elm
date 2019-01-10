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
    { gameModel : GameModel, thisPlayer : Player }


type alias GameModel =
    { holes : List Hole, player1Seeds : Int, player2Seeds : Int, gameState : GameState }


type GameState
    = Playing Player
    | StartTurn Player
    | Collect
    | Finished


type alias Hole =
    { seeds : List Seed, ix : Int, hilite : Bool, owner : Player }


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
                            Hole (createSeeds 1) ix False PlayerOne

                        else
                            Hole (createSeeds n) ix False PlayerOne
                    )
                |> List.indexedMap
                    (\ix hole ->
                        if ix > 6 then
                            { hole | owner = PlayerTwo }

                        else
                            hole
                    )

        gameModel =
            { holes = holes, player1Seeds = 3, player2Seeds = 23, gameState = StartTurn PlayerOne }
    in
    ( { gameModel = gameModel, thisPlayer = PlayerOne }, Cmd.none )



---- UPDATE ----


type Msg
    = OnHoleClick Hole
    | HideDisappearingSeeds
    | StopHilite


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        gameModel =
            model.gameModel
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
                            if ix <= nholesToFill currentHole && ix > currentHole.ix then
                                -- next (n seeds) holes, add one seed each
                                { hole | seeds = hole.seeds ++ [ Appearing ], hilite = True }

                            else if nholesToFill currentHole >= nHoles && ix <= (nholesToFill currentHole - nHoles) then
                                -- (rotate) fill n initial holes if currentHole spills over
                                { hole | seeds = hole.seeds ++ [ Appearing ], hilite = True }

                            else
                                hole
                        )

                newHoles =
                    if anyDisappearingSeeds gameModel || isHoleEmpty currentHole || isNotCurrentPlayersTurnOrHole currentHole gameModel then
                        gameModel.holes

                    else
                        gameModel.holes
                            -- stop highliting holes
                            |> stopHilitingHoles
                            -- current hole zero seeds, mark them Disappearing
                            |> markCurrentHoleDisappering
                            |> fillNextHolesRotateIfNecessary

                newGameModel =
                    { gameModel | holes = newHoles }
            in
            ( { model | gameModel = newGameModel }, Cmd.none )

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

                newGameModel =
                    { gameModel
                        | holes =
                            gameModel.holes
                                |> removeDisappearingSeeds
                                |> markAllSeedsNormal
                    }
            in
            ( { model | gameModel = newGameModel }, Cmd.none )


nholesToFill : Hole -> Int
nholesToFill currentHole =
    currentHole.ix + List.length currentHole.seeds



-- nextGameState : GameModel -> GameState
-- nextGameState gameModel =
--     case gameModel.gameState of
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


isNotCurrentPlayersTurnOrHole : Hole -> GameModel -> Bool
isNotCurrentPlayersTurnOrHole currentHole gameModel =
    let
        gameState =
            gameModel.gameState
    in
    case gameState of
        Playing player ->
            False

        StartTurn player ->
            currentHole.owner /= player

        Collect ->
            False

        Finished ->
            False



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        gameModel =
            model.gameModel
    in
    div [ class "main" ]
        [ h1 [] [ text "Sow n Reap" ]
        , renderCurrentPlayer gameModel
        , div [ class "board-container" ]
            [ renderPlayer gameModel.player1Seeds "player1"
            , renderBoard gameModel
            , renderPlayer gameModel.player2Seeds "player2"
            ]
        ]


renderBoard : GameModel -> Html Msg
renderBoard gameModel =
    let
        holesPerRow =
            nHoles // 2

        firstRow =
            List.take holesPerRow gameModel.holes

        secondRow =
            List.drop holesPerRow gameModel.holes |> List.reverse

        renderSeeds seeds =
            seeds
                |> List.map
                    (\seed -> div [ class "seed", class (seedStr seed) ] [])

        debugHole hole =
            ""

        -- ( hole.seeds |> List.map (seedStr >> String.slice 0 1) |> String.join "", hole.ix )
        -- |> Debug.toString
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
    table [ id "mainTbl", class "flex-item", HA.attribute "cellpadding" "10", HA.attribute "cellspacing" "10" ]
        [ renderRow secondRow
        , tr [ class "middle-line" ] [ td [ colspan holesPerRow ] [ hr [ class "middle" ] [] ] ]
        , renderRow firstRow
        ]


renderPlayer : Int -> String -> Html Msg
renderPlayer n whichPlayer =
    div [ class "flex-item player", class whichPlayer ]
        (List.range 1 n |> List.map (\_ -> div [ class "seed appearing" ] []))


renderCurrentPlayer : GameModel -> Html Msg
renderCurrentPlayer gameModel =
    h3 []
        [ text <|
            "Current Player: "
                ++ Debug.toString gameModel.gameState
        ]


isHoleEmpty : Hole -> Bool
isHoleEmpty hole =
    List.isEmpty hole.seeds


anyDisappearingSeeds : GameModel -> Bool
anyDisappearingSeeds gameModel =
    gameModel.holes
        |> List.map (\hole -> List.any ((==) Disappearing) hole.seeds)
        |> List.any ((==) True)


subscriptions : Model -> Sub Msg
subscriptions model =
    if anyDisappearingSeeds model.gameModel then
        Time.every 2000 (always HideDisappearingSeeds)

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
