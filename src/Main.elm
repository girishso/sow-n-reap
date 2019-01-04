module Main exposing (Model, Msg(..), init, main, nHoles, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



---- MODEL ----


type alias Model =
    List Int


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
                            1

                        else
                            n
                    )
    in
    ( holes, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        holesPerRow =
            nHoles // 2

        firstRow =
            List.take holesPerRow model

        secondRow =
            List.drop holesPerRow model

        renderHole hole =
            div [ class "hole" ]
                [ div [ class "hole-i1" ]
                    (List.range 1 hole
                        |> List.map
                            (\x ->
                                div [ class "hole-inner" ]
                                    -- [ Char.fromCode 8226 |> String.fromChar |> text ]
                                    []
                            )
                    )
                ]

        renderRow row =
            tr []
                (List.map
                    (\hole ->
                        td []
                            [ renderHole hole
                            ]
                    )
                    row
                )
    in
    div [ style "text-align" "center" ]
        [ h1 [] [ text "Sow n Reap" ]
        , table
            [ id "mainTbl"
            , Html.Attributes.attribute "cellpadding" "10"
            , Html.Attributes.attribute "cellspacing" "10"
            ]
            [ renderRow firstRow
            , tr [ class "middle-line" ] [ td [ colspan holesPerRow ] [ hr [] [] ] ]
            , renderRow secondRow
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
