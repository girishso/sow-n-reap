module Main exposing (Model, Msg(..), init, main, nHoles, update, view)

import Browser
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events as HE exposing (..)



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

        renderSeeds n =
            List.range 1 n
                |> List.map
                    (\x -> div [ class "seed" ] [])

        renderHole hole =
            td []
                [ div [ class "hole" ]
                    [ div [ class "hole-i1" ] (renderSeeds hole) ]
                ]

        renderRow row =
            tr [] (List.map renderHole row)
    in
    div [ style "text-align" "center" ]
        [ h1 [] [ text "Sow n Reap" ]
        , table
            [ id "mainTbl"
            , HA.attribute "cellpadding" "10"
            , HA.attribute "cellspacing" "10"
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
