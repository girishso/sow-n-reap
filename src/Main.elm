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
    ( List.repeat nHoles 3, Cmd.none )



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
    in
    div [ style "text-align" "center" ]
        [ h1 [] [ text "Sow n Reap" ]
        , table
            [ id "mainTbl"
            , Html.Attributes.attribute "cellpadding" "10"
            , Html.Attributes.attribute "cellspacing" "10"
            ]
            [ tr [] (List.map (\hole -> td [ class "hole" ] [ text <| Debug.toString hole ]) firstRow)
            , tr [ class "middle-line" ] [ td [ colspan holesPerRow ] [ hr [] [] ] ]
            , tr [] (List.map (\hole -> td [ class "hole" ] [ text <| Debug.toString hole ]) secondRow)
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
