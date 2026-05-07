module Main exposing (main)

import Browser
import Html exposing (Html, a, aside, button, div, i, li, p, span, text, ul)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Svg exposing (Svg, rect, svg)
import Svg.Attributes as SvgAttr



-- 1. MODEL


type alias Model =
    { items : List String
    , isOpenMenu : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { items = [ "Tisch", "Stuhl", "Schrank" ]
      , isOpenMenu = True
      }
    , Cmd.none
    )



-- 2. UPDATE


type Msg
    = ToggleMenu


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleMenu ->
            ( { model | isOpenMenu = not model.isOpenMenu }, Cmd.none )



-- 3. VIEW


view : Model -> Html Msg
view model =
    div [ Attr.class "hero is-fullheight" ]
        [ div [ Attr.class "columns is-gapless is-marginless" ]
            [ aside [ Attr.class "column is-2", Attr.style "min-height" "100vh" ]
                [ viewMenu model ]
            , div [ Attr.class "column is-flex is-justify-content-center is-align-items-center", Attr.style "min-height" "100vh" ]
                [ renderCanvas model ]
            ]
        ]


viewMenu : Model -> Html Msg
viewMenu model =
    let
        animationClass =
            if model.isOpenMenu then
                "animate__slideInLeft"

            else
                "animate__slideOutLeft"

        iconClass =
            if model.isOpenMenu then
                "fa-xmark animate__rotateInDownLeft"

            else
                "fa-bars animate__rotateInDownRight"
    in
    div []
        [ button
            [ Attr.class "button is-light m-2"
            , onClick ToggleMenu
            ]
            [ span [ Attr.class "icon" ]
                [ i [ Attr.class ("fa-solid animate__animated " ++ iconClass) ] [] ]
            ]
        , aside
            [ Attr.class ("menu p-4 animate__animated " ++ animationClass) ]
            [ p [ Attr.class "menu-label" ] [ text "Label 1" ]
            , ul [ Attr.class "menu-list" ]
                [ li [] [ a [] [ text "Link 1" ] ]
                , li [] [ a [] [ text "Link 2" ] ]
                ]
            ]
        ]


renderCanvas : Model -> Html Msg
renderCanvas model =
    svg
        [ SvgAttr.width "600"
        , SvgAttr.height "400"
        , SvgAttr.viewBox "0 0 600 400"
        , Attr.style "border" "2px solid black"
        , Attr.style "display" "block"
        ]
        [ rect
            [ SvgAttr.x "50"
            , SvgAttr.y "50"
            , SvgAttr.width "100"
            , SvgAttr.height "60"
            , SvgAttr.fill "cornflowerblue"
            ]
            []
        ]



-- 4. SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- 5. MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
