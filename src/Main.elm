module Main exposing (main)

import Browser
import Html exposing (Html, a, aside, button, div, i, input, li, nav, p, span, text, ul)
import Html.Attributes as Attr exposing (class)
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
    div [ Attr.class "hero is-fullheight is-clipped" ]
        [ div [ Attr.class "hero-body p-0 is-align-items-stretch" ]
            [ div [ Attr.class "columns is-gapless is-marginless", Attr.style "width" "100%" ]
                [ aside
                    [ Attr.class "column is-2"
                    ]
                    [ viewMenu model ]
                , div
                    [ Attr.class "column is-flex is-justify-content-center is-align-items-center"
                    ]
                    [ renderCanvas model ]
                ]
            ]
        , div [ Attr.class "hero-foot" ]
            [ viewBottomBar ]
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
            [ p [ Attr.class "menu-label" ] [ text "Furniture" ]
            , ul [ Attr.class "menu-list" ]
                [ li [] [ a [] [ text "Table" ] ]
                , li [] [ a [] [ text "Chair" ] ]
                , li [] [ a [] [ text "Carpet" ] ]
                ]
            ]
        ]


viewBottomBar : Html Msg
viewBottomBar =
    nav [ Attr.class "navbar is-fixed-bottom is-dark" ]
        [ div [ Attr.class "container is-flex is-justify-content-center" ]
            [ div [ Attr.class "navbar-brand is-flex is-align-items-center" ]
                [ i [ Attr.class "fa-solid fa-ruler-combined fa-lg mr-3" ] []
                , a [ Attr.class "navbar-item has-text-weight-bold box m-0 is-shadowless" ] [ text "3x3" ]
                , a [ Attr.class "navbar-item has-text-weight-bold box m-0 is-shadowless" ] [ text "3x4" ]
                , a [ Attr.class "navbar-item has-text-weight-bold box m-0 is-shadowless" ] [ text "5x6" ]
                , a [ Attr.class "navbar-item has-text-weight-bold box m-0 is-shadowless mr-3" ] [ text "6x6" ]
                , text "Custom size"
                , viewSquareInput
                , text "x"
                , viewSquareInput
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


viewSquareInput =
    input
        [ Attr.class "mx-3"
        , Attr.style "width" "40px"
        , Attr.style "height" "25px"
        , Attr.style "text-align" "center"
        , Attr.style "border" "2px solid #555"
        , Attr.style "border-radius" "8px"
        , Attr.style "background" "none"
        , Attr.style "color" "white"
        , Attr.style "outline" "none"
        ]
        []



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
