module Main exposing (main)

import Browser
import Dict exposing (Dict, toList)
import Html exposing (Attribute, Html, a, aside, button, div, form, i, img, input, li, nav, p, span, text, ul)
import Html.Attributes as Attr exposing (width)
import Html.Events exposing (on, onClick, onSubmit)
import Json.Decode as Decode
import Platform.Cmd as Cmd
import Svg exposing (svg)
import Svg.Attributes as SvgAttr



-- 1. MODEL


type alias CanvasSize =
    { width : Float
    , height : Float
    }


type alias RoomItem =
    { name : String
    , imgSrc : String
    , width : Float
    , height : Float
    , allowedOn : List String
    , layer : Int
    }


type alias Position =
    ( Int, Int )


type alias Grid =
    { active : Bool
    , items : Dict Position RoomItem
    }


type PlacementState
    = Idle
    | HoldingItem RoomItem


type alias Model =
    { itemsFurniture : List RoomItem
    , itemsUtilities : List RoomItem
    , itemsDecor : List RoomItem
    , isOpenMenu : Bool
    , canvasSize : CanvasSize
    , canvasGrid : Grid
    , customInputW : String
    , customInputH : String
    , isOpenToaster : Bool
    , toasterMsg : String
    , floorType : String
    , mousePosition : Position
    , placement : PlacementState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { itemsFurniture =
            [ { name = "Bed", imgSrc = "src/img/bedFurniture.png", width = 140, height = 200, allowedOn = [ "Carpet" ], layer = 3 }
            , { name = "Chair", imgSrc = "src/img/chairFurniture.png", width = 50, height = 50, allowedOn = [ "Carpet" ], layer = 2 }
            , { name = "Table", imgSrc = "src/img/tableFurniture.png", width = 140, height = 80, allowedOn = [ "Carpet" ], layer = 1 }
            ]
      , itemsUtilities =
            [ { name = "Desktop", imgSrc = "src/img/desktopUtilities.png", width = 120, height = 80, allowedOn = [ "Carpet" ], layer = 3 }
            , { name = "Lamp", imgSrc = "src/img/lampUtilities.png", width = 40, height = 40, allowedOn = [ "Carpet", "Table", "Chair" ], layer = 3 }
            , { name = "TV", imgSrc = "src/img/tvUtilities.png", width = 120, height = 50, allowedOn = [ "Carpet" ], layer = 3 }
            ]
      , itemsDecor =
            [ { name = "Carpet", imgSrc = "src/img/carpetDecor.png", width = 230, height = 160, allowedOn = [ "Carpet" ], layer = 0 }
            , { name = "Plant", imgSrc = "src/img/plantDecor.png", width = 50, height = 50, allowedOn = [ "Carpet", "Table", "Chair" ], layer = 3 }
            ]
      , isOpenMenu = True
      , canvasSize = { width = 400, height = 300 }
      , canvasGrid = { active = False, items = Dict.empty }
      , customInputW = ""
      , customInputH = ""
      , isOpenToaster = False
      , toasterMsg = ""
      , floorType = "src/img/laminateFloor.jpg"
      , mousePosition = ( 0, 0 )
      , placement = Idle
      }
    , Cmd.none
    )



-- 2. UPDATE


type Msg
    = ToggleMenu
    | ResizeCanvas Float Float
    | SetCustomInputW String
    | SetCustomInputH String
    | OpenToaster String
    | HideToaster
    | SetFloorType String
    | SelectItem RoomItem
    | ClickCanvas Position
    | MouseMoved Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleMenu ->
            ( { model | isOpenMenu = not model.isOpenMenu }, Cmd.none )

        ResizeCanvas w h ->
            let
                old =
                    model.canvasSize

                newCanvas =
                    { old | width = w * 100, height = h * 100 }
            in
            ( { model | canvasSize = newCanvas, isOpenToaster = False }, Cmd.none )

        SetCustomInputW value ->
            ( { model | customInputW = value, isOpenToaster = False }, Cmd.none )

        SetCustomInputH value ->
            ( { model | customInputH = value, isOpenToaster = False }, Cmd.none )

        OpenToaster message ->
            ( { model | isOpenToaster = True, toasterMsg = message }, Cmd.none )

        HideToaster ->
            ( { model | isOpenToaster = False }, Cmd.none )

        SetFloorType floor ->
            ( { model | floorType = floor }, Cmd.none )

        SelectItem item ->
            let
                oldGrid =
                    model.canvasGrid

                newGrid =
                    { oldGrid | active = True }
            in
            ( { model | canvasGrid = newGrid, placement = HoldingItem item }, Cmd.none )

        ClickCanvas position ->
            case model.placement of
                Idle ->
                    ( model, Cmd.none )

                HoldingItem item ->
                    let
                        clearedModel =
                            { model | isOpenToaster = False, toasterMsg = "" }
                    in
                    case checkPlacement model position item model.canvasGrid of
                        Err toasterMsg ->
                            update toasterMsg model

                        Ok newGrid ->
                            ( { clearedModel | placement = Idle, canvasGrid = newGrid }, Cmd.none )

        MouseMoved pos ->
            ( { model | mousePosition = pos }, Cmd.none )


submitCustomSize : String -> String -> Msg
submitCustomSize w h =
    case ( String.toFloat w, String.toFloat h ) of
        ( Just width, Just height ) ->
            if width > 11 then
                OpenToaster "Width must be 11m or less"

            else if height > 6.5 then
                OpenToaster "Height must be 6.5m or less"

            else
                ResizeCanvas width height

        _ ->
            OpenToaster "Inputs should be of type number"


checkPlacement : Model -> Position -> RoomItem -> Grid -> Result Msg Grid
checkPlacement model position item oldGrid =
    let
        ( x, y ) =
            position

        posCheckX =
            toFloat x * 10 + item.width

        posCheckY =
            toFloat y * 10 + item.height

        nx1 =
            toFloat x * 10

        nx2 =
            nx1 + item.width

        ny1 =
            toFloat y * 10

        ny2 =
            ny1 + item.height

        checkCollision ( ( oldx, oldy ), oldItem ) =
            let
                ox1 =
                    toFloat oldx * 10

                ox2 =
                    ox1 + oldItem.width

                oy1 =
                    toFloat oldy * 10

                oy2 =
                    oy1 + oldItem.height
            in
            if (nx2 <= ox1) || (nx1 >= ox2) || (ny2 <= oy1) || (ny1 >= oy2) then
                True

            else if List.member oldItem.name item.allowedOn then
                True

            else
                False

        checkAllCollisions =
            oldGrid.items |> Dict.toList |> List.all checkCollision
    in
    if posCheckX > model.canvasSize.width || posCheckY > model.canvasSize.height then
        Err (OpenToaster (item.name ++ " can't be placed there"))

    else if Dict.member position oldGrid.items then
        Err (OpenToaster "This tile position is already taken")

    else if not checkAllCollisions then
        Err (OpenToaster "This position is already taken by another item")

    else
        let
            newItems =
                Dict.insert position item oldGrid.items
        in
        Ok { oldGrid | active = False, items = newItems }


mousePositionDecoder : Decode.Decoder Position
mousePositionDecoder =
    Decode.map2 (\x y -> ( floor (x / 10), floor (y / 10) ))
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)



-- 3. VIEW


view : Model -> Html Msg
view model =
    div
        [ Attr.class "hero is-fullheight is-clipped"
        ]
        [ if model.isOpenToaster then
            viewToast model.toasterMsg

          else
            text ""
        , div [ Attr.class "hero-body p-0 is-relative" ]
            [ div [ Attr.style "position" "absolute", Attr.style "z-index" "10", Attr.style "top" "0", Attr.style "left" "0" ]
                [ viewMenu model ]
            , div [ Attr.class "is-flex is-justify-content-center is-align-items-center", Attr.style "width" "100%", Attr.style "height" "100%" ]
                [ renderCanvas model ]
            , div [ Attr.style "position" "absolute", Attr.style "z-index" "10", Attr.style "top" "0", Attr.style "right" "0" ]
                [ viewRoomSettings model ]
            ]
        , div [ Attr.class "hero-foot" ]
            [ viewBottomBar model ]
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
                (List.map (\item -> li [] [ a [ Attr.class "is-flex is-justify-content-space-between is-align-items-center", onClick (SelectItem item) ] [ text item.name, span [ Attr.class "is-flex is-align-items-center is-justify-content-center ml-3", Attr.style "width" "35px", Attr.style "height" "35px" ] [ img [ Attr.src item.imgSrc, Attr.style "border" "1.7px solid #363636", Attr.style "border-radius" "4px", Attr.style "box-shadow" "0 2px 4px rgba(0,0,0,1)", Attr.style "width" "100%", Attr.style "height" "100%", Attr.style "object-fit" "contain" ] [] ] ] ]) model.itemsFurniture)
            , p [ Attr.class "menu-label" ] [ text "Utilities" ]
            , ul [ Attr.class "menu-list" ]
                (List.map (\item -> li [] [ a [ Attr.class "is-flex is-justify-content-space-between is-align-items-center", onClick (SelectItem item) ] [ text item.name, span [ Attr.class "is-flex is-align-items-center is-justify-content-center ml-3", Attr.style "width" "35px", Attr.style "height" "35px" ] [ img [ Attr.src item.imgSrc, Attr.style "border" "1.7px solid #363636", Attr.style "border-radius" "4px", Attr.style "box-shadow" "0 2px 4px rgba(0,0,0,1)", Attr.style "width" "100%", Attr.style "height" "100%", Attr.style "object-fit" "contain" ] [] ] ] ]) model.itemsUtilities)
            , p [ Attr.class "menu-label" ] [ text "Decor" ]
            , ul [ Attr.class "menu-list" ]
                (List.map (\item -> li [] [ a [ Attr.class "is-flex is-justify-content-space-between is-align-items-center", onClick (SelectItem item) ] [ text item.name, span [ Attr.class "is-flex is-align-items-center is-justify-content-center ml-3", Attr.style "width" "35px", Attr.style "height" "35px" ] [ img [ Attr.src item.imgSrc, Attr.style "border" "1.7px solid #363636", Attr.style "border-radius" "4px", Attr.style "box-shadow" "0 2px 4px rgba(0,0,0,1)", Attr.style "width" "100%", Attr.style "height" "100%", Attr.style "object-fit" "contain" ] [] ] ] ]) model.itemsDecor)
            ]
        ]


viewRoomSettings : Model -> Html Msg
viewRoomSettings model =
    aside
        [ Attr.class "menu p-4" ]
        [ p [ Attr.class "menu-label has-text-centered" ] [ text "Floor" ]
        , ul [ Attr.class "menu-list" ]
            [ li []
                [ a (floorBtnAttrs model "src/img/graniteFloor.jpg")
                    [ span [ Attr.class "icon mr-2" ]
                        [ img [ Attr.src "src/img/graniteFloor.jpg", Attr.style "border" "1px solid #dbdbdb" ] [] ]
                    , text "Granite"
                    ]
                ]
            , li []
                [ a (floorBtnAttrs model "src/img/herringboneFloor.jpg")
                    [ span [ Attr.class "icon mr-2" ]
                        [ img [ Attr.src "src/img/herringboneFloor.jpg", Attr.style "border" "1px solid #dbdbdb" ] [] ]
                    , text "Herringbone"
                    ]
                ]
            , li []
                [ a (floorBtnAttrs model "src/img/laminateFloor.jpg")
                    [ span [ Attr.class "icon mr-2" ]
                        [ img [ Attr.src "src/img/laminateFloor.jpg", Attr.style "border" "1px solid #dbdbdb" ] [] ]
                    , text "Laminate"
                    ]
                ]
            , li []
                [ a (floorBtnAttrs model "src/img/patioFloor.jpg")
                    [ span [ Attr.class "icon mr-2" ]
                        [ img [ Attr.src "src/img/patioFloor.jpg", Attr.style "border" "1px solid #dbdbdb" ] [] ]
                    , text "Patio"
                    ]
                ]
            , li []
                [ a (floorBtnAttrs model "src/img/plankFloor.jpg")
                    [ span [ Attr.class "icon mr-2" ]
                        [ img [ Attr.src "src/img/plankFloor.jpg", Attr.style "border" "1px solid #dbdbdb" ] [] ]
                    , text "Plank"
                    ]
                ]
            ]
        ]


viewBottomBar : Model -> Html Msg
viewBottomBar model =
    let
        isBarDisabled =
            not (Dict.isEmpty model.canvasGrid.items)

        disabledStyles =
            if isBarDisabled then
                [ Attr.style "opacity" "0.5", Attr.style "pointer-events" "none" ]

            else
                []
    in
    nav (Attr.class "navbar is-fixed-bottom is-dark" :: disabledStyles)
        [ div [ Attr.class "container is-flex is-justify-content-center" ]
            [ div [ Attr.class "navbar-brand is-flex is-align-items-center" ]
                [ i [ Attr.class "fa-solid fa-ruler-combined fa-lg mr-3" ] []
                , a (sizeBtnAttrs model 3 3) [ text "3x3" ]
                , a (sizeBtnAttrs model 4 3) [ text "4x3" ]
                , a (sizeBtnAttrs model 6 5) [ text "6x5" ]
                , a (sizeBtnAttrs model 6 6) [ text "6x6" ]
                , text "Custom size (m)"
                , form
                    [ Attr.class "is-flex is-align-items-center"
                    , onSubmit (submitCustomSize model.customInputW model.customInputH)
                    ]
                    [ viewSquareInput model.customInputW SetCustomInputW
                    , text "x"
                    , viewSquareInput model.customInputH SetCustomInputH
                    , button
                        [ Attr.class "button is-small is-dark p-0 ml-2"
                        , Attr.style "width" "1.8rem"
                        , Attr.style "height" "1.8rem"
                        , Attr.type_ "submit"
                        , Attr.disabled isBarDisabled
                        ]
                        [ span [ Attr.class "icon is-small m-0" ]
                            [ i [ Attr.class "fas fa-check" ] [] ]
                        ]
                    ]
                ]
            ]
        ]


renderCanvas : Model -> Html Msg
renderCanvas model =
    let
        wStr =
            String.fromFloat model.canvasSize.width

        hStr =
            String.fromFloat model.canvasSize.height

        gridSize =
            "10"

        renderedItems =
            model.canvasGrid.items
                |> Dict.toList
                |> List.sortBy (\( _, item ) -> item.layer)
                |> List.map
                    (\( ( x, y ), item ) ->
                        Svg.image
                            [ SvgAttr.xlinkHref item.imgSrc
                            , SvgAttr.x (String.fromInt (x * 10))
                            , SvgAttr.y (String.fromInt (y * 10))
                            , SvgAttr.width (String.fromFloat item.width)
                            , SvgAttr.height (String.fromFloat item.height)
                            , SvgAttr.preserveAspectRatio "none"
                            ]
                            []
                    )

        previewItem =
            case model.placement of
                Idle ->
                    []

                HoldingItem item ->
                    let
                        ( mx, my ) =
                            model.mousePosition
                    in
                    [ Svg.image
                        [ SvgAttr.xlinkHref item.imgSrc
                        , SvgAttr.x (String.fromInt (mx * 10))
                        , SvgAttr.y (String.fromInt (my * 10))
                        , SvgAttr.width (String.fromFloat item.width)
                        , SvgAttr.height (String.fromFloat item.height)
                        , SvgAttr.preserveAspectRatio "none"
                        , Attr.style "opacity" "0.5"
                        ]
                        []
                    ]
    in
    svg
        [ SvgAttr.width wStr
        , SvgAttr.height hStr
        , SvgAttr.viewBox ("0 0 " ++ wStr ++ " " ++ hStr)
        , Attr.style "border" "2px solid #E0E0E0"
        , Attr.style "display" "block"
        , on "mousemove" (Decode.map MouseMoved mousePositionDecoder)
        , onClick (ClickCanvas model.mousePosition)
        ]
        ([ Svg.defs []
            [ Svg.pattern
                [ SvgAttr.id "floorPattern"
                , SvgAttr.patternUnits "userSpaceOnUse"
                , SvgAttr.width "200"
                , SvgAttr.height "200"
                ]
                [ Svg.image
                    [ SvgAttr.xlinkHref model.floorType
                    , SvgAttr.width "200"
                    , SvgAttr.height "200"
                    , SvgAttr.transform "rotate(90, 100, 100)"
                    ]
                    []
                ]
            ]
         , Svg.pattern
            [ SvgAttr.id "gridPattern"
            , SvgAttr.patternUnits "userSpaceOnUse"
            , SvgAttr.width gridSize
            , SvgAttr.height gridSize
            ]
            [ Svg.path
                [ SvgAttr.d ("M " ++ gridSize ++ " 0 L 0 0 0 " ++ gridSize)
                , SvgAttr.fill "none"
                , SvgAttr.stroke "rgba(0, 0, 0, 1)"
                , SvgAttr.strokeWidth "1"
                ]
                []
            ]
         , Svg.pattern
            [ SvgAttr.id "roomItem"
            , SvgAttr.patternUnits "userSpaceOnUse"
            , SvgAttr.width gridSize
            , SvgAttr.height gridSize
            ]
            [ Svg.path
                [ SvgAttr.d ("M " ++ gridSize ++ " 0 L 0 0 0 " ++ gridSize)
                , SvgAttr.fill "none"
                , SvgAttr.stroke "rgba(0, 0, 0, 1)"
                , SvgAttr.strokeWidth "1"
                ]
                []
            ]
         , Svg.rect
            [ SvgAttr.width wStr
            , SvgAttr.height hStr
            , SvgAttr.fill "url(#floorPattern)"
            ]
            []
         , if model.canvasGrid.active then
            Svg.rect
                [ SvgAttr.width wStr
                , SvgAttr.height hStr
                , SvgAttr.fill "url(#gridPattern)"
                ]
                []

           else
            Svg.text ""
         ]
            ++ (renderedItems ++ previewItem)
        )


viewSquareInput : String -> (String -> Msg) -> Html Msg
viewSquareInput currentVal toMsg =
    input
        [ Attr.value currentVal
        , Html.Events.onInput toMsg
        , Attr.class "mx-3"
        , Attr.style "width" "40px"
        , Attr.style "height" "25px"
        , Attr.style "text-align" "center"
        , Attr.style "border" "2px solid #555"
        , Attr.style "border-radius" "8px"
        , Attr.style "background" "none"
        , Attr.style "color" "white"
        , Attr.style "outline" "none"
        , Attr.required True
        ]
        []


viewToast : String -> Html Msg
viewToast message =
    div
        [ Attr.class "notification is-danger animate__animated animate__fadeInRight"
        , Attr.style "position" "fixed"
        , Attr.style "bottom" "10px"
        , Attr.style "right" "20px"
        , Attr.style "z-index" "1000"
        , Attr.style "box-shadow" "0 4px 12px rgba(0,0,0,0.1)"
        , Attr.style "padding-right" "3.5rem"
        , Attr.style "min-width" "200px"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        ]
        [ button
            [ Attr.class "delete"
            , onClick HideToaster
            , Attr.style "position" "absolute"
            , Attr.style "right" "0.5rem"
            , Attr.style "top" "0.5rem"
            ]
            []
        , span [] [ text message ]
        ]


sizeBtnAttrs : Model -> Float -> Float -> List (Attribute Msg)
sizeBtnAttrs model w h =
    let
        isActive =
            model.canvasSize.width == w * 100 && model.canvasSize.height == h * 100

        activeClass =
            if isActive then
                "has-background-warning has-text-black"

            else
                ""
    in
    [ Attr.class ("navbar-item has-text-weight-bold box m-0 mx-1 is-shadowless" ++ activeClass)
    , onClick (ResizeCanvas w h)
    ]


floorBtnAttrs : Model -> String -> List (Attribute Msg)
floorBtnAttrs model targetFloor =
    let
        isActive =
            model.floorType == targetFloor

        activeClass =
            if isActive then
                "has-background-warning has-text-weight-bold has-text-black"

            else
                ""
    in
    [ Attr.class ("is-flex is-align-items-center " ++ activeClass), onClick (SetFloorType targetFloor) ]



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
