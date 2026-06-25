module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, aside, button, div, form, i, img, input, li, nav, p, span, text, ul)
import Html.Attributes as Attr exposing (width)
import Html.Events exposing (on, onClick, onSubmit)
import Http
import Json.Decode as Decode
import Platform.Cmd as Cmd
import Svg exposing (Svg, svg)
import Svg.Attributes as SvgAttr
import Url
import Url.Parser as Parser exposing ((<?>), Parser)
import Url.Parser.Query as Query


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
    , rotation : Int
    }


type alias Position =
    ( Int, Int )


type alias Grid =
    { active : Bool
    , items : Dict Position RoomItem
    }


type alias Template =
    { name : String, code : String }


type PlacementState
    = Idle
    | HoldingItem RoomItem
    | ModifyingItem Position RoomItem


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , itemsFurniture : List RoomItem
    , itemsUtilities : List RoomItem
    , itemsDecor : List RoomItem
    , itemsStructure : List RoomItem
    , isOpenMenu : Bool
    , canvasSize : CanvasSize
    , canvasGrid : Grid
    , customInputW : String
    , customInputH : String
    , isOpenToaster : Bool
    , toasterMsg : String
    , toasterClass : String
    , floorType : String
    , mousePosition : Position
    , placement : PlacementState
    , history : List (Dict Position RoomItem)
    , inspirations : List Template
    }


allAvailableItems : List RoomItem
allAvailableItems =
    [ { name = "Bed", imgSrc = "src/img/bedFurniture.png", width = 140, height = 200, allowedOn = [ "Carpet" ], layer = 3, rotation = 0 }
    , { name = "Chair", imgSrc = "src/img/chairFurniture.png", width = 50, height = 50, allowedOn = [ "Carpet" ], layer = 2, rotation = 0 }
    , { name = "Table", imgSrc = "src/img/tableFurniture.png", width = 140, height = 80, allowedOn = [ "Carpet" ], layer = 1, rotation = 0 }
    , { name = "Desktop", imgSrc = "src/img/desktopUtilities.png", width = 120, height = 80, allowedOn = [ "Carpet" ], layer = 3, rotation = 0 }
    , { name = "Lamp", imgSrc = "src/img/lampUtilities.png", width = 40, height = 40, allowedOn = [ "Carpet", "Table", "Chair" ], layer = 3, rotation = 0 }
    , { name = "TV", imgSrc = "src/img/tvUtilities.png", width = 120, height = 50, allowedOn = [ "Carpet" ], layer = 3, rotation = 0 }
    , { name = "Carpet", imgSrc = "src/img/carpetDecor.png", width = 230, height = 160, allowedOn = [ "Carpet" ], layer = 0, rotation = 0 }
    , { name = "Plant", imgSrc = "src/img/plantDecor.png", width = 50, height = 50, allowedOn = [ "Carpet", "Table", "Chair" ], layer = 3, rotation = 0 }
    , { name = "Door", imgSrc = "src/img/doorStructure.svg", width = 140, height = 140, allowedOn = [ "Carpet" ], layer = 0, rotation = 0 }
    , { name = "Window", imgSrc = "src/img/windowStructure.svg", width = 140, height = 10, allowedOn = [ "Bed", "Chair", "Table", "Desktop", "Lamp", "TV", "Carpet", "Plant" ], layer = 4, rotation = 0 }
    ]


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        parsedRoom =
            getParsedRoom url

        initialGridItems =
            roomDecoder parsedRoom

        initialFloor =
            if parsedRoom == "" then
                "src/img/laminateFloor.jpg"

            else
                floorDecoder parsedRoom

        initialCanvasSize =
            if parsedRoom == "" then
                { width = 400, height = 300 }

            else
                canvasSizeDecoder parsedRoom
    in
    ( { key = key
      , url = url
      , itemsFurniture =
            [ { name = "Bed", imgSrc = "src/img/bedFurniture.png", width = 140, height = 200, allowedOn = [ "Carpet" ], layer = 3, rotation = 0 }
            , { name = "Chair", imgSrc = "src/img/chairFurniture.png", width = 50, height = 50, allowedOn = [ "Carpet" ], layer = 2, rotation = 0 }
            , { name = "Table", imgSrc = "src/img/tableFurniture.png", width = 140, height = 80, allowedOn = [ "Carpet" ], layer = 1, rotation = 0 }
            ]
      , itemsUtilities =
            [ { name = "Desktop", imgSrc = "src/img/desktopUtilities.png", width = 120, height = 80, allowedOn = [ "Carpet" ], layer = 3, rotation = 0 }
            , { name = "Lamp", imgSrc = "src/img/lampUtilities.png", width = 40, height = 40, allowedOn = [ "Carpet", "Table", "Chair" ], layer = 3, rotation = 0 }
            , { name = "TV", imgSrc = "src/img/tvUtilities.png", width = 120, height = 50, allowedOn = [ "Carpet" ], layer = 3, rotation = 0 }
            ]
      , itemsDecor =
            [ { name = "Carpet", imgSrc = "src/img/carpetDecor.png", width = 230, height = 160, allowedOn = [ "Carpet" ], layer = 0, rotation = 0 }
            , { name = "Plant", imgSrc = "src/img/plantDecor.png", width = 50, height = 50, allowedOn = [ "Carpet", "Table", "Chair" ], layer = 3, rotation = 0 }
            ]
      , itemsStructure =
            [ { name = "Door", imgSrc = "src/img/doorStructure.svg", width = 140, height = 140, allowedOn = [ "Carpet" ], layer = 0, rotation = 0 }
            , { name = "Window", imgSrc = "src/img/windowStructure.svg", width = 140, height = 10, allowedOn = [ "Bed", "Chair", "Table", "Desktop", "Lamp", "TV", "Carpet", "Plant" ], layer = 4, rotation = 0 }
            ]
      , isOpenMenu = True
      , canvasSize = initialCanvasSize
      , canvasGrid = { active = False, items = initialGridItems }
      , customInputW = ""
      , customInputH = ""
      , isOpenToaster = False
      , toasterMsg = ""
      , toasterClass = "is-danger"
      , floorType = initialFloor
      , mousePosition = ( 0, 0 )
      , placement = Idle
      , history = []
      , inspirations = []
      }
    , Cmd.none
    )


roomParser : Parser (Maybe String -> a) a
roomParser =
    Parser.top <?> Query.string "room"


encodingDict : Dict String String
encodingDict =
    Dict.fromList
        [ ( "Bed", "b" )
        , ( "Chair", "c" )
        , ( "Table", "t" )
        , ( "Desktop", "d" )
        , ( "Lamp", "l" )
        , ( "TV", "v" )
        , ( "Carpet", "r" )
        , ( "Plant", "p" )
        , ( "Door", "o" )
        , ( "Window", "w" )
        , ( "src/img/graniteFloor.jpg", "fg" )
        , ( "src/img/herringboneFloor.jpg", "fh" )
        , ( "src/img/laminateFloor.jpg", "fl" )
        , ( "src/img/patioFloor.jpg", "fp" )
        , ( "src/img/plankFloor.jpg", "fk" )
        ]


decodingDict : Dict String String
decodingDict =
    encodingDict
        |> Dict.toList
        |> List.map (\( key, value ) -> ( value, key ))
        |> Dict.fromList


getParsedRoom : Url.Url -> String
getParsedRoom url =
    let
        urlWithRootPath =
            { url | path = "/" }
    in
    case Parser.parse roomParser urlWithRootPath of
        Just (Just roomString) ->
            roomString

        _ ->
            ""


roomEncoder : Model -> String
roomEncoder model =
    let
        grid =
            model.canvasGrid

        items =
            grid.items

        floor =
            model.floorType

        canvasSize =
            model.canvasSize

        itemsCode =
            items
                |> Dict.toList
                |> List.map (\( ( x, y ), item ) -> Maybe.withDefault "" (Dict.get item.name encodingDict) ++ "," ++ String.fromInt x ++ "," ++ String.fromInt y ++ "," ++ String.fromInt item.rotation)
                |> String.join ";"
    in
    Maybe.withDefault "" (Dict.get floor encodingDict) ++ "," ++ String.fromFloat canvasSize.width ++ "," ++ String.fromFloat canvasSize.height ++ ";" ++ itemsCode


roomDecoder : String -> Dict Position RoomItem
roomDecoder encoded =
    if String.isEmpty encoded then
        Dict.empty

    else
        let
            parts =
                String.split ";" encoded

            itemStrings =
                List.drop 1 parts

            parseItem str =
                case String.split "," str of
                    [ code, xStr, yStr, rStr ] ->
                        let
                            name =
                                Maybe.withDefault "" (Dict.get code decodingDict)

                            x =
                                Maybe.withDefault 0 (String.toInt xStr)

                            y =
                                Maybe.withDefault 0 (String.toInt yStr)

                            r =
                                Maybe.withDefault 0 (String.toInt rStr)

                            matchedItem =
                                List.head (List.filter (\i -> i.name == name) allAvailableItems)
                        in
                        case matchedItem of
                            Just baseItem ->
                                Just ( ( x, y ), { baseItem | rotation = r } )

                            Nothing ->
                                Nothing

                    _ ->
                        Nothing
        in
        itemStrings |> List.filterMap parseItem |> Dict.fromList


floorDecoder : String -> String
floorDecoder encoded =
    if String.isEmpty encoded then
        "src/img/laminateFloor.jpg"

    else
        let
            header =
                encoded |> String.split ";" |> List.head |> Maybe.withDefault "fl"

            floorCode =
                header |> String.split "," |> List.head |> Maybe.withDefault "fl"
        in
        Maybe.withDefault "src/img/laminateFloor.jpg" (Dict.get floorCode decodingDict)


canvasSizeDecoder : String -> CanvasSize
canvasSizeDecoder encoded =
    if String.isEmpty encoded then
        { width = 400, height = 300 }

    else
        let
            header =
                encoded |> String.split ";" |> List.head |> Maybe.withDefault ""

            parts =
                String.split "," header
        in
        case parts of
            [ _, wStr, hStr ] ->
                { width = Maybe.withDefault 400 (String.toFloat wStr)
                , height = Maybe.withDefault 300 (String.toFloat hStr)
                }

            _ ->
                { width = 400, height = 300 }


type Msg
    = ToggleMenu
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ResizeCanvas Float Float
    | SetCustomInputW String
    | SetCustomInputH String
    | OpenToaster String String
    | HideToaster
    | SetFloorType String
    | SelectItem RoomItem
    | ClickCanvas Position
    | MouseMoved Position
    | Delete Position
    | Rotate Position RoomItem
    | MoveItem Position RoomItem
    | ClearCanvas
    | SaveRoom
    | Undo
    | FetchInspirations
    | GotInspirations (Result Http.Error (List Template))
    | LoadTemplate String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                parsedRoom =
                    getParsedRoom url

                oldGrid =
                    model.canvasGrid

                newGrid =
                    { oldGrid | items = roomDecoder parsedRoom }

                newFloor =
                    if parsedRoom == "" then
                        "src/img/laminateFloor.jpg"

                    else
                        floorDecoder parsedRoom

                newCanvasSize =
                    if parsedRoom == "" then
                        { width = 400, height = 300 }

                    else
                        canvasSizeDecoder parsedRoom
            in
            ( { model
                | url = url
                , canvasGrid = newGrid
                , floorType = newFloor
                , canvasSize = newCanvasSize
              }
            , Cmd.none
            )

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

        OpenToaster toastClass message ->
            ( { model | isOpenToaster = True, toasterClass = toastClass, toasterMsg = message }, Cmd.none )

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

                updatedHistory =
                    case model.placement of
                        HoldingItem _ ->
                            model.history

                        _ ->
                            model.canvasGrid.items :: model.history
            in
            ( { model | canvasGrid = newGrid, placement = HoldingItem item, history = updatedHistory }, Cmd.none )

        ClickCanvas position ->
            case model.placement of
                Idle ->
                    case findItemAtPos position model.canvasGrid.items of
                        Just ( oldPos, item ) ->
                            ( { model | placement = ModifyingItem oldPos item }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                HoldingItem item ->
                    let
                        clearedModel =
                            { model | isOpenToaster = False, toasterMsg = "", toasterClass = "is-danger" }
                    in
                    case checkPlacement model position item model.canvasGrid of
                        Err toasterMsg ->
                            update toasterMsg model

                        Ok newGrid ->
                            ( { clearedModel | placement = Idle, canvasGrid = newGrid }, Cmd.none )

                ModifyingItem curPosition item ->
                    ( { model | placement = Idle }, Cmd.none )

        MouseMoved pos ->
            ( { model | mousePosition = pos }, Cmd.none )

        Delete pos ->
            let
                oldGrid =
                    model.canvasGrid

                oldItems =
                    oldGrid.items

                newGrid =
                    { oldGrid | items = Dict.remove pos oldGrid.items }

                updatedHistory =
                    oldItems :: model.history
            in
            ( { model | canvasGrid = newGrid, history = updatedHistory }, Cmd.none )

        Rotate pos item ->
            let
                oldGrid =
                    model.canvasGrid

                clearedItems =
                    Dict.remove pos oldGrid.items

                clearedGrid =
                    { oldGrid | items = clearedItems }

                updatedHistory =
                    oldGrid.items :: model.history

                newRotation =
                    if item.rotation >= 270 then
                        0

                    else
                        item.rotation + 90

                newItem =
                    { item | rotation = newRotation }
            in
            case checkPlacement model pos newItem clearedGrid of
                Ok newGrid ->
                    ( { model | canvasGrid = newGrid, placement = ModifyingItem pos newItem, history = updatedHistory }, Cmd.none )

                Err toasterMsg ->
                    update toasterMsg model

        MoveItem pos item ->
            let
                oldGrid =
                    model.canvasGrid

                clearedItems =
                    Dict.remove pos oldGrid.items

                updatedHistory =
                    oldGrid.items :: model.history

                updatedGrid =
                    { oldGrid | items = clearedItems, active = True }
            in
            ( { model | canvasGrid = updatedGrid, placement = HoldingItem item, history = updatedHistory }, Cmd.none )

        ClearCanvas ->
            let
                oldGrid =
                    model.canvasGrid

                clearedGrid =
                    { oldGrid | items = Dict.empty }

                updatedHistory =
                    oldGrid.items :: model.history
            in
            ( { model | canvasGrid = clearedGrid, history = updatedHistory }, Cmd.none )

        SaveRoom ->
            let
                roomCode =
                    roomEncoder model

                currentUrl =
                    model.url

                newUrl =
                    { currentUrl | query = Just ("room=" ++ roomCode) }

                fullLink =
                    Url.toString newUrl

                nextModel =
                    { model
                        | isOpenToaster = True
                        , toasterClass = "is-success"
                        , toasterMsg = "Room saved! Url: " ++ fullLink
                    }
            in
            ( nextModel, Nav.replaceUrl model.key fullLink )

        Undo ->
            let
                oldHistory =
                    model.history

                lastItems =
                    List.head oldHistory

                updatedHistory =
                    Maybe.withDefault [] (List.tail model.history)

                oldGrid =
                    model.canvasGrid
            in
            case lastItems of
                Just dict ->
                    let
                        newGrid =
                            { oldGrid | items = dict }
                    in
                    ( { model | canvasGrid = newGrid, history = updatedHistory }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        FetchInspirations ->
            ( model, getInspirations )

        GotInspirations (Ok templates) ->
            ( { model | inspirations = templates }, Cmd.none )

        GotInspirations (Err _) ->
            ( { model | isOpenToaster = True, toasterClass = "is-danger", toasterMsg = "Fehler beim Laden der Vorlagen" }, Cmd.none )

        LoadTemplate code ->
            let
                newItems =
                    roomDecoder code

                newFloor =
                    floorDecoder code

                newCanvasSize =
                    canvasSizeDecoder code

                oldGrid =
                    model.canvasGrid

                updatedHistory =
                    oldGrid.items :: model.history
            in
            ( { model
                | canvasGrid = { active = False, items = newItems }
                , floorType = newFloor
                , canvasSize = newCanvasSize
                , history = updatedHistory
              }
            , Cmd.none
            )


onClickStopPropagation : msg -> Svg.Attribute msg
onClickStopPropagation msg =
    Html.Events.stopPropagationOn "click" (Decode.succeed ( msg, True ))


submitCustomSize : String -> String -> Msg
submitCustomSize w h =
    case ( String.toFloat w, String.toFloat h ) of
        ( Just width, Just height ) ->
            if width > 11 then
                OpenToaster "is-danger" "Width must be 11m or less"

            else if height > 6.5 then
                OpenToaster "is-danger" "Height must be 6.5m or less"

            else
                ResizeCanvas width height

        _ ->
            OpenToaster "is-danger" "Inputs should be of type number"


getRotatedItemDimensions : RoomItem -> ( Float, Float )
getRotatedItemDimensions item =
    if item.rotation == 90 || item.rotation == 270 then
        ( item.height, item.width )

    else
        ( item.width, item.height )


checkPlacement : Model -> Position -> RoomItem -> Grid -> Result Msg Grid
checkPlacement model position item oldGrid =
    let
        ( x, y ) =
            position

        ( itemX, itemY ) =
            getRotatedItemDimensions item

        cx =
            (toFloat x * 10) + (item.width / 2.0)

        cy =
            (toFloat y * 10) + (item.height / 2.0)

        nx1 =
            cx - (itemX / 2.0)

        nx2 =
            cx + (itemX / 2.0)

        ny1 =
            cy - (itemY / 2.0)

        ny2 =
            cy + (itemY / 2.0)

        checkCollision ( ( oldx, oldy ), oldItem ) =
            let
                ( oldItemX, oldItemY ) =
                    getRotatedItemDimensions oldItem

                oldCx =
                    (toFloat oldx * 10) + (oldItem.width / 2.0)

                oldCy =
                    (toFloat oldy * 10) + (oldItem.height / 2.0)

                ox1 =
                    oldCx - (oldItemX / 2.0)

                ox2 =
                    oldCx + (oldItemX / 2.0)

                oy1 =
                    oldCy - (oldItemY / 2.0)

                oy2 =
                    oldCy + (oldItemY / 2.0)
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
    if nx2 > model.canvasSize.width || ny2 > model.canvasSize.height || nx1 < 0 || ny1 < 0 then
        Err (OpenToaster "is-danger" (item.name ++ " can't be placed there"))

    else if Dict.member position oldGrid.items then
        Err (OpenToaster "is-danger" "This tile is already taken by another item")

    else if not checkAllCollisions then
        Err (OpenToaster "is-danger" "This position is already taken by another item")

    else
        let
            newItems =
                Dict.insert position item oldGrid.items
        in
        Ok { oldGrid | active = False, items = newItems }


findItemAtPos : Position -> Dict Position RoomItem -> Maybe ( Position, RoomItem )
findItemAtPos ( clickX, clickY ) items =
    let
        px =
            toFloat clickX * 10

        py =
            toFloat clickY * 10

        isHit ( ( oldx, oldy ), oldItem ) =
            let
                ox1 =
                    toFloat oldx * 10

                oy1 =
                    toFloat oldy * 10

                cx =
                    ox1 + (oldItem.width / 2.0)

                cy =
                    oy1 + (oldItem.height / 2.0)

                isRotated =
                    oldItem.rotation == 90 || oldItem.rotation == 270

                xMin =
                    if isRotated then
                        cx - (oldItem.height / 2.0)

                    else
                        cx - (oldItem.width / 2.0)

                xMax =
                    if isRotated then
                        cx + (oldItem.height / 2.0)

                    else
                        cx + (oldItem.width / 2.0)

                yMin =
                    if isRotated then
                        cy - (oldItem.width / 2.0)

                    else
                        cy - (oldItem.height / 2.0)

                yMax =
                    if isRotated then
                        cy + (oldItem.width / 2.0)

                    else
                        cy + (oldItem.height / 2.0)
            in
            (px >= xMin && px < xMax) && (py >= yMin && py < yMax)
    in
    items
        |> Dict.toList
        |> List.filter isHit
        |> List.sortBy (\( _, item ) -> -item.layer)
        |> List.head


templatesDecoder : Decode.Decoder (List Template)
templatesDecoder =
    Decode.list
        (Decode.map2 Template
            (Decode.field "name" Decode.string)
            (Decode.field "code" Decode.string)
        )


getInspirations : Cmd Msg
getInspirations =
    Http.get
        { url = "inspirations.json"
        , expect = Http.expectJson GotInspirations templatesDecoder
        }


mousePositionDecoder : Decode.Decoder Position
mousePositionDecoder =
    Decode.map2 (\x y -> ( floor (x / 10), floor (y / 10) ))
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)


view : Model -> Browser.Document Msg
view model =
    { title = "Room Constructor"
    , body =
        [ div
            [ Attr.class "hero is-fullheight is-clipped"
            ]
            [ if model.isOpenToaster then
                viewToast model.toasterClass model.toasterMsg

              else
                text ""
            , div [ Attr.class "hero-body p-0 is-relative" ]
                [ div [ Attr.style "position" "absolute", Attr.style "z-index" "20", Attr.style "top" "0", Attr.style "left" "0" ]
                    [ viewMenu model ]
                , div
                    [ Attr.style "position" "absolute"
                    , Attr.style "z-index" "10"
                    , Attr.style "top" "0"
                    , Attr.style "width" "100%"
                    , Attr.class "is-flex is-justify-content-center p-2"
                    ]
                    [ viewTopBar model ]
                , div [ Attr.class "is-flex is-justify-content-center is-align-items-center", Attr.style "width" "100%", Attr.style "height" "100%" ]
                    [ renderCanvas model ]
                , div [ Attr.style "position" "absolute", Attr.style "z-index" "10", Attr.style "top" "0", Attr.style "right" "0" ]
                    [ viewRoomSettings model ]
                ]
            , div [ Attr.class "hero-foot" ]
                [ viewBottomBar model ]
            ]
        ]
    }


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
                (List.map (\item -> li [] [ button [ Attr.class "is-flex is-justify-content-space-between is-align-items-center", onClick (SelectItem item) ] [ text item.name, span [ Attr.class "is-flex is-align-items-center is-justify-content-center ml-3", Attr.style "width" "35px", Attr.style "height" "35px" ] [ img [ Attr.src item.imgSrc, Attr.style "border" "1.7px solid #363636", Attr.style "border-radius" "4px", Attr.style "box-shadow" "0 2px 4px rgba(0,0,0,1)", Attr.style "width" "100%", Attr.style "height" "100%", Attr.style "object-fit" "contain" ] [] ] ] ]) model.itemsFurniture)
            , p [ Attr.class "menu-label" ] [ text "Utilities" ]
            , ul [ Attr.class "menu-list" ]
                (List.map (\item -> li [] [ button [ Attr.class "is-flex is-justify-content-space-between is-align-items-center", onClick (SelectItem item) ] [ text item.name, span [ Attr.class "is-flex is-align-items-center is-justify-content-center ml-3", Attr.style "width" "35px", Attr.style "height" "35px" ] [ img [ Attr.src item.imgSrc, Attr.style "border" "1.7px solid #363636", Attr.style "border-radius" "4px", Attr.style "box-shadow" "0 2px 4px rgba(0,0,0,1)", Attr.style "width" "100%", Attr.style "height" "100%", Attr.style "object-fit" "contain" ] [] ] ] ]) model.itemsUtilities)
            , p [ Attr.class "menu-label" ] [ text "Decor" ]
            , ul [ Attr.class "menu-list" ]
                (List.map (\item -> li [] [ button [ Attr.class "is-flex is-justify-content-space-between is-align-items-center", onClick (SelectItem item) ] [ text item.name, span [ Attr.class "is-flex is-align-items-center is-justify-content-center ml-3", Attr.style "width" "35px", Attr.style "height" "35px" ] [ img [ Attr.src item.imgSrc, Attr.style "border" "1.7px solid #363636", Attr.style "border-radius" "4px", Attr.style "box-shadow" "0 2px 4px rgba(0,0,0,1)", Attr.style "width" "100%", Attr.style "height" "100%", Attr.style "object-fit" "contain" ] [] ] ] ]) model.itemsDecor)
            , p [ Attr.class "menu-label" ] [ text "Structure" ]
            , ul [ Attr.class "menu-list" ]
                (List.map (\item -> li [] [ button [ Attr.class "is-flex is-justify-content-space-between is-align-items-center", onClick (SelectItem item) ] [ text item.name, span [ Attr.class "is-flex is-align-items-center is-justify-content-center ml-3", Attr.style "width" "35px", Attr.style "height" "35px" ] [ img [ Attr.src item.imgSrc, Attr.style "border" "1.7px solid #363636", Attr.style "border-radius" "4px", Attr.style "box-shadow" "0 2px 4px rgba(0,0,0,1)", Attr.style "width" "100%", Attr.style "height" "100%", Attr.style "object-fit" "contain" ] [] ] ] ]) model.itemsStructure)
            ]
        ]


viewRoomSettings : Model -> Html Msg
viewRoomSettings model =
    aside
        [ Attr.class "menu p-4" ]
        [ p [ Attr.class "menu-label has-text-centered" ] [ text "Floor" ]
        , ul [ Attr.class "menu-list" ]
            [ li []
                [ button (floorBtnAttrs model "src/img/graniteFloor.jpg")
                    [ span [ Attr.class "icon mr-2" ]
                        [ img [ Attr.src "src/img/graniteFloor.jpg", Attr.style "border" "1px solid #dbdbdb" ] [] ]
                    , text "Granite"
                    ]
                ]
            , li []
                [ button (floorBtnAttrs model "src/img/herringboneFloor.jpg")
                    [ span [ Attr.class "icon mr-2" ]
                        [ img [ Attr.src "src/img/herringboneFloor.jpg", Attr.style "border" "1px solid #dbdbdb" ] [] ]
                    , text "Herringbone"
                    ]
                ]
            , li []
                [ button (floorBtnAttrs model "src/img/laminateFloor.jpg")
                    [ span [ Attr.class "icon mr-2" ]
                        [ img [ Attr.src "src/img/laminateFloor.jpg", Attr.style "border" "1px solid #dbdbdb" ] [] ]
                    , text "Laminate"
                    ]
                ]
            , li []
                [ button (floorBtnAttrs model "src/img/patioFloor.jpg")
                    [ span [ Attr.class "icon mr-2" ]
                        [ img [ Attr.src "src/img/patioFloor.jpg", Attr.style "border" "1px solid #dbdbdb" ] [] ]
                    , text "Patio"
                    ]
                ]
            , li []
                [ button (floorBtnAttrs model "src/img/plankFloor.jpg")
                    [ span [ Attr.class "icon mr-2" ]
                        [ img [ Attr.src "src/img/plankFloor.jpg", Attr.style "border" "1px solid #dbdbdb" ] [] ]
                    , text "Plank"
                    ]
                ]
            ]
        , div [ Attr.class "mt-6 is-flex is-justify-content-center" ]
            [ button
                [ Attr.class "button is-primary has-text-weight-bold"
                , onClick FetchInspirations
                ]
                [ span [ Attr.class "icon" ]
                    [ i [ Attr.class "fa-solid fa-wand-magic-sparkles" ] [] ]
                , span [] [ text "Inspirations" ]
                ]
            ]
        , if List.isEmpty model.inspirations then
            text ""

          else
            div [ Attr.class "mt-4" ]
                [ ul [ Attr.class "menu-list" ]
                    (List.map
                        (\template ->
                            li []
                                [ button
                                    [ onClick (LoadTemplate template.code)
                                    , Attr.style "cursor" "pointer"
                                    ]
                                    [ text template.name ]
                                ]
                        )
                        model.inspirations
                    )
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
                , button (sizeBtnAttrs model 3 3) [ text "3x3" ]
                , button (sizeBtnAttrs model 4 3) [ text "4x3" ]
                , button (sizeBtnAttrs model 6 5) [ text "6x5" ]
                , button (sizeBtnAttrs model 6 6) [ text "6x6" ]
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


viewTopBar : Model -> Html Msg
viewTopBar model =
    nav [ Attr.class "navbar is-fixed-top is-dark" ]
        [ div [ Attr.class "container is-flex is-justify-content-center" ]
            [ div [ Attr.class "navbar-brand is-flex is-align-items-center" ]
                [ button
                    [ Attr.class "button is-small is-dark mx-1"
                    , Attr.type_ "button"
                    , Attr.title "Clear All"
                    , onClick ClearCanvas
                    ]
                    [ span [ Attr.class "icon is-small has-text-danger" ]
                        [ i [ Attr.class "fa-solid fa-trash" ] [] ]
                    ]
                , button
                    [ Attr.class "button is-small is-dark mx-1"
                    , Attr.type_ "button"
                    , Attr.title "Undo"
                    , onClick Undo
                    ]
                    [ span [ Attr.class "icon is-small has-text-grey-light" ]
                        [ i [ Attr.class "fa-solid fa-rotate-left" ] [] ]
                    ]
                , button
                    [ Attr.class "button is-small is-dark mx-1"
                    , Attr.type_ "button"
                    , Attr.title "Save"
                    , onClick SaveRoom
                    ]
                    [ span [ Attr.class "icon is-small has-text-primary" ]
                        [ i [ Attr.class "fa-solid fa-floppy-disk" ] [] ]
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

        modifyingData =
            case model.placement of
                ModifyingItem pos item ->
                    Just ( pos, item )

                _ ->
                    Nothing

        renderedItems =
            model.canvasGrid.items
                |> Dict.toList
                |> List.sortBy (\( _, item ) -> item.layer)
                |> List.map
                    (\( ( x, y ), item ) ->
                        let
                            isModifying =
                                case modifyingData of
                                    Just ( ( modX, modY ), _ ) ->
                                        x == modX && y == modY

                                    Nothing ->
                                        False

                            cx =
                                (toFloat x * 10.0) + (item.width / 2.0)

                            cy =
                                (toFloat y * 10.0) + (item.height / 2.0)

                            angle =
                                String.fromInt item.rotation

                            groupTransform =
                                "rotate(" ++ angle ++ ", " ++ String.fromFloat cx ++ ", " ++ String.fromFloat cy ++ ")"
                        in
                        Svg.g []
                            [ Svg.g [ SvgAttr.transform groupTransform ]
                                [ Svg.image
                                    [ SvgAttr.xlinkHref item.imgSrc
                                    , SvgAttr.x (String.fromInt (x * 10))
                                    , SvgAttr.y (String.fromInt (y * 10))
                                    , SvgAttr.width (String.fromFloat item.width)
                                    , SvgAttr.height (String.fromFloat item.height)
                                    , SvgAttr.preserveAspectRatio "none"
                                    , SvgAttr.opacity
                                        (if isModifying then
                                            "0.6"

                                         else
                                            "1.0"
                                        )
                                    ]
                                    []
                                ]
                            , if isModifying then
                                renderModifyingOverlay ( x, y ) item

                              else
                                Svg.g [] []
                            ]
                    )

        previewItem =
            case model.placement of
                Idle ->
                    []

                HoldingItem item ->
                    let
                        ( mx, my ) =
                            model.mousePosition

                        cx =
                            (toFloat mx * 10.0) + (item.width / 2.0)

                        cy =
                            (toFloat my * 10.0) + (item.height / 2.0)

                        angle =
                            String.fromInt item.rotation

                        groupTransform =
                            "rotate(" ++ angle ++ ", " ++ String.fromFloat cx ++ ", " ++ String.fromFloat cy ++ ")"
                    in
                    [ Svg.g
                        [ SvgAttr.transform groupTransform ]
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
                        , Svg.rect
                            [ SvgAttr.x (String.fromInt (mx * 10))
                            , SvgAttr.y (String.fromInt (my * 10))
                            , SvgAttr.width (String.fromFloat item.width)
                            , SvgAttr.height (String.fromFloat item.height)
                            , SvgAttr.fill "none"
                            , SvgAttr.stroke "#00a8ff"
                            , SvgAttr.strokeWidth "2"
                            , SvgAttr.strokeDasharray "4 4"
                            ]
                            []
                        ]
                    ]

                ModifyingItem position item ->
                    []
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


renderModifyingOverlay : Position -> RoomItem -> Svg Msg
renderModifyingOverlay ( x, y ) item =
    let
        posX =
            toFloat x * 10

        posY =
            toFloat y * 10

        btnSize =
            24

        btnSpacing =
            8

        totalWidth =
            (btnSize * 3) + (btnSpacing * 2)

        barX =
            posX + ((item.width - totalWidth) / 2.0)

        barY =
            posY + ((item.height - btnSize) / 2.0)

        cx =
            (toFloat x * 10.0) + (item.width / 2.0)

        cy =
            (toFloat y * 10.0) + (item.height / 2.0)

        angle =
            String.fromInt item.rotation

        groupTransform =
            "rotate(" ++ angle ++ ", " ++ String.fromFloat cx ++ ", " ++ String.fromFloat cy ++ ")"
    in
    Svg.g []
        [ Svg.rect
            [ SvgAttr.x (String.fromFloat posX)
            , SvgAttr.y (String.fromFloat posY)
            , SvgAttr.width (String.fromFloat item.width)
            , SvgAttr.height (String.fromFloat item.height)
            , SvgAttr.fill "none"
            , SvgAttr.stroke "#007aff"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.strokeDasharray "4 4"
            , SvgAttr.transform groupTransform
            ]
            []
        , Svg.g
            [ SvgAttr.transform ("translate(" ++ String.fromFloat barX ++ "," ++ String.fromFloat barY ++ ")")
            ]
            [ Svg.g [ onClickStopPropagation (MoveItem ( x, y ) item), SvgAttr.cursor "pointer" ]
                [ Svg.rect
                    [ SvgAttr.x "0"
                    , SvgAttr.y "0"
                    , SvgAttr.width (String.fromFloat btnSize)
                    , SvgAttr.height (String.fromFloat btnSize)
                    , SvgAttr.fill "white"
                    , SvgAttr.stroke "black"
                    , SvgAttr.strokeWidth "1"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M 12,4 L 12,20 M 4,12 L 20,12 M 12,4 L 9,7 M 12,4 L 15,7 M 12,20 L 9,17 M 12,20 L 15,17 M 4,12 L 7,9 M 4,12 L 7,15 M 20,12 L 17,9 M 20,12 L 17,15"
                    , SvgAttr.stroke "black"
                    , SvgAttr.strokeWidth "1.5"
                    , SvgAttr.fill "none"
                    ]
                    []
                ]
            , Svg.g
                [ SvgAttr.transform ("translate(" ++ String.fromFloat (btnSize + btnSpacing) ++ ",0)")
                , Html.Events.onClick (Rotate ( x, y ) item)
                , SvgAttr.cursor "pointer"
                ]
                [ Svg.rect
                    [ SvgAttr.x "0"
                    , SvgAttr.y "0"
                    , SvgAttr.width (String.fromFloat btnSize)
                    , SvgAttr.height (String.fromFloat btnSize)
                    , SvgAttr.fill "white"
                    , SvgAttr.stroke "black"
                    , SvgAttr.strokeWidth "1"
                    ]
                    []
                , Svg.text_
                    [ SvgAttr.x "6"
                    , SvgAttr.y "17"
                    , SvgAttr.fill "black"
                    , SvgAttr.fontSize "14"
                    , SvgAttr.fontWeight "bold"
                    ]
                    [ Svg.text "⟳" ]
                ]
            , Svg.g
                [ SvgAttr.transform ("translate(" ++ String.fromFloat ((btnSize * 2) + (btnSpacing * 2)) ++ ",0)")
                , Html.Events.onClick (Delete ( x, y ))
                , SvgAttr.cursor "pointer"
                ]
                [ Svg.rect
                    [ SvgAttr.x "0"
                    , SvgAttr.y "0"
                    , SvgAttr.width (String.fromFloat btnSize)
                    , SvgAttr.height (String.fromFloat btnSize)
                    , SvgAttr.fill "white"
                    , SvgAttr.stroke "black"
                    , SvgAttr.strokeWidth "1"
                    ]
                    []
                , Svg.text_
                    [ SvgAttr.x "7"
                    , SvgAttr.y "17"
                    , SvgAttr.fill "black"
                    , SvgAttr.fontSize "14"
                    , SvgAttr.fontWeight "bold"
                    ]
                    [ Svg.text "X" ]
                ]
            ]
        ]


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


viewToast : String -> String -> Html Msg
viewToast toastClass message =
    div
        [ Attr.class ("notification " ++ toastClass ++ " animate__animated animate__fadeInRight")
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
                "is-dark has-text-white"
    in
    [ Attr.class ("button navbar-item has-text-weight-bold m-0 mx-2 is-borderless " ++ activeClass)
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
