module Pages.CityView exposing (Actions, CityMenuModal(..), viewCity)

{-| Top-level City View HUD and 3D diorama workspace (`Pages.CityView`).

Displays the full-screen 3D interactive city (`City.Render`), the `UI.CameraGlobe`
navigation widget with Orbit / Street Walk toggle, live city statistics,
expandable grid controls (`4×4`, `5×5`, `6×6`, `8×8`), selected-lot inspector
with instant 9-template stamping and 3D Element Editor entry, local JSON/.mpd
import/export menu, and real-time Cloudflare signaling collaboration panel.

-}

import City.I18n as T
import City.Mesh exposing (MeshCache)
import City.Render as Render
import City.Stats as Stats
import City.Templates as Templates exposing (Template)
import City.Types exposing (CityGrid, CityRoom, LotCoord, LotElement, RoomConnection(..), RoomRole(..))
import Dict
import FeatherIcons
import Html exposing (Html, button, div, img, input, p, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import I18n exposing (Language(..))
import Render.Camera as Camera exposing (Camera, Navigation(..))
import UI.CameraGlobe as CameraGlobe


{-| Optional open modal/panel in the City View overlay.
-}
type CityMenuModal
    = NoModal
    | CollabModal
    | CityFileModal


{-| Callbacks from `Pages.CityView` back to `Main`.
-}
type alias Actions msg =
    { onCityNameChanged : String -> msg
    , onResizeGrid : Int -> msg
    , onPointerDown : Float -> Float -> Int -> Bool -> msg
    , onPointerMove : Float -> Float -> msg
    , onPointerUp : Float -> Float -> msg
    , onDoubleClick : Float -> Float -> msg
    , onWheel : Float -> msg
    , onGlobePress : ( Float, Float ) -> msg
    , onGlobeMove : ( Float, Float ) -> msg
    , onGlobeRelease : msg
    , onGlobeZoom : Float -> msg
    , onGlobeZoomDone : msg
    , onGlobeReset : msg
    , onToggleWalk : msg
    , onEditSelectedLot : msg
    , onRotateSelectedLot : msg
    , onCopySelectedLot : msg
    , onPasteSelectedLot : msg
    , onClearSelectedLot : msg
    , onStampTemplate : Template -> msg
    , onDeselectLot : msg
    , onToggleModal : CityMenuModal -> msg
    , onHostRoom : msg
    , onJoinRoom : msg
    , onLeaveRoom : msg
    , onRoomCodeInput : String -> msg
    , onCopyRoomLink : msg
    , onLoadStarterCity : msg
    , onNewEmptyCity : msg
    , onExportMpd : msg
    , onExportJson : msg
    , onImportJson : msg
    , onToggleLanguage : msg
    , onToggleMute : msg
    }


{-| Render the interactive 3D City View and HUD overlays.
-}
viewCity :
    { city : CityGrid
    , meshes : MeshCache
    , camera : Camera
    , selectedLot : Maybe LotCoord
    , hoveredLot : Maybe LotCoord
    , clipboard : Maybe LotElement
    , room : CityRoom
    , roomCodeInput : String
    , openModal : CityMenuModal
    , toast : Maybe String
    , audioMuted : Bool
    , language : Language
    , time : Float
    , width : Int
    , height : Int
    , actions : Actions msg
    }
    -> Html msg
viewCity cfg =
    let
        stats =
            Stats.computeStats cfg.city
    in
    div
        [ Attr.class "fixed inset-0 overflow-hidden bg-brand text-white font-sans select-none"
        , Attr.attribute "data-testid" "city-view-page"
        ]
        [ Render.renderCityCanvas
            { city = cfg.city
            , meshes = cfg.meshes
            , camera = cfg.camera
            , selectedLot = cfg.selectedLot
            , hoveredLot = cfg.hoveredLot
            , peers = cfg.room.peers
            , mySlot = cfg.room.mySlot
            , time = cfg.time
            , width = cfg.width
            , height = cfg.height
            , onPointerDown = cfg.actions.onPointerDown
            , onPointerMove = cfg.actions.onPointerMove
            , onPointerUp = cfg.actions.onPointerUp
            , onDoubleClick = cfg.actions.onDoubleClick
            , onWheel = cfg.actions.onWheel
            }
        , topHud cfg stats
        , walkBanner cfg.language cfg.camera
        , toastView cfg.toast
        , case cfg.selectedLot of
            Just coord ->
                lotInspectorCard cfg coord

            Nothing ->
                text ""
        , cameraGlobeCorner cfg
        , case cfg.openModal of
            CollabModal ->
                collabOverlay cfg

            CityFileModal ->
                cityFileOverlay cfg

            NoModal ->
                text ""
        ]


topHud :
    { a
        | city : CityGrid
        , room : CityRoom
        , openModal : CityMenuModal
        , audioMuted : Bool
        , language : Language
        , actions : Actions msg
    }
    -> City.Types.CityStats
    -> Html msg
topHud cfg stats =
    let
        lang =
            cfg.language

        isCollab =
            cfg.room.role /= LocalSolo

        roomBadgeLabel =
            case cfg.room.status of
                Connected ->
                    cfg.room.roomId

                Connecting ->
                    T.connectingLabel lang

                _ ->
                    T.roomHeading lang
    in
    div
        [ Attr.class "absolute top-0 left-0 right-0 z-20 flex flex-col gap-2 p-2.5 pointer-events-none"
        , Attr.attribute "data-testid" "city-top-hud"
        ]
        [ div [ Attr.class "flex flex-wrap items-center justify-between gap-2" ]
            [ div [ Attr.class "pointer-events-auto flex items-center gap-2 px-3 py-1.5 rounded-2xl bg-brand/90 border border-white/15 backdrop-blur-md shadow-lg" ]
                [ img [ Attr.src "/favicon.svg", Attr.alt "", Attr.class "w-7 h-7 rounded-md shrink-0" ] []
                , input
                    [ Attr.type_ "text"
                    , Attr.value cfg.city.cityName
                    , onInput cfg.actions.onCityNameChanged
                    , Attr.attribute "aria-label" (T.cityNameLabel lang)
                    , Attr.attribute "data-testid" "city-name-input"
                    , Attr.class "w-32 sm:w-44 bg-transparent border-b border-white/20 focus:border-brand-yellow outline-none text-sm font-extrabold text-white"
                    ]
                    []
                , gridSizePills cfg.city.cols cfg.actions.onResizeGrid
                ]
            , div [ Attr.class "pointer-events-auto flex items-center gap-1.5" ]
                [ button
                    [ Attr.type_ "button"
                    , onClick (cfg.actions.onToggleModal CollabModal)
                    , Attr.attribute "data-testid" "collab-modal-button"
                    , Attr.class
                        ("inline-flex items-center gap-1.5 h-10 px-3 rounded-xl border text-xs font-extrabold cursor-pointer shadow-md transition-all "
                            ++ (if isCollab then
                                    "bg-emerald-600/90 border-emerald-300/60 text-white"

                                else
                                    "bg-brand/90 border-white/20 text-brand-yellow hover:border-brand-yellow"
                               )
                        )
                    ]
                    [ FeatherIcons.users |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml []
                    , span [] [ text roomBadgeLabel ]
                    , if isCollab then
                        span [ Attr.class "px-1.5 py-0.5 rounded-full bg-black/30 text-[10px] font-mono" ]
                            [ text (String.fromInt (List.length cfg.room.peers)) ]

                      else
                        text ""
                    ]
                , button
                    [ Attr.type_ "button"
                    , onClick (cfg.actions.onToggleModal CityFileModal)
                    , Attr.attribute "data-testid" "city-menu-button"
                    , Attr.title (T.tr lang "City & Export" "Kaupunki ja vienti")
                    , Attr.class "inline-flex items-center gap-1.5 h-10 px-3 rounded-xl bg-brand/90 border border-white/20 text-xs font-bold text-white cursor-pointer shadow-md hover:border-white/40"
                    ]
                    [ FeatherIcons.folder |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml []
                    , span [ Attr.class "hidden sm:inline" ] [ text (T.tr lang "City" "Kaupunki") ]
                    ]
                , button
                    [ Attr.type_ "button"
                    , onClick cfg.actions.onToggleLanguage
                    , Attr.attribute "data-testid" "language-toggle-button"
                    , Attr.class "h-10 px-2.5 rounded-xl bg-brand/90 border border-white/20 text-xs font-black text-slate-200 cursor-pointer hover:text-brand-yellow"
                    ]
                    [ text
                        (case lang of
                            Fi ->
                                "EN"

                            En ->
                                "FI"
                        )
                    ]
                , button
                    [ Attr.type_ "button"
                    , onClick cfg.actions.onToggleMute
                    , Attr.title (T.muteLabel lang cfg.audioMuted)
                    , Attr.attribute "aria-label" (T.muteLabel lang cfg.audioMuted)
                    , Attr.class "w-10 h-10 flex items-center justify-center rounded-xl bg-brand/90 border border-white/20 text-white cursor-pointer hover:text-brand-yellow"
                    ]
                    [ (if cfg.audioMuted then
                        FeatherIcons.volumeX

                       else
                        FeatherIcons.volume2
                      )
                        |> FeatherIcons.withSize 16
                        |> FeatherIcons.toHtml []
                    ]
                ]
            ]
        , statsBar lang stats
        ]


gridSizePills : Int -> (Int -> msg) -> Html msg
gridSizePills currentSize onResize =
    let
        pill size =
            let
                active =
                    currentSize == size
            in
            button
                [ Attr.type_ "button"
                , onClick (onResize size)
                , Attr.attribute "data-testid" ("grid-size-" ++ String.fromInt size)
                , Attr.class
                    ("px-2 py-1 rounded-lg text-[11px] font-mono font-bold cursor-pointer transition-all "
                        ++ (if active then
                                "bg-brand-yellow text-brand"

                            else
                                "bg-white/10 text-slate-300 hover:bg-white/20"
                           )
                    )
                ]
                [ text (String.fromInt size ++ "×" ++ String.fromInt size) ]
    in
    div [ Attr.class "hidden md:flex items-center gap-1 pl-2 border-l border-white/15" ]
        (List.map pill [ 4, 5, 6, 8 ])


statsBar : Language -> City.Types.CityStats -> Html msg
statsBar lang stats =
    let
        statPill icon label value =
            div [ Attr.class "inline-flex items-center gap-1.5 px-2.5 py-1 rounded-xl bg-brand/85 border border-white/10 text-xs" ]
                [ span [ Attr.class "text-brand-yellow shrink-0" ]
                    [ icon |> FeatherIcons.withSize 13 |> FeatherIcons.toHtml [] ]
                , span [ Attr.class "text-slate-300 hidden sm:inline" ] [ text label ]
                , span [ Attr.class "font-mono font-bold text-white" ] [ text value ]
                ]
    in
    div
        [ Attr.class "pointer-events-auto self-start flex flex-wrap items-center gap-1.5"
        , Attr.attribute "data-testid" "city-stats-bar"
        ]
        [ statPill FeatherIcons.users (T.statPopulation lang) (String.fromInt stats.population)
        , statPill FeatherIcons.home (T.statBuildings lang) (String.fromInt stats.buildings)
        , statPill FeatherIcons.feather (T.statGreen lang) (String.fromInt stats.parks ++ "%")
        , statPill FeatherIcons.gitCommit (T.statRoads lang) (String.fromInt stats.roadConnections)
        , statPill FeatherIcons.barChart2 (T.statPeak lang) (String.fromInt stats.maxHeightPlates ++ "/32")
        , statPill FeatherIcons.award (T.statScore lang) (String.fromInt stats.score)
        ]


walkBanner : Language -> Camera -> Html msg
walkBanner lang camera =
    if camera.navigation == Walk then
        div
            [ Attr.class "absolute top-28 left-1/2 -translate-x-1/2 z-20 px-3.5 py-1.5 rounded-xl bg-brand/90 border border-brand-yellow/40 text-brand-yellow text-xs font-bold shadow-lg pointer-events-none"
            ]
            [ text (T.walkHint lang) ]

    else
        text ""


toastView : Maybe String -> Html msg
toastView maybeToast =
    case maybeToast of
        Just msg ->
            div
                [ Attr.attribute "role" "status"
                , Attr.attribute "data-testid" "city-toast"
                , Attr.class "absolute top-24 left-1/2 -translate-x-1/2 z-30 px-4 py-2 rounded-xl bg-slate-900/95 border border-brand-yellow/60 text-brand-yellow text-xs font-bold shadow-xl"
                ]
                [ text msg ]

        Nothing ->
            text ""


lotInspectorCard :
    { a
        | city : CityGrid
        , clipboard : Maybe LotElement
        , language : Language
        , actions : Actions msg
    }
    -> LotCoord
    -> Html msg
lotInspectorCard cfg coord =
    let
        lang =
            cfg.language

        maybeEl =
            Dict.get ( coord.row, coord.col ) cfg.city.lots

        titleText =
            case maybeEl of
                Just el ->
                    el.name

                Nothing ->
                    T.emptyLotTitle lang

        subtitleText =
            case maybeEl of
                Just el ->
                    T.categoryLabel lang el.category
                        ++ " • "
                        ++ T.heightUnit lang el.heightPlates
                        ++ " • "
                        ++ String.fromInt el.partCount
                        ++ " pcs"

                Nothing ->
                    T.emptyLotHint lang
    in
    div
        [ Attr.attribute "data-testid" "lot-inspector"
        , Attr.class "absolute left-2.5 bottom-2.5 z-20 w-[calc(100%-10rem)] max-w-sm p-3.5 rounded-2xl bg-brand/95 border border-white/15 backdrop-blur-md shadow-2xl flex flex-col gap-3"
        ]
        [ div [ Attr.class "flex items-start justify-between gap-2" ]
            [ div [ Attr.class "min-w-0" ]
                [ span [ Attr.class "text-[11px] font-mono font-bold uppercase tracking-wider text-brand-yellow" ]
                    [ text (T.lotLabel lang coord.col coord.row) ]
                , p [ Attr.class "text-sm font-extrabold text-white truncate" ] [ text titleText ]
                , p [ Attr.class "text-xs text-slate-300" ] [ text subtitleText ]
                ]
            , button
                [ Attr.type_ "button"
                , onClick cfg.actions.onDeselectLot
                , Attr.class "w-8 h-8 shrink-0 flex items-center justify-center rounded-lg bg-white/10 text-slate-300 cursor-pointer hover:bg-white/20"
                ]
                [ FeatherIcons.x |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
            ]
        , div [ Attr.class "flex flex-wrap items-center gap-1.5" ]
            [ button
                [ Attr.type_ "button"
                , onClick cfg.actions.onEditSelectedLot
                , Attr.attribute "data-testid" "edit-lot-3d-button"
                , Attr.class "flex-1 min-w-[8rem] inline-flex items-center justify-center gap-1.5 h-10 px-3 rounded-xl bg-brand-yellow text-brand font-extrabold text-xs cursor-pointer shadow-md active:brightness-110"
                ]
                [ FeatherIcons.tool |> FeatherIcons.withSize 15 |> FeatherIcons.toHtml []
                , span [] [ text (T.actionBuildLot lang) ]
                ]
            , if maybeEl /= Nothing then
                smallActionBtn (T.actionRotateLot lang) FeatherIcons.rotateCw cfg.actions.onRotateSelectedLot "rotate-lot-button"

              else
                text ""
            , if maybeEl /= Nothing then
                smallActionBtn (T.actionCopyLot lang) FeatherIcons.copy cfg.actions.onCopySelectedLot "copy-lot-button"

              else
                text ""
            , case cfg.clipboard of
                Just _ ->
                    smallActionBtn (T.actionPasteLot lang) FeatherIcons.clipboard cfg.actions.onPasteSelectedLot "paste-lot-button"

                Nothing ->
                    text ""
            , if maybeEl /= Nothing then
                smallActionBtn (T.actionClearLot lang) FeatherIcons.trash2 cfg.actions.onClearSelectedLot "clear-lot-button"

              else
                text ""
            ]
        , div [ Attr.class "flex flex-col gap-1.5" ]
            [ span [ Attr.class "text-[10px] font-bold uppercase tracking-wider text-slate-400" ]
                [ text (T.quickTemplatesLabel lang) ]
            , div [ Attr.class "grid grid-cols-3 gap-1.5" ]
                (List.map (templateQuickButton lang cfg.actions.onStampTemplate) Templates.all)
            ]
        ]


smallActionBtn : String -> FeatherIcons.Icon -> msg -> String -> Html msg
smallActionBtn label icon msg testId =
    button
        [ Attr.type_ "button"
        , onClick msg
        , Attr.title label
        , Attr.attribute "aria-label" label
        , Attr.attribute "data-testid" testId
        , Attr.class "w-10 h-10 flex items-center justify-center rounded-xl bg-white/10 border border-white/15 text-white cursor-pointer hover:bg-white/20"
        ]
        [ icon |> FeatherIcons.withSize 15 |> FeatherIcons.toHtml [] ]


templateQuickButton : Language -> (Template -> msg) -> Template -> Html msg
templateQuickButton lang onStamp tmpl =
    button
        [ Attr.type_ "button"
        , onClick (onStamp tmpl)
        , Attr.attribute "data-testid" ("stamp-" ++ tmpl.id)
        , Attr.class "px-2 py-1.5 rounded-lg bg-slate-800/90 border border-white/10 hover:border-brand-yellow/60 text-left cursor-pointer transition-all"
        ]
        [ p [ Attr.class "text-[11px] font-bold text-slate-100 truncate" ]
            [ text (T.tr lang tmpl.nameEn tmpl.nameFi) ]
        , span [ Attr.class "text-[10px] font-mono text-sky-300" ]
            [ text (String.fromInt tmpl.heightPlates ++ "/32") ]
        ]


cameraGlobeCorner :
    { a
        | camera : Camera
        , language : Language
        , actions : Actions msg
    }
    -> Html msg
cameraGlobeCorner cfg =
    let
        isWalk =
            cfg.camera.navigation == Walk

        walkBtn =
            button
                [ Attr.type_ "button"
                , onClick cfg.actions.onToggleWalk
                , Attr.attribute "data-testid" "toggle-walk-button"
                , Attr.title
                    (if isWalk then
                        T.modeOrbitLabel cfg.language

                     else
                        T.modeWalkLabel cfg.language
                    )
                , Attr.class
                    ("w-8 h-8 flex items-center justify-center rounded-lg border cursor-pointer "
                        ++ (if isWalk then
                                "bg-brand-yellow border-brand-yellow text-brand"

                            else
                                "bg-slate-800 border-white/20 text-white"
                           )
                    )
                ]
                [ (if isWalk then
                    FeatherIcons.globe

                   else
                    FeatherIcons.compass
                  )
                    |> FeatherIcons.withSize 15
                    |> FeatherIcons.toHtml []
                ]
    in
    div [ Attr.class "absolute right-2.5 bottom-2.5 z-20" ]
        [ CameraGlobe.view
            { camera = cfg.camera
            , onPress = cfg.actions.onGlobePress
            , onMove = cfg.actions.onGlobeMove
            , onRelease = cfg.actions.onGlobeRelease
            , onZoom = cfg.actions.onGlobeZoom
            , onZoomDone = cfg.actions.onGlobeZoomDone
            , onReset = cfg.actions.onGlobeReset
            , extras = [ walkBtn ]
            }
        ]


collabOverlay :
    { a
        | room : CityRoom
        , roomCodeInput : String
        , language : Language
        , actions : Actions msg
    }
    -> Html msg
collabOverlay cfg =
    let
        lang =
            cfg.language

        inRoom =
            cfg.room.role /= LocalSolo
    in
    modalBackdrop (cfg.actions.onToggleModal NoModal)
        (T.roomHeading lang)
        [ if inRoom then
            div [ Attr.class "flex flex-col gap-3" ]
                [ div [ Attr.class "flex items-center justify-between gap-2 p-3 rounded-xl bg-slate-900 border border-white/15" ]
                    [ div []
                        [ span [ Attr.class "text-xs text-slate-400" ] [ text (T.tr lang "Room Code" "Huonekoodi") ]
                        , p [ Attr.class "text-lg font-mono font-black text-brand-yellow" ] [ text cfg.room.roomId ]
                        ]
                    , button
                        [ Attr.type_ "button"
                        , onClick cfg.actions.onCopyRoomLink
                        , Attr.class "inline-flex items-center gap-1.5 h-9 px-3 rounded-xl bg-brand-yellow text-brand text-xs font-extrabold cursor-pointer"
                        ]
                        [ FeatherIcons.copy |> FeatherIcons.withSize 14 |> FeatherIcons.toHtml []
                        , span [] [ text (T.copyInviteLink lang) ]
                        ]
                    ]
                , div [ Attr.class "flex flex-col gap-1.5" ]
                    [ span [ Attr.class "text-xs font-bold uppercase tracking-wider text-slate-400" ]
                        [ text (T.buildersHeading lang) ]
                    , div [ Attr.class "flex flex-wrap gap-1.5" ]
                        (List.map
                            (\peer ->
                                div [ Attr.class "inline-flex items-center gap-1.5 px-2.5 py-1 rounded-full bg-slate-800 border border-white/15 text-xs" ]
                                    [ span [ Attr.class "w-2.5 h-2.5 rounded-full", Attr.style "background" peer.color ] []
                                    , span [ Attr.class "font-semibold" ]
                                        [ text
                                            (peer.name
                                                ++ (if peer.slot == cfg.room.mySlot then
                                                        " (" ++ T.youTag lang ++ ")"

                                                    else
                                                        ""
                                                   )
                                            )
                                        ]
                                    ]
                            )
                            cfg.room.peers
                        )
                    ]
                , button
                    [ Attr.type_ "button"
                    , onClick cfg.actions.onLeaveRoom
                    , Attr.class "h-10 px-4 rounded-xl bg-rose-600/80 hover:bg-rose-600 text-white text-xs font-extrabold cursor-pointer"
                    ]
                    [ text (T.leaveRoomButton lang) ]
                ]

          else
            div [ Attr.class "flex flex-col gap-3" ]
                [ button
                    [ Attr.type_ "button"
                    , onClick cfg.actions.onHostRoom
                    , Attr.attribute "data-testid" "host-room-button"
                    , Attr.class "h-11 px-4 rounded-xl bg-brand-yellow text-brand text-sm font-extrabold cursor-pointer shadow-md"
                    ]
                    [ text (T.hostRoomButton lang) ]
                , div [ Attr.class "flex items-center gap-2 pt-2 border-t border-white/10" ]
                    [ input
                        [ Attr.type_ "text"
                        , Attr.value cfg.roomCodeInput
                        , onInput cfg.actions.onRoomCodeInput
                        , Attr.placeholder (T.joinRoomPlaceholder lang)
                        , Attr.attribute "data-testid" "join-room-input"
                        , Attr.class "flex-1 h-10 px-3 rounded-xl bg-slate-900 border border-white/20 text-xs font-mono text-white"
                        ]
                        []
                    , button
                        [ Attr.type_ "button"
                        , onClick cfg.actions.onJoinRoom
                        , Attr.attribute "data-testid" "join-room-button"
                        , Attr.class "h-10 px-4 rounded-xl bg-sky-600 hover:bg-sky-500 text-white text-xs font-extrabold cursor-pointer"
                        ]
                        [ text (T.joinRoomButton lang) ]
                    ]
                ]
        ]


cityFileOverlay :
    { a
        | city : CityGrid
        , language : Language
        , actions : Actions msg
    }
    -> Html msg
cityFileOverlay cfg =
    let
        lang =
            cfg.language

        menuBtn label icon msg testId =
            button
                [ Attr.type_ "button"
                , onClick msg
                , Attr.attribute "data-testid" testId
                , Attr.class "inline-flex items-center gap-2.5 h-11 px-3.5 rounded-xl bg-slate-800/90 border border-white/15 hover:border-brand-yellow/60 text-xs font-bold text-white cursor-pointer"
                ]
                [ span [ Attr.class "text-brand-yellow shrink-0" ]
                    [ icon |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
                , span [] [ text label ]
                ]
    in
    modalBackdrop (cfg.actions.onToggleModal NoModal)
        (T.tr lang "City Layout & Export" "Kaupunki ja tiedostot")
        [ div [ Attr.class "flex flex-col gap-2" ]
            [ div [ Attr.class "flex items-center justify-between gap-2 pb-2 border-b border-white/10 md:hidden" ]
                [ span [ Attr.class "text-xs text-slate-300 font-bold" ] [ text (T.gridSizeLabel lang) ]
                , div [ Attr.class "flex gap-1" ]
                    (List.map
                        (\s ->
                            button
                                [ Attr.type_ "button"
                                , onClick (cfg.actions.onResizeGrid s)
                                , Attr.class
                                    ("px-2.5 py-1 rounded-lg text-xs font-mono font-bold "
                                        ++ (if cfg.city.cols == s then
                                                "bg-brand-yellow text-brand"

                                            else
                                                "bg-slate-800 text-slate-300"
                                           )
                                    )
                                ]
                                [ text (String.fromInt s ++ "×" ++ String.fromInt s) ]
                        )
                        [ 4, 5, 6, 8 ]
                    )
                ]
            , menuBtn (T.actionStarterTown lang) FeatherIcons.map cfg.actions.onLoadStarterCity "starter-town-button"
            , menuBtn (T.actionNewCity lang) FeatherIcons.filePlus cfg.actions.onNewEmptyCity "new-city-button"
            , menuBtn (T.actionExportMpd lang) FeatherIcons.box cfg.actions.onExportMpd "export-mpd-button"
            , menuBtn (T.actionExportJson lang) FeatherIcons.download cfg.actions.onExportJson "export-json-button"
            , menuBtn (T.actionImportJson lang) FeatherIcons.upload cfg.actions.onImportJson "import-json-button"
            ]
        ]


modalBackdrop : msg -> String -> List (Html msg) -> Html msg
modalBackdrop onClose heading body =
    div [ Attr.class "fixed inset-0 z-40 flex items-center justify-center bg-black/60 backdrop-blur-sm p-4" ]
        [ div [ Attr.class "w-full max-w-sm p-4 rounded-2xl bg-brand border border-white/20 shadow-2xl flex flex-col gap-3" ]
            [ div [ Attr.class "flex items-center justify-between" ]
                [ span [ Attr.class "text-sm font-extrabold text-white" ] [ text heading ]
                , button
                    [ Attr.type_ "button"
                    , onClick onClose
                    , Attr.class "w-8 h-8 flex items-center justify-center rounded-lg bg-white/10 text-slate-300 cursor-pointer"
                    ]
                    [ FeatherIcons.x |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
                ]
            , div [ Attr.class "flex flex-col gap-2" ] body
            ]
        ]
