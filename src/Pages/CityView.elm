module Pages.CityView exposing
    ( Actions
    , CameraMode(..)
    , CityMenuModal(..)
    , viewCity
    )

{-| Top-level City View HUD and 3D diorama workspace (`Pages.CityView`).

Displays the full-screen 3D interactive city (`City.Render`) framed by light,
frosted-glass overlays:

  - Clean top HUD with Main Menu button, editable city name, live stat pill
    (Population, Buildings, Green Space, Score), collab status pill, and menu.
  - Minimal bottom-center floating action bar with segmented camera mode
    toggles (Orbit / Walk / Top-down), reset view, and quick lot actions.
  - Compact floating Lot Inspector card with coordinate badge, category icon &
    pill, part count, primary "Build / Edit Element" CTA, Rotate/Copy/Paste/Clear
    quick actions, and an expandable 9-template quick-stamp drawer.
  - Unified frosted modal overlay for City Layout/JSON/MPD Export-Import and
    collaborative Room Sharing with QR invite code.

-}

import City.I18n as T
import City.Mesh exposing (MeshCache)
import City.Render as Render
import City.Templates as Templates exposing (Template)
import City.Types exposing (CityGrid, CityRoom, CityStats, ElementCategory(..), LotCoord, LotElement, RoomConnection(..), RoomRole(..))
import DesignTokens.Colors as Brand
import Dict
import FeatherIcons
import Html exposing (Html, button, div, img, input, p, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Html.Lazy as Lazy
import I18n exposing (Language(..))
import Json.Decode as Decode
import Pages.MainMenu as MainMenu
import QRCode
import Render.Camera as Camera exposing (Camera, Navigation(..))
import Svg.Attributes as SvgAttr
import UI.CameraGlobe as CameraGlobe


{-| Optional open modal/panel in the City View overlay.
-}
type CityMenuModal
    = NoModal
    | CollabModal
    | CityFileModal


{-| Camera viewing mode selectable from the bottom-center action bar.
-}
type CameraMode
    = OrbitMode
    | WalkMode
    | TopDownMode


{-| Callbacks from `Pages.CityView` back to `Main`.
-}
type alias Actions msg =
    { onOpenMainMenu : msg
    , onCityNameChanged : String -> msg
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
    , onSetCameraMode : CameraMode -> msg
    , onEditSelectedLot : msg
    , onRotateSelectedLot : msg
    , onCopySelectedLot : msg
    , onPasteSelectedLot : msg
    , onClearSelectedLot : msg
    , onStampTemplate : Template -> msg
    , onToggleLotTemplates : msg
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
    , onNoOp : msg
    }


{-| Render the interactive 3D City View and HUD overlays.
-}
viewCity :
    { city : CityGrid
    , stats : CityStats
    , meshes : MeshCache
    , camera : Camera
    , selectedLot : Maybe LotCoord
    , hoveredLot : Maybe LotCoord
    , clipboard : Maybe LotElement
    , showLotTemplates : Bool
    , room : CityRoom
    , roomCodeInput : String
    , openModal : CityMenuModal
    , toast : Maybe String
    , audioMuted : Bool
    , language : Language
    , baseUrl : String
    , time : Float
    , width : Int
    , height : Int
    , actions : Actions msg
    }
    -> Html msg
viewCity cfg =
    let
        activeMode =
            if cfg.camera.navigation == Walk then
                WalkMode

            else if Render.isTopDownCamera cfg.camera then
                TopDownMode

            else
                OrbitMode
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
        , Lazy.lazy6 topHud cfg.city.cityName cfg.room cfg.stats cfg.audioMuted cfg.language cfg.actions
        , Lazy.lazy2 walkBanner cfg.language (cfg.camera.navigation == Walk)
        , Lazy.lazy toastView cfg.toast
        , case cfg.selectedLot of
            Just coord ->
                Lazy.lazy6 lotInspectorCard cfg.city coord (cfg.clipboard /= Nothing) cfg.showLotTemplates cfg.language cfg.actions

            Nothing ->
                text ""
        , Lazy.lazy6 bottomActionBar activeMode cfg.city cfg.selectedLot (cfg.clipboard /= Nothing) cfg.language cfg.actions
        , Lazy.lazy2 cameraGlobeCorner cfg.camera cfg.actions
        , case cfg.openModal of
            NoModal ->
                text ""

            modalTab ->
                cityMenuOverlay
                    { city = cfg.city
                    , stats = cfg.stats
                    , room = cfg.room
                    , roomCodeInput = cfg.roomCodeInput
                    , language = cfg.language
                    , baseUrl = cfg.baseUrl
                    , actions = cfg.actions
                    }
                    modalTab
        ]


topHud :
    String
    -> CityRoom
    -> CityStats
    -> Bool
    -> Language
    -> Actions msg
    -> Html msg
topHud cityName room stats audioMuted lang actions =
    let
        isCollab =
            room.role /= LocalSolo

        roomBadgeLabel =
            case room.status of
                Connected ->
                    room.roomId

                Connecting ->
                    T.connectingLabel lang

                _ ->
                    T.roomHeading lang
    in
    div
        [ Attr.class "absolute top-0 left-0 right-0 z-20 flex flex-wrap items-center justify-between gap-2 p-2.5 sm:p-3 pointer-events-none"
        , Attr.attribute "data-testid" "city-top-hud"
        ]
        [ div [ Attr.class "pointer-events-auto order-1 inline-flex items-center gap-2 h-10 px-2.5 rounded-full bg-slate-900/80 backdrop-blur-md border border-white/15 shadow-lg min-w-0" ]
            [ button
                [ Attr.type_ "button"
                , onClick actions.onOpenMainMenu
                , Attr.attribute "data-testid" "nav-menu-button"
                , Attr.title (T.menuButton lang)
                , Attr.attribute "aria-label" (T.menuButton lang)
                , Attr.class "touch-target inline-flex items-center gap-1.5 h-7 px-2.5 rounded-full bg-white/10 hover:bg-brand-yellow hover:text-brand text-xs font-extrabold text-white cursor-pointer transition-colors shrink-0"
                ]
                [ img [ Attr.src "/favicon.svg", Attr.alt "", Attr.class "w-4 h-4 rounded-full shrink-0" ] []
                , span [ Attr.class "hidden sm:inline" ] [ text (T.menuButton lang) ]
                ]
            , input
                [ Attr.type_ "text"
                , Attr.value cityName
                , onInput actions.onCityNameChanged
                , Attr.attribute "aria-label" (T.cityNameLabel lang)
                , Attr.attribute "data-testid" "city-name-input"
                , Attr.class "w-28 sm:w-36 bg-transparent border-b border-transparent focus:border-brand-yellow hover:border-white/25 outline-none text-xs sm:text-sm font-extrabold text-white truncate"
                ]
                []
            , span [ Attr.class "text-slate-400 hidden sm:inline shrink-0" ]
                [ FeatherIcons.edit |> FeatherIcons.withSize 12 |> FeatherIcons.toHtml [] ]
            ]
        , statsPill lang stats
        , div [ Attr.class "pointer-events-auto order-2 lg:order-3 flex items-center gap-1.5" ]
            [ button
                [ Attr.type_ "button"
                , onClick (actions.onToggleModal CollabModal)
                , Attr.attribute "data-testid" "collab-modal-button"
                , Attr.class
                    ("touch-target inline-flex items-center gap-1.5 h-10 px-3 rounded-full border backdrop-blur-md text-xs font-extrabold cursor-pointer shadow-lg transition-all "
                        ++ (if isCollab then
                                "bg-emerald-600/85 border-emerald-300/60 text-white"

                            else
                                "bg-slate-900/80 border-white/15 text-brand-yellow hover:border-brand-yellow/60"
                           )
                    )
                ]
                [ if isCollab then
                    span [ Attr.class "w-2 h-2 rounded-full bg-emerald-200 animate-pulse shrink-0" ] []

                  else
                    FeatherIcons.users |> FeatherIcons.withSize 15 |> FeatherIcons.toHtml []
                , span [ Attr.class "hidden sm:inline" ] [ text roomBadgeLabel ]
                , if isCollab then
                    span [ Attr.class "px-1.5 py-0.5 rounded-full bg-black/30 text-[10px] font-mono tabular-nums" ]
                        [ text (String.fromInt (List.length room.peers)) ]

                  else
                    text ""
                ]
            , button
                [ Attr.type_ "button"
                , onClick (actions.onToggleModal CityFileModal)
                , Attr.attribute "data-testid" "city-menu-button"
                , Attr.title (T.cityFilesTab lang)
                , Attr.attribute "aria-label" (T.cityFilesTab lang)
                , Attr.class "touch-target inline-flex items-center gap-1.5 h-10 px-3 rounded-full bg-slate-900/80 backdrop-blur-md border border-white/15 text-xs font-bold text-white cursor-pointer shadow-lg hover:border-white/40"
                ]
                [ FeatherIcons.sliders |> FeatherIcons.withSize 15 |> FeatherIcons.toHtml []
                , span [ Attr.class "hidden sm:inline" ] [ text (T.tr lang "City" "Kaupunki") ]
                ]
            , div [ Attr.class "inline-flex items-center gap-0.5 p-1 rounded-full bg-slate-900/80 backdrop-blur-md border border-white/15 shadow-lg" ]
                [ button
                    [ Attr.type_ "button"
                    , onClick actions.onToggleLanguage
                    , Attr.attribute "data-testid" "language-toggle-button"
                    , Attr.class "touch-target h-8 px-2.5 rounded-full text-xs font-black text-slate-200 cursor-pointer hover:text-brand-yellow hover:bg-white/10 transition-colors"
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
                    , onClick actions.onToggleMute
                    , Attr.title (T.muteLabel lang audioMuted)
                    , Attr.attribute "aria-label" (T.muteLabel lang audioMuted)
                    , Attr.class "touch-target w-8 h-8 flex items-center justify-center rounded-full text-slate-200 cursor-pointer hover:text-brand-yellow hover:bg-white/10 transition-colors"
                    ]
                    [ (if audioMuted then
                        FeatherIcons.volumeX

                       else
                        FeatherIcons.volume2
                      )
                        |> FeatherIcons.withSize 15
                        |> FeatherIcons.toHtml []
                    ]
                ]
            ]
        ]


statsPill : Language -> City.Types.CityStats -> Html msg
statsPill lang stats =
    let
        statItem icon colorHex label value =
            div
                [ Attr.class "inline-flex items-center gap-1.5"
                , Attr.title (label ++ ": " ++ value)
                ]
                [ span [ Attr.style "color" colorHex, Attr.class "shrink-0" ]
                    [ icon |> FeatherIcons.withSize 13 |> FeatherIcons.toHtml [] ]
                , span [ Attr.class "hidden xl:inline text-slate-300 font-medium" ] [ text label ]
                , span [ Attr.class "font-mono font-bold tabular-nums text-white" ] [ text value ]
                ]

        divider =
            span [ Attr.class "w-px h-3.5 bg-white/15 shrink-0" ] []
    in
    div
        [ Attr.class "pointer-events-auto order-3 lg:order-2 w-full lg:w-auto flex justify-center lg:justify-start"
        ]
        [ div
            [ Attr.class "inline-flex items-center gap-2.5 sm:gap-3 h-9 sm:h-10 px-3.5 rounded-full bg-slate-900/80 backdrop-blur-md border border-white/15 shadow-lg text-xs"
            , Attr.attribute "data-testid" "city-stats-bar"
            ]
            [ statItem FeatherIcons.users Brand.rainbowBrightLightBlue (T.statPopulation lang) (String.fromInt stats.population)
            , divider
            , statItem FeatherIcons.home Brand.legoYellow (T.statBuildings lang) (String.fromInt stats.buildings)
            , divider
            , statItem FeatherIcons.sun Brand.rainbowMediumGreen (T.statGreen lang) (String.fromInt stats.parks ++ "%")
            , divider
            , statItem FeatherIcons.award Brand.rainbowLightOrange (T.statScore lang) (String.fromInt stats.score)
            ]
        ]


walkBanner : Language -> Bool -> Html msg
walkBanner lang isWalk =
    if isWalk then
        div
            [ Attr.class "absolute top-24 lg:top-16 left-1/2 -translate-x-1/2 z-20 px-4 py-1.5 rounded-full bg-slate-900/85 backdrop-blur-md border border-brand-yellow/45 text-brand-yellow text-xs font-bold shadow-lg pointer-events-none whitespace-nowrap max-w-[calc(100vw-2rem)] truncate"
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
                , Attr.class "absolute top-24 lg:top-16 left-1/2 -translate-x-1/2 z-30 px-4 py-1.5 rounded-full bg-slate-900/90 backdrop-blur-md border border-brand-yellow/60 text-brand-yellow text-xs font-bold shadow-xl pointer-events-none"
                ]
                [ text msg ]

        Nothing ->
            text ""


bottomActionBar :
    CameraMode
    -> CityGrid
    -> Maybe LotCoord
    -> Bool
    -> Language
    -> Actions msg
    -> Html msg
bottomActionBar activeMode city selectedLot hasClipboard lang actions =
    let
        modeBtn mode testId icon fullLabel shortLabel =
            let
                active =
                    activeMode == mode
            in
            button
                [ Attr.type_ "button"
                , onClick (actions.onSetCameraMode mode)
                , Attr.attribute "data-testid" testId
                , Attr.attribute "aria-pressed" (boolAttr active)
                , Attr.title fullLabel
                , Attr.attribute "aria-label" fullLabel
                , Attr.class
                    ("touch-target inline-flex items-center gap-1.5 h-9 px-2.5 sm:px-3 rounded-full text-xs font-bold cursor-pointer transition-all shrink-0 "
                        ++ (if active then
                                "bg-brand-yellow text-brand shadow-sm font-extrabold"

                            else
                                "text-slate-200 hover:bg-white/10"
                           )
                    )
                ]
                [ icon |> FeatherIcons.withSize 15 |> FeatherIcons.toHtml []
                , span [ Attr.class "hidden sm:inline" ] [ text shortLabel ]
                ]

        selectedElement =
            selectedLot
                |> Maybe.andThen (\coord -> Dict.get ( coord.row, coord.col ) city.lots)

        barIconBtn label icon msg testId enabled =
            button
                ([ Attr.type_ "button"
                 , Attr.title label
                 , Attr.attribute "aria-label" label
                 , Attr.attribute "data-testid" testId
                 , Attr.disabled (not enabled)
                 , Attr.class
                    ("touch-target w-9 h-9 inline-flex items-center justify-center rounded-full transition-all shrink-0 "
                        ++ (if enabled then
                                "text-slate-100 hover:bg-white/15 cursor-pointer"

                            else
                                "text-slate-500 opacity-40 cursor-default"
                           )
                    )
                 ]
                    ++ (if enabled then
                            [ onClick msg ]

                        else
                            []
                       )
                )
                [ icon |> FeatherIcons.withSize 15 |> FeatherIcons.toHtml [] ]
    in
    div [ Attr.class "pointer-events-none fixed bottom-3 left-0 right-0 z-20 flex justify-center px-3" ]
        [ div
            [ Attr.attribute "data-testid" "city-bottom-bar"
            , Attr.class "pointer-events-auto inline-flex items-center gap-1 p-1.5 rounded-full bg-slate-900/80 backdrop-blur-md border border-white/15 shadow-2xl max-w-full overflow-x-auto palette-scroll-x"
            ]
            [ modeBtn OrbitMode "camera-mode-orbit" FeatherIcons.globe (T.modeOrbitLabel lang) (T.modeOrbitLabel lang)
            , modeBtn WalkMode "toggle-walk-button" FeatherIcons.compass (T.modeWalkLabel lang) (T.modeWalkShort lang)
            , modeBtn TopDownMode "camera-mode-topdown" FeatherIcons.map (T.modeTopDownLabel lang) (T.modeTopDownShort lang)
            , span [ Attr.class "w-px h-5 bg-white/15 mx-0.5 shrink-0" ] []
            , barIconBtn (T.fitViewLabel lang) FeatherIcons.maximize actions.onGlobeReset "camera-fit-button" True
            , case selectedLot of
                Just _ ->
                    div [ Attr.class "inline-flex items-center gap-0.5 pl-1 ml-0.5 border-l border-white/15 shrink-0" ]
                        [ barIconBtn (T.actionBuildElement lang (selectedElement /= Nothing)) FeatherIcons.tool actions.onEditSelectedLot "bar-edit-lot-button" True
                        , barIconBtn (T.actionRotateLot lang) FeatherIcons.rotateCw actions.onRotateSelectedLot "bar-rotate-lot-button" (selectedElement /= Nothing)
                        , barIconBtn (T.actionPasteLot lang) FeatherIcons.clipboard actions.onPasteSelectedLot "bar-paste-lot-button" hasClipboard
                        ]

                Nothing ->
                    span [ Attr.class "hidden md:inline-flex items-center pl-2.5 pr-2 ml-0.5 border-l border-white/15 text-[11px] text-slate-300 font-medium whitespace-nowrap" ]
                        [ text (T.selectLotHint lang) ]
            ]
        ]


lotInspectorCard :
    CityGrid
    -> LotCoord
    -> Bool
    -> Bool
    -> Language
    -> Actions msg
    -> Html msg
lotInspectorCard city coord hasClipboard showLotTemplates lang actions =
    let
        maybeEl =
            Dict.get ( coord.row, coord.col ) city.lots

        isBuilt =
            maybeEl /= Nothing

        titleText =
            case maybeEl of
                Just el ->
                    el.name

                Nothing ->
                    T.emptyLotTitle lang

        categoryBadge =
            case maybeEl of
                Just el ->
                    let
                        meta =
                            categoryStyle el.category
                    in
                    span [ Attr.class ("inline-flex items-center gap-1 px-2 py-0.5 rounded-full border text-[10px] font-bold shrink-0 " ++ meta.pillClass) ]
                        [ meta.icon |> FeatherIcons.withSize 11 |> FeatherIcons.toHtml []
                        , span [] [ text (T.categoryLabel lang el.category) ]
                        ]

                Nothing ->
                    span [ Attr.class "inline-flex items-center gap-1 px-2 py-0.5 rounded-full border border-white/15 bg-white/5 text-[10px] font-semibold text-slate-300 shrink-0" ]
                        [ FeatherIcons.square |> FeatherIcons.withSize 10 |> FeatherIcons.toHtml []
                        , span [] [ text (T.tr lang "Empty" "Vapaa") ]
                        ]
    in
    div
        [ Attr.attribute "data-testid" "lot-inspector"
        , Attr.class "fixed bottom-20 left-3 right-3 sm:right-auto sm:w-80 lg:bottom-4 lg:left-4 lg:w-72 z-20 p-3.5 rounded-2xl bg-slate-900/85 backdrop-blur-md border border-white/15 shadow-2xl flex flex-col gap-2.5"
        ]
        [ div [ Attr.class "flex items-start justify-between gap-2" ]
            [ div [ Attr.class "min-w-0 flex flex-col gap-1" ]
                [ div [ Attr.class "flex flex-wrap items-center gap-1.5" ]
                    [ span [ Attr.class "px-2 py-0.5 rounded-full bg-brand-yellow/15 border border-brand-yellow/35 text-[10px] font-mono font-extrabold text-brand-yellow" ]
                        [ text (T.lotLabel lang coord.col coord.row) ]
                    , categoryBadge
                    ]
                , p [ Attr.class "text-sm font-extrabold text-white truncate" ] [ text titleText ]
                , case maybeEl of
                    Just el ->
                        p [ Attr.class "text-[11px] font-mono text-slate-300" ]
                            [ text (T.heightUnit lang el.heightPlates ++ " • " ++ T.partCountShort lang el.partCount) ]

                    Nothing ->
                        p [ Attr.class "text-[11px] text-slate-300 leading-snug" ] [ text (T.emptyLotHint lang) ]
                ]
            , button
                [ Attr.type_ "button"
                , onClick actions.onDeselectLot
                , Attr.title (T.tr lang "Close" "Sulje")
                , Attr.attribute "aria-label" (T.tr lang "Close" "Sulje")
                , Attr.class "touch-target w-7 h-7 shrink-0 flex items-center justify-center rounded-full bg-white/10 text-slate-300 cursor-pointer hover:bg-white/20"
                ]
                [ FeatherIcons.x |> FeatherIcons.withSize 14 |> FeatherIcons.toHtml [] ]
            ]
        , button
            [ Attr.type_ "button"
            , onClick actions.onEditSelectedLot
            , Attr.attribute "data-testid" "edit-lot-3d-button"
            , Attr.class "touch-target w-full inline-flex items-center justify-center gap-2 h-10 px-4 rounded-xl bg-brand-yellow text-brand font-extrabold text-xs sm:text-sm cursor-pointer shadow-md active:brightness-110"
            ]
            [ FeatherIcons.tool |> FeatherIcons.withSize 15 |> FeatherIcons.toHtml []
            , span [] [ text (T.actionBuildElement lang isBuilt) ]
            ]
        , div [ Attr.class "flex items-center justify-between gap-1.5" ]
            [ div [ Attr.class "flex items-center gap-1" ]
                [ quickLotBtn (T.actionRotateLot lang) FeatherIcons.rotateCw actions.onRotateSelectedLot "rotate-lot-button" isBuilt False
                , quickLotBtn (T.actionCopyLot lang) FeatherIcons.copy actions.onCopySelectedLot "copy-lot-button" isBuilt False
                , quickLotBtn (T.actionPasteLot lang) FeatherIcons.clipboard actions.onPasteSelectedLot "paste-lot-button" hasClipboard False
                , quickLotBtn (T.actionClearLot lang) FeatherIcons.trash2 actions.onClearSelectedLot "clear-lot-button" isBuilt True
                ]
            , button
                [ Attr.type_ "button"
                , onClick actions.onToggleLotTemplates
                , Attr.attribute "data-testid" "toggle-lot-templates"
                , Attr.attribute "aria-expanded" (boolAttr showLotTemplates)
                , Attr.class
                    ("touch-target inline-flex items-center gap-1 h-9 px-2.5 rounded-xl border text-[11px] font-bold cursor-pointer transition-all "
                        ++ (if showLotTemplates then
                                "bg-brand-yellow/20 border-brand-yellow/50 text-brand-yellow"

                            else
                                "bg-white/10 border-white/15 text-slate-200 hover:bg-white/15"
                           )
                    )
                ]
                [ FeatherIcons.layers |> FeatherIcons.withSize 13 |> FeatherIcons.toHtml []
                , span [] [ text (T.lotTemplatesToggle lang) ]
                ]
            ]
        , if showLotTemplates then
            div [ Attr.class "pt-1.5 border-t border-white/10 grid grid-cols-3 gap-1.5 max-h-40 overflow-y-auto palette-scroll-y" ]
                (List.map (templateQuickButton lang actions.onStampTemplate) Templates.all)

          else
            text ""
        ]


categoryStyle : ElementCategory -> { icon : FeatherIcons.Icon, pillClass : String }
categoryStyle cat =
    case cat of
        Residential ->
            { icon = FeatherIcons.home, pillClass = "bg-sky-500/20 border-sky-400/35 text-sky-200" }

        Commercial ->
            { icon = FeatherIcons.trendingUp, pillClass = "bg-amber-500/20 border-amber-400/35 text-amber-200" }

        Civic ->
            { icon = FeatherIcons.award, pillClass = "bg-purple-500/20 border-purple-400/35 text-purple-200" }

        Park ->
            { icon = FeatherIcons.sun, pillClass = "bg-emerald-500/20 border-emerald-400/35 text-emerald-200" }

        Road ->
            { icon = FeatherIcons.gitBranch, pillClass = "bg-slate-500/25 border-slate-400/35 text-slate-200" }

        Industrial ->
            { icon = FeatherIcons.settings, pillClass = "bg-orange-500/20 border-orange-400/35 text-orange-200" }

        Waterfront ->
            { icon = FeatherIcons.anchor, pillClass = "bg-cyan-500/20 border-cyan-400/35 text-cyan-200" }

        Custom ->
            { icon = FeatherIcons.box, pillClass = "bg-indigo-500/20 border-indigo-400/35 text-indigo-200" }


quickLotBtn : String -> FeatherIcons.Icon -> msg -> String -> Bool -> Bool -> Html msg
quickLotBtn label icon msg testId enabled danger =
    let
        toneClass =
            if not enabled then
                "bg-white/5 border-white/10 text-slate-500 opacity-40 cursor-default"

            else if danger then
                "bg-white/10 border-white/15 text-rose-300 hover:bg-rose-500/25 hover:border-rose-400/40 cursor-pointer"

            else
                "bg-white/10 border-white/15 text-slate-100 hover:bg-white/20 cursor-pointer"
    in
    button
        ([ Attr.type_ "button"
         , Attr.title label
         , Attr.attribute "aria-label" label
         , Attr.attribute "data-testid" testId
         , Attr.disabled (not enabled)
         , Attr.class ("touch-target w-9 h-9 flex items-center justify-center rounded-xl border transition-all " ++ toneClass)
         ]
            ++ (if enabled then
                    [ onClick msg ]

                else
                    []
               )
        )
        [ icon |> FeatherIcons.withSize 14 |> FeatherIcons.toHtml [] ]


templateQuickButton : Language -> (Template -> msg) -> Template -> Html msg
templateQuickButton lang onStamp tmpl =
    button
        [ Attr.type_ "button"
        , onClick (onStamp tmpl)
        , Attr.attribute "data-testid" ("stamp-" ++ tmpl.id)
        , Attr.class "px-2 py-1.5 rounded-xl bg-slate-800/80 border border-white/10 hover:border-brand-yellow/60 text-left cursor-pointer transition-all"
        ]
        [ p [ Attr.class "text-[11px] font-bold text-slate-100 truncate" ]
            [ text (T.tr lang tmpl.nameEn tmpl.nameFi) ]
        , span [ Attr.class "text-[10px] font-mono text-sky-300" ]
            [ text (String.fromInt tmpl.heightPlates ++ "/32") ]
        ]


cameraGlobeCorner : Camera -> Actions msg -> Html msg
cameraGlobeCorner camera actions =
    div [ Attr.class "hidden md:block absolute right-3 bottom-3 z-20" ]
        [ CameraGlobe.view
            { camera = camera
            , onPress = actions.onGlobePress
            , onMove = actions.onGlobeMove
            , onRelease = actions.onGlobeRelease
            , onZoom = actions.onGlobeZoom
            , onZoomDone = actions.onGlobeZoomDone
            , onReset = actions.onGlobeReset
            , extras = []
            }
        ]


cityMenuOverlay :
    { a
        | city : CityGrid
        , stats : CityStats
        , room : CityRoom
        , roomCodeInput : String
        , language : Language
        , baseUrl : String
        , actions : Actions msg
    }
    -> CityMenuModal
    -> Html msg
cityMenuOverlay cfg activeTab =
    let
        lang =
            cfg.language

        tabButton tab label =
            let
                selected =
                    activeTab == tab
            in
            button
                [ Attr.type_ "button"
                , onClick (cfg.actions.onToggleModal tab)
                , Attr.class
                    ("flex-1 h-9 px-3 rounded-full text-xs font-extrabold cursor-pointer transition-all "
                        ++ (if selected then
                                "bg-brand-yellow text-brand shadow-sm"

                            else
                                "text-slate-300 hover:text-white"
                           )
                    )
                ]
                [ text label ]
    in
    div
        [ Attr.class "fixed inset-0 z-40 flex items-center justify-center bg-black/55 backdrop-blur-sm p-4"
        , onClick (cfg.actions.onToggleModal NoModal)
        ]
        [ div
            [ Attr.class "w-full max-w-md p-5 rounded-3xl bg-slate-900/90 backdrop-blur-xl border border-white/15 shadow-2xl flex flex-col gap-4"
            , Html.Events.stopPropagationOn "click" (Decode.succeed ( cfg.actions.onNoOp, True ))
            ]
            [ div [ Attr.class "flex items-center justify-between gap-2" ]
                [ div [ Attr.class "flex items-center gap-2 min-w-0" ]
                    [ img [ Attr.src "/favicon.svg", Attr.alt "", Attr.class "w-6 h-6 rounded-full shrink-0" ] []
                    , span [ Attr.class "text-sm font-extrabold text-white truncate" ] [ text cfg.city.cityName ]
                    ]
                , button
                    [ Attr.type_ "button"
                    , onClick (cfg.actions.onToggleModal NoModal)
                    , Attr.title (T.tr lang "Close" "Sulje")
                    , Attr.attribute "aria-label" (T.tr lang "Close" "Sulje")
                    , Attr.class "touch-target w-8 h-8 flex items-center justify-center rounded-full bg-white/10 text-slate-300 cursor-pointer hover:bg-white/20 shrink-0"
                    ]
                    [ FeatherIcons.x |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
                ]
            , div [ Attr.class "flex items-center gap-1 p-1 rounded-full bg-slate-950/75 border border-white/10" ]
                [ tabButton CityFileModal (T.cityFilesTab lang)
                , tabButton CollabModal (T.roomSharingTab lang)
                ]
            , case activeTab of
                CollabModal ->
                    Lazy.lazy5 collabPanel cfg.room cfg.roomCodeInput cfg.language cfg.baseUrl cfg.actions

                _ ->
                    Lazy.lazy4 cityFilesPanel cfg.city cfg.stats cfg.language cfg.actions
            ]
        ]


cityFilesPanel : CityGrid -> CityStats -> Language -> Actions msg -> Html msg
cityFilesPanel city stats lang actions =
    let
        menuBtn label icon msg testId =
            button
                [ Attr.type_ "button"
                , onClick msg
                , Attr.attribute "data-testid" testId
                , Attr.class "touch-target inline-flex items-center gap-2.5 h-11 px-3.5 rounded-2xl bg-slate-800/80 border border-white/15 hover:border-brand-yellow/60 text-xs font-bold text-white cursor-pointer transition-all"
                ]
                [ span [ Attr.class "text-brand-yellow shrink-0" ]
                    [ icon |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
                , span [] [ text label ]
                ]
    in
    div [ Attr.class "flex flex-col gap-3" ]
        [ div [ Attr.class "flex items-center justify-between gap-2 p-2.5 rounded-2xl bg-slate-950/60 border border-white/10" ]
            [ span [ Attr.class "text-xs font-bold text-slate-300 pl-1" ] [ text (T.gridSizeLabel lang) ]
            , div [ Attr.class "flex items-center gap-1" ]
                (List.map
                    (\size ->
                        let
                            active =
                                city.cols == size
                        in
                        button
                            [ Attr.type_ "button"
                            , onClick (actions.onResizeGrid size)
                            , Attr.attribute "data-testid" ("grid-size-" ++ String.fromInt size)
                            , Attr.class
                                ("px-2.5 py-1 rounded-full text-xs font-mono font-bold cursor-pointer transition-all "
                                    ++ (if active then
                                            "bg-brand-yellow text-brand"

                                        else
                                            "bg-white/10 text-slate-300 hover:bg-white/20"
                                       )
                                )
                            ]
                            [ text (String.fromInt size ++ "×" ++ String.fromInt size) ]
                    )
                    [ 4, 5, 6, 8 ]
                )
            ]
        , div [ Attr.class "grid grid-cols-3 gap-2 text-center p-2.5 rounded-2xl bg-slate-950/45 border border-white/10" ]
            [ miniMetric (T.statRoads lang) (String.fromInt stats.roadConnections)
            , miniMetric (T.statPeak lang) (String.fromInt stats.maxHeightPlates ++ "/32")
            , miniMetric (T.tr lang "Bricks" "Paloja") (String.fromInt stats.totalParts)
            ]
        , div [ Attr.class "grid grid-cols-2 gap-2" ]
            [ menuBtn (T.actionStarterTown lang) FeatherIcons.map actions.onLoadStarterCity "starter-town-button"
            , menuBtn (T.actionNewCity lang) FeatherIcons.filePlus actions.onNewEmptyCity "new-city-button"
            ]
        , div [ Attr.class "flex flex-col gap-2 pt-1 border-t border-white/10" ]
            [ menuBtn (T.actionExportMpd lang) FeatherIcons.box actions.onExportMpd "export-mpd-button"
            , div [ Attr.class "grid grid-cols-2 gap-2" ]
                [ menuBtn (T.actionExportJson lang) FeatherIcons.download actions.onExportJson "export-json-button"
                , menuBtn (T.actionImportJson lang) FeatherIcons.upload actions.onImportJson "import-json-button"
                ]
            ]
        ]


miniMetric : String -> String -> Html msg
miniMetric label value =
    div [ Attr.class "flex flex-col items-center" ]
        [ span [ Attr.class "text-[10px] text-slate-400 font-medium" ] [ text label ]
        , span [ Attr.class "text-xs font-mono font-bold text-white" ] [ text value ]
        ]


viewQrBlock : String -> Html msg
viewQrBlock roomUrl =
    case QRCode.fromString roomUrl of
        Ok qr ->
            div [ Attr.class "flex justify-center" ]
                [ div
                    [ Attr.style "background" Brand.legoWhite
                    , Attr.style "padding" "8px"
                    , Attr.style "border-radius" "14px"
                    ]
                    [ QRCode.toSvg
                        [ SvgAttr.width "124px"
                        , SvgAttr.height "124px"
                        ]
                        qr
                    ]
                ]

        Err _ ->
            text ""


collabPanel : CityRoom -> String -> Language -> String -> Actions msg -> Html msg
collabPanel room roomCodeInput lang baseUrl actions =
    let
        inRoom =
            room.role /= LocalSolo

        roomUrl =
            MainMenu.makeCityRoomUrl baseUrl room.roomId
    in
    if inRoom then
        div [ Attr.class "flex flex-col gap-3" ]
            [ div [ Attr.class "flex items-center justify-between gap-2 p-3 rounded-2xl bg-slate-950/75 border border-white/10" ]
                [ div []
                    [ span [ Attr.class "text-[11px] text-slate-400" ] [ text (T.tr lang "Room Code" "Huonekoodi") ]
                    , p [ Attr.class "text-lg font-mono font-black text-brand-yellow" ] [ text room.roomId ]
                    ]
                , button
                    [ Attr.type_ "button"
                    , onClick actions.onCopyRoomLink
                    , Attr.class "touch-target inline-flex items-center gap-1.5 h-9 px-3.5 rounded-full bg-brand-yellow text-brand text-xs font-extrabold cursor-pointer shadow-md"
                    ]
                    [ FeatherIcons.copy |> FeatherIcons.withSize 14 |> FeatherIcons.toHtml []
                    , span [] [ text (T.copyInviteLink lang) ]
                    ]
                ]
            , Lazy.lazy viewQrBlock roomUrl
            , div [ Attr.class "flex flex-col gap-1.5" ]
                [ span [ Attr.class "text-[11px] font-bold uppercase tracking-wider text-slate-400" ]
                    [ text (T.buildersHeading lang) ]
                , div [ Attr.class "flex flex-wrap gap-1.5" ]
                    (List.map
                        (\peer ->
                            div [ Attr.class "inline-flex items-center gap-1.5 px-2.5 py-1 rounded-full bg-slate-800/90 border border-white/15 text-xs" ]
                                [ span [ Attr.class "w-2.5 h-2.5 rounded-full", Attr.style "background" peer.color ] []
                                , span [ Attr.class "font-semibold" ]
                                    [ text
                                        (peer.name
                                            ++ (if peer.slot == room.mySlot then
                                                    " (" ++ T.youTag lang ++ ")"

                                                else
                                                    ""
                                               )
                                        )
                                    ]
                                ]
                        )
                        room.peers
                    )
                ]
            , button
                [ Attr.type_ "button"
                , onClick actions.onLeaveRoom
                , Attr.class "touch-target h-10 px-4 rounded-2xl bg-rose-600/85 hover:bg-rose-600 text-white text-xs font-extrabold cursor-pointer"
                ]
                [ text (T.leaveRoomButton lang) ]
            ]

    else
        div [ Attr.class "flex flex-col gap-3" ]
            [ p [ Attr.class "text-xs text-slate-300 leading-relaxed" ]
                [ text (T.collabMenuSubtitle lang) ]
            , button
                [ Attr.type_ "button"
                , onClick actions.onHostRoom
                , Attr.attribute "data-testid" "host-room-button"
                , Attr.class "touch-target h-11 px-4 rounded-2xl bg-brand-yellow text-brand text-sm font-extrabold cursor-pointer shadow-md"
                ]
                [ text (T.hostRoomButton lang) ]
            , div [ Attr.class "flex items-center gap-2 pt-2 border-t border-white/10" ]
                [ input
                    [ Attr.type_ "text"
                    , Attr.value roomCodeInput
                    , onInput actions.onRoomCodeInput
                    , Attr.placeholder (T.joinRoomPlaceholder lang)
                    , Attr.attribute "data-testid" "join-room-input"
                    , Attr.class "flex-1 h-10 px-3 rounded-xl bg-slate-950/80 border border-white/20 text-xs font-mono text-white outline-none focus:border-brand-yellow"
                    ]
                    []
                , button
                    [ Attr.type_ "button"
                    , onClick actions.onJoinRoom
                    , Attr.attribute "data-testid" "join-room-button"
                    , Attr.class "touch-target h-10 px-4 rounded-xl bg-sky-600 hover:bg-sky-500 text-white text-xs font-extrabold cursor-pointer"
                    ]
                    [ text (T.joinRoomButton lang) ]
                ]
            ]


boolAttr : Bool -> String
boolAttr on =
    if on then
        "true"

    else
        "false"
