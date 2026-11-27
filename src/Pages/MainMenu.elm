module Pages.MainMenu exposing
    ( MainMenuActions
    , MainMenuViewData
    , PreviewSource(..)
    , SavedCitySummary
    , makeCityRoomUrl
    , view
    )

{-| Main Menu and New Game setup screen (`Pages.MainMenu`) for Brick City.

Follows the layout and interaction architecture of `Pages.Garage` (`brick-racer`)
and `Pages.Shipyard` (`brick-sails`):

  - Compact brand header with FI / EN language selector and audio mute toggle.
  - Interactive 3D turntable preview diorama (`City.Render.renderCityPreview`)
    of either the player's saved city or the selected new city preset.
  - Prominent "Continue Game" ("Jatka peliä") card whenever a saved city
    exists in `localStorage` (showing city name, population, and timestamp).
  - Two-column options row with "New City" ("Uusi kaupunki" — city name input,
    4×4..8×8 grid size picker, 4 starter layout presets, and primary CTA) and
    "Collaborative Room" ("Yhteiskaupunki" — host with QR code / share link or
    join via 4+ letter code or pasted URL).

-}

import City.I18n as T
import City.Mesh as Mesh exposing (MeshCache)
import City.Render as Render
import City.Stats as Stats
import City.Templates as Templates exposing (CityPreset(..))
import City.Types exposing (CityGrid, CityRoom, CityStats, RoomConnection(..), RoomRole(..))
import Component.Navbar as Navbar
import DesignTokens.Colors as Brand
import Dict
import FeatherIcons
import Html exposing (Html, button, div, h2, input, label, p, section, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Html.Lazy as Lazy
import I18n exposing (Language(..))
import Json.Decode as Decode
import QRCode
import Render.Camera exposing (Camera)
import Set
import Svg.Attributes as SvgAttr
import Time
import WebGL


{-| Whether the 3D turntable previews the player's saved city or the new city
configuration.
-}
type PreviewSource
    = PreviewSaved
    | PreviewNew


{-| Snapshot of a locally saved city in `localStorage` for the Continue Game
card.
-}
type alias SavedCitySummary =
    { city : CityGrid
    , savedAt : Maybe Time.Posix
    }


{-| View-model snapshot passed from `Main` to render the Main Menu.
-}
type alias MainMenuViewData =
    { language : Language
    , audioMuted : Bool
    , savedCity : Maybe SavedCitySummary
    , previewSource : PreviewSource
    , previewCity : CityGrid
    , previewStats : CityStats
    , meshes : MeshCache
    , camera : Camera
    , time : Float
    , timeZone : Time.Zone
    , viewportWidth : Int
    , viewportHeight : Int
    , newCityName : String
    , gridSize : Int
    , preset : CityPreset
    , room : CityRoom
    , roomCodeInput : String
    , builderName : String
    , baseUrl : String
    , shareCopiedNotice : Bool
    , toast : Maybe String
    }


{-| Callbacks dispatched from `Pages.MainMenu` back to `Main`.
-}
type alias MainMenuActions msg =
    { onSetLanguage : Language -> msg
    , onToggleMute : msg
    , onContinueSaved : msg
    , onCityNameChanged : String -> msg
    , onSelectGridSize : Int -> msg
    , onSelectPreset : CityPreset -> msg
    , onSetPreviewSource : PreviewSource -> msg
    , onStartNewCity : msg
    , onImportJson : msg
    , onBuilderNameChanged : String -> msg
    , onHostRoom : msg
    , onRoomCodeChanged : String -> msg
    , onJoinRoom : msg
    , onCopyRoomLink : msg
    , onEnterRoomCity : msg
    , onLeaveRoom : msg
    , onMouseDown : Float -> Float -> msg
    , onMouseMove : Float -> Float -> msg
    , onMouseUp : msg
    , onWheel : Float -> msg
    }


{-| Construct the shareable collaborative room URL for invite links and QR codes.
-}
makeCityRoomUrl : String -> String -> String
makeCityRoomUrl baseUrl roomId =
    let
        trimmedBase =
            String.trim baseUrl

        cleanRoom =
            String.toUpper (String.trim roomId)
    in
    if String.isEmpty cleanRoom then
        ""

    else if String.isEmpty trimmedBase then
        "#/city/" ++ cleanRoom

    else if String.endsWith "/" trimmedBase || String.endsWith ".html" trimmedBase then
        trimmedBase ++ "#/city/" ++ cleanRoom

    else
        trimmedBase ++ "/#/city/" ++ cleanRoom


{-| Render the Main Menu / New Game screen.
-}
view : MainMenuViewData -> MainMenuActions msg -> Html msg
view data actions =
    let
        heroWidth =
            clamp 288 1024 (data.viewportWidth - 32)

        heroHeight =
            clamp 230 430 (round (toFloat data.viewportHeight * 0.4))
    in
    div
        [ Attr.class "fixed inset-0 overflow-y-auto overflow-x-hidden text-white font-sans select-none"
        , Attr.style "touch-action" "pan-y"
        , Attr.style "background" ("linear-gradient(170deg, " ++ Brand.legoBlack ++ " 0%, #08263a 50%, " ++ Brand.legoBlack ++ " 100%)")
        , Attr.attribute "data-testid" "main-menu-page"
        ]
        [ Lazy.lazy3 viewHeaderParts data.language data.audioMuted actions
        , Lazy.lazy viewToast data.toast
        , div
            [ Attr.class "w-full flex flex-col items-center px-4 py-4"
            , Attr.style "padding-left" "max(16px, env(safe-area-inset-left))"
            , Attr.style "padding-right" "max(16px, env(safe-area-inset-right))"
            , Attr.style "padding-bottom" "max(24px, env(safe-area-inset-bottom))"
            ]
            [ div
                [ Attr.class "w-full max-w-5xl flex flex-col gap-5"
                , Attr.attribute "data-testid" "main-content"
                ]
                [ viewPreviewHero data actions heroWidth heroHeight
                , case data.savedCity of
                    Just saved ->
                        Lazy.lazy4 viewContinueCard data.language data.timeZone saved actions.onContinueSaved

                    Nothing ->
                        text ""
                , div
                    [ Attr.attribute "data-testid" "options-row"
                    , Attr.class "w-full grid grid-cols-1 lg:grid-cols-2 gap-4 items-stretch"
                    ]
                    [ Lazy.lazy5 viewNewCityPanelParts data.language data.newCityName data.gridSize data.preset actions
                    , Lazy.lazy7 viewCollabPanelParts data.language data.room data.roomCodeInput data.builderName data.baseUrl data.shareCopiedNotice actions
                    ]
                ]
            ]
        ]


viewHeaderParts : Language -> Bool -> MainMenuActions msg -> Html msg
viewHeaderParts lang audioMuted actions =
    let
        langBtn targetCode targetLang =
            let
                active =
                    lang == targetLang
            in
            button
                [ Attr.type_ "button"
                , onClick (actions.onSetLanguage targetLang)
                , Attr.attribute "aria-pressed" (boolStr active)
                , Attr.attribute "data-testid" ("menu-lang-" ++ String.toLower targetCode)
                , Attr.class
                    ("px-2.5 py-1 rounded-md text-xs font-black tracking-wide cursor-pointer transition-all "
                        ++ (if active then
                                "bg-brand-yellow text-brand"

                            else
                                "text-slate-300 hover:text-white"
                           )
                    )
                ]
                [ text targetCode ]
    in
    div
        [ Attr.class "bg-brand border-b border-white/10"
        , Attr.attribute "data-testid" "header-navbar"
        , Attr.style "padding-top" "env(safe-area-inset-top)"
        , Attr.style "padding-left" "env(safe-area-inset-left)"
        , Attr.style "padding-right" "env(safe-area-inset-right)"
        ]
        [ Navbar.viewAppHeader
            { brand =
                Navbar.viewBrand
                    { href = "#menu"
                    , iconSrc = "/favicon.svg"
                    , title = T.title lang
                    , onClick = Nothing
                    }
            , navLabel = T.title lang
            , links = []
            , trailing =
                [ div
                    [ Attr.class "flex items-center gap-0.5 p-0.5 rounded-lg bg-slate-900/90 border border-white/15"
                    , Attr.attribute "role" "group"
                    , Attr.attribute "aria-label" (T.tr lang "Language" "Kieli")
                    , Attr.attribute "data-testid" "menu-language-selector"
                    ]
                    [ langBtn "FI" Fi
                    , langBtn "EN" En
                    ]
                , button
                    [ Attr.type_ "button"
                    , onClick actions.onToggleMute
                    , Attr.title (T.muteLabel lang audioMuted)
                    , Attr.attribute "aria-label" (T.muteLabel lang audioMuted)
                    , Attr.attribute "data-testid" "menu-audio-toggle"
                    , Attr.class "w-10 h-10 flex items-center justify-center rounded-lg text-white cursor-pointer hover:bg-brand-yellow hover:text-brand transition-colors"
                    ]
                    [ (if audioMuted then
                        FeatherIcons.volumeX

                       else
                        FeatherIcons.volume2
                      )
                        |> FeatherIcons.withSize 20
                        |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
                    ]
                ]
            , menuToggle = Nothing
            , sticky = False
            }
        ]


viewToast : Maybe String -> Html msg
viewToast maybeToast =
    case maybeToast of
        Just msg ->
            div
                [ Attr.attribute "role" "status"
                , Attr.attribute "data-testid" "menu-toast"
                , Attr.class "fixed top-16 left-1/2 -translate-x-1/2 z-40 px-4 py-2 rounded-xl bg-slate-900/95 border border-brand-yellow/60 text-brand-yellow text-xs font-bold shadow-xl"
                ]
                [ text msg ]

        Nothing ->
            text ""


viewPreviewHero : MainMenuViewData -> MainMenuActions msg -> Int -> Int -> Html msg
viewPreviewHero data actions heroWidth heroHeight =
    let
        lang =
            data.language

        stats =
            data.previewStats

        uniqueLotHashes =
            Dict.values data.previewCity.lots
                |> List.map .contentHash
                |> Set.fromList

        uniqueCount =
            1 + Set.size uniqueLotHashes

        emptyReady =
            if Mesh.hasLotMesh Mesh.emptyLotKey data.meshes then
                1

            else
                0

        compiledCount =
            emptyReady
                + Set.foldl
                    (\h acc ->
                        if Mesh.hasLotMesh h data.meshes then
                            acc + 1

                        else
                            acc
                    )
                    0
                    uniqueLotHashes

        loadingRemaining =
            max 0 (uniqueCount - compiledCount)
    in
    div
        [ Attr.class "relative w-full shrink-0 rounded-2xl overflow-hidden border border-white/15 bg-brand shadow-2xl"
        , Attr.style "height" (String.fromInt heroHeight ++ "px")
        , Attr.style "touch-action" "pan-y"
        , Attr.attribute "data-testid" "hero-preview"
        ]
        [ Render.renderCityPreview
            { city = data.previewCity
            , meshes = data.meshes
            , camera = data.camera
            , time = data.time
            , width = heroWidth
            , height = heroHeight
            , onMouseDown = actions.onMouseDown
            , onMouseMove = actions.onMouseMove
            , onMouseUp = actions.onMouseUp
            , onWheel = actions.onWheel
            }
        , div [ Attr.class "absolute top-3 left-3 flex flex-wrap items-center gap-2 pointer-events-none" ]
            [ span [ Attr.class "px-3 py-1.5 rounded-xl bg-brand/90 border border-white/20 text-xs font-extrabold text-white shadow" ]
                [ text
                    (data.previewCity.cityName
                        ++ " • "
                        ++ String.fromInt data.previewCity.cols
                        ++ "×"
                        ++ String.fromInt data.previewCity.rows
                    )
                ]
            , if loadingRemaining > 0 then
                span [ Attr.class "px-3 py-1.5 rounded-xl bg-sky-600/90 text-xs font-semibold text-white shadow" ]
                    [ text (T.loadingBricksLabel lang loadingRemaining uniqueCount) ]

              else
                text ""
            ]
        , div [ Attr.class "absolute top-3 right-3 flex items-center gap-2" ]
            [ case data.savedCity of
                Just _ ->
                    viewPreviewSwitch lang data.previewSource actions.onSetPreviewSource

                Nothing ->
                    text ""
            , span [ Attr.class "hidden sm:inline-flex items-center gap-1.5 px-2.5 py-1 rounded-xl bg-brand/80 border border-white/15 text-[11px] font-semibold text-slate-300 pointer-events-none" ]
                [ FeatherIcons.rotateCw |> FeatherIcons.withSize 12 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
                , span [] [ text (T.dragOrbitHint lang) ]
                ]
            ]
        , div [ Attr.class "absolute bottom-2.5 left-2.5 flex flex-wrap items-center gap-2 px-3 py-1.5 rounded-xl bg-brand/80 border border-white/10 text-[11px] pointer-events-none backdrop-blur-sm" ]
            [ previewStat FeatherIcons.layers (T.statLots lang) (String.fromInt stats.builtLots ++ "/" ++ String.fromInt stats.totalLots)
            , previewStat FeatherIcons.users (T.statPopulation lang) (String.fromInt stats.population)
            , previewStat FeatherIcons.home (T.statBuildings lang) (String.fromInt stats.buildings)
            , previewStat FeatherIcons.feather (T.statGreen lang) (String.fromInt stats.parks ++ "%")
            , previewStat FeatherIcons.barChart2 (T.statPeak lang) (String.fromInt stats.maxHeightPlates ++ "/32")
            ]
        ]


viewPreviewSwitch : Language -> PreviewSource -> (PreviewSource -> msg) -> Html msg
viewPreviewSwitch lang current onSet =
    let
        pill src txt testId =
            let
                active =
                    current == src
            in
            button
                [ Attr.type_ "button"
                , onClick (onSet src)
                , Attr.attribute "data-testid" testId
                , Attr.class
                    ("px-2.5 py-1 rounded-lg text-[11px] font-bold cursor-pointer transition-all "
                        ++ (if active then
                                "bg-brand-yellow text-brand"

                            else
                                "text-slate-200 hover:text-white"
                           )
                    )
                ]
                [ text txt ]
    in
    div [ Attr.class "flex items-center gap-0.5 p-0.5 rounded-xl bg-brand/90 border border-white/20 shadow" ]
        [ pill PreviewSaved (T.previewSavedLabel lang) "preview-saved-toggle"
        , pill PreviewNew (T.previewNewLabel lang) "preview-new-toggle"
        ]


previewStat : FeatherIcons.Icon -> String -> String -> Html msg
previewStat icon labelText valueText =
    div [ Attr.class "inline-flex items-center gap-1 text-slate-200" ]
        [ span [ Attr.class "text-brand-yellow" ]
            [ icon |> FeatherIcons.withSize 12 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ] ]
        , span [ Attr.class "text-slate-300 hidden sm:inline" ] [ text labelText ]
        , span [ Attr.class "font-mono font-bold text-white" ] [ text valueText ]
        ]


viewContinueCard : Language -> Time.Zone -> SavedCitySummary -> msg -> Html msg
viewContinueCard lang zone saved onContinue =
    let
        stats =
            Stats.computeStats saved.city

        timeLabel =
            case saved.savedAt of
                Just posix ->
                    T.formatTimestamp lang zone posix

                Nothing ->
                    T.savedLocallyLabel lang
    in
    section
        [ Attr.attribute "data-testid" "continue-game-card"
        , Attr.attribute "aria-label" (T.continueGameButton lang)
        , Attr.class "w-full rounded-2xl p-4 sm:p-5 bg-[#0d2235]/95 border-2 border-brand-yellow/60 shadow-xl flex flex-col sm:flex-row sm:items-center justify-between gap-4"
        ]
        [ div [ Attr.class "flex flex-col gap-1.5 min-w-0" ]
            [ span [ Attr.class "text-[11px] font-black uppercase tracking-widest text-brand-yellow" ]
                [ text (T.savedCityBadge lang) ]
            , h2 [ Attr.class "text-lg sm:text-xl font-extrabold text-white truncate" ]
                [ text saved.city.cityName ]
            , div [ Attr.class "flex flex-wrap items-center gap-x-4 gap-y-1 text-xs text-slate-300" ]
                [ span [ Attr.class "inline-flex items-center gap-1.5", Attr.attribute "data-testid" "continue-city-size" ]
                    [ FeatherIcons.grid |> FeatherIcons.withSize 13 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
                    , span [ Attr.class "font-mono font-bold text-white" ]
                        [ text (String.fromInt saved.city.cols ++ "×" ++ String.fromInt saved.city.rows) ]
                    ]
                , span [ Attr.class "inline-flex items-center gap-1.5", Attr.attribute "data-testid" "continue-city-lots" ]
                    [ FeatherIcons.layers |> FeatherIcons.withSize 13 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
                    , span [] [ text (T.statLots lang ++ ": ") ]
                    , span [ Attr.class "font-mono font-bold text-white" ]
                        [ text (String.fromInt stats.builtLots ++ "/" ++ String.fromInt stats.totalLots) ]
                    ]
                , span [ Attr.class "inline-flex items-center gap-1.5", Attr.attribute "data-testid" "continue-city-pieces" ]
                    [ FeatherIcons.box |> FeatherIcons.withSize 13 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
                    , span [] [ text (T.statPieces lang ++ ": ") ]
                    , span [ Attr.class "font-mono font-bold text-white" ]
                        [ text (String.fromInt stats.totalParts) ]
                    ]
                , span [ Attr.class "inline-flex items-center gap-1.5", Attr.attribute "data-testid" "continue-city-population" ]
                    [ FeatherIcons.users |> FeatherIcons.withSize 13 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
                    , span [] [ text (T.statPopulation lang ++ ": ") ]
                    , span [ Attr.class "font-mono font-bold text-white" ] [ text (String.fromInt stats.population) ]
                    ]
                , span [ Attr.class "inline-flex items-center gap-1.5", Attr.attribute "data-testid" "continue-city-timestamp" ]
                    [ FeatherIcons.clock |> FeatherIcons.withSize 13 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
                    , span [] [ text timeLabel ]
                    ]
                ]
            ]
        , button
            [ Attr.type_ "button"
            , onClick onContinue
            , Attr.attribute "data-testid" "continue-game-button"
            , Attr.class "min-h-[52px] shrink-0 inline-flex items-center justify-center gap-2.5 px-7 py-3 rounded-xl bg-brand-yellow text-brand font-extrabold text-base uppercase tracking-wider cursor-pointer shadow-lg shadow-brand-yellow/25 hover:brightness-110 active:brightness-95 focus-visible:ring-2 focus-visible:ring-brand-yellow focus-visible:ring-offset-2 focus-visible:ring-offset-[#0d2235] focus-visible:outline-none transition-all"
            ]
            [ FeatherIcons.play |> FeatherIcons.withSize 20 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
            , span [] [ text (T.continueGameButton lang) ]
            ]
        ]


viewNewCityPanelParts : Language -> String -> Int -> CityPreset -> MainMenuActions msg -> Html msg
viewNewCityPanelParts lang newCityName gridSize preset actions =
    let
        nameInputId =
            "menu-new-city-name"

        enterStartDecoder =
            Decode.field "key" Decode.string
                |> Decode.andThen
                    (\k ->
                        if k == "Enter" then
                            Decode.succeed actions.onStartNewCity

                        else
                            Decode.fail "not Enter"
                    )
    in
    section
        [ Attr.attribute "data-testid" "new-city-panel"
        , Attr.attribute "aria-label" (T.newCityHeading lang)
        , Attr.class "flex flex-col justify-between gap-4 p-4 sm:p-5 rounded-2xl bg-[#0d2235]/95 border border-white/10 shadow-xl"
        ]
        [ div [ Attr.class "flex flex-col gap-4" ]
            [ div [ Attr.class "flex items-center justify-between gap-2" ]
                [ div [ Attr.class "flex items-center gap-2" ]
                    [ span [ Attr.class "text-brand-yellow" ]
                        [ FeatherIcons.map |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ] ]
                    , h2 [ Attr.class "text-base font-extrabold text-white tracking-tight" ]
                        [ text (T.newCityHeading lang) ]
                    ]
                , button
                    [ Attr.type_ "button"
                    , onClick actions.onImportJson
                    , Attr.attribute "data-testid" "menu-import-json-button"
                    , Attr.class "inline-flex items-center gap-1.5 px-2.5 py-1 rounded-lg bg-white/5 border border-white/15 hover:border-brand-yellow/60 text-[11px] font-bold text-slate-300 hover:text-white cursor-pointer focus-visible:ring-2 focus-visible:ring-brand-yellow focus-visible:outline-none transition-colors"
                    ]
                    [ FeatherIcons.upload |> FeatherIcons.withSize 12 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
                    , span [] [ text (T.actionImportJson lang) ]
                    ]
                ]
            , div [ Attr.class "flex flex-col gap-1.5" ]
                [ label
                    [ Attr.for nameInputId
                    , Attr.class "text-xs font-bold uppercase tracking-wider text-slate-300"
                    ]
                    [ text (T.cityNameLabel lang) ]
                , input
                    [ Attr.id nameInputId
                    , Attr.type_ "text"
                    , Attr.value newCityName
                    , onInput actions.onCityNameChanged
                    , Html.Events.on "keydown" enterStartDecoder
                    , Attr.maxlength 32
                    , Attr.placeholder (T.defaultCityName lang)
                    , Attr.attribute "data-testid" "new-city-name-input"
                    , Attr.class "w-full min-h-[44px] px-3.5 rounded-xl bg-slate-900/90 border border-white/20 focus:border-brand-yellow focus:ring-1 focus:ring-brand-yellow outline-none text-sm font-bold text-white placeholder:text-slate-500"
                    ]
                    []
                , div [ Attr.class "flex flex-wrap items-center gap-1.5 pt-0.5" ]
                    (List.map
                        (\suggestion ->
                            button
                                [ Attr.type_ "button"
                                , onClick (actions.onCityNameChanged suggestion)
                                , Attr.class
                                    ("px-2.5 py-0.5 rounded-full text-[11px] font-semibold border cursor-pointer focus-visible:ring-2 focus-visible:ring-brand-yellow focus-visible:outline-none transition-colors "
                                        ++ (if String.trim newCityName == suggestion then
                                                "bg-brand-yellow/20 border-brand-yellow/60 text-brand-yellow"

                                            else
                                                "bg-white/5 border-white/10 text-slate-300 hover:border-white/30"
                                           )
                                    )
                                ]
                                [ text suggestion ]
                        )
                        (T.cityNameSuggestions lang)
                    )
                ]
            , div [ Attr.class "flex flex-col gap-1.5" ]
                [ div [ Attr.class "flex items-center justify-between gap-2" ]
                    [ span [ Attr.class "text-xs font-bold uppercase tracking-wider text-slate-300" ]
                        [ text (T.gridSizeLabel lang) ]
                    , span [ Attr.class "text-[11px] font-mono text-sky-300" ]
                        [ text (T.gridDimensionsHint lang gridSize) ]
                    ]
                , div [ Attr.class "grid grid-cols-4 gap-2" ]
                    (List.map (viewGridSizeButton gridSize actions.onSelectGridSize) [ 4, 5, 6, 8 ])
                ]
            , div [ Attr.class "flex flex-col gap-1.5" ]
                [ span [ Attr.class "text-xs font-bold uppercase tracking-wider text-slate-300" ]
                    [ text (T.starterLayoutLabel lang) ]
                , div [ Attr.class "grid grid-cols-1 sm:grid-cols-2 gap-2" ]
                    (List.map (viewPresetCard lang preset actions.onSelectPreset) Templates.allPresets)
                ]
            ]
        , button
            [ Attr.type_ "button"
            , onClick actions.onStartNewCity
            , Attr.attribute "data-testid" "start-building-button"
            , Attr.class "w-full min-h-[52px] flex items-center justify-center gap-2.5 py-3.5 px-6 rounded-xl bg-brand-yellow text-brand font-extrabold text-base uppercase tracking-wider cursor-pointer shadow-lg shadow-brand-yellow/25 hover:brightness-110 active:brightness-95 focus-visible:ring-2 focus-visible:ring-brand-yellow focus-visible:ring-offset-2 focus-visible:ring-offset-[#0d2235] focus-visible:outline-none transition-all"
            ]
            [ FeatherIcons.tool |> FeatherIcons.withSize 20 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
            , span [] [ text (T.startBuildingButton lang) ]
            ]
        ]


viewGridSizeButton : Int -> (Int -> msg) -> Int -> Html msg
viewGridSizeButton selectedSize onSelect size =
    let
        active =
            selectedSize == size
    in
    button
        [ Attr.type_ "button"
        , onClick (onSelect size)
        , Attr.attribute "aria-pressed" (boolStr active)
        , Attr.attribute "data-testid" ("menu-grid-size-" ++ String.fromInt size)
        , Attr.class
            ("min-h-[42px] rounded-xl font-mono text-sm font-extrabold border cursor-pointer focus-visible:ring-2 focus-visible:ring-brand-yellow focus-visible:outline-none transition-all "
                ++ (if active then
                        "bg-brand-yellow border-brand-yellow text-brand shadow-md"

                    else
                        "bg-slate-900/75 border-white/15 text-slate-200 hover:border-white/35"
                   )
            )
        ]
        [ text (String.fromInt size ++ "×" ++ String.fromInt size) ]


viewPresetCard : Language -> CityPreset -> (CityPreset -> msg) -> CityPreset -> Html msg
viewPresetCard lang selectedPreset onSelect preset =
    let
        active =
            selectedPreset == preset

        icon =
            case preset of
                HarborTown ->
                    FeatherIcons.anchor

                GridMetropolis ->
                    FeatherIcons.grid

                GreenValley ->
                    FeatherIcons.feather

                BlankCanvas ->
                    FeatherIcons.square
    in
    button
        [ Attr.type_ "button"
        , onClick (onSelect preset)
        , Attr.attribute "aria-pressed" (boolStr active)
        , Attr.attribute "data-testid" ("preset-" ++ Templates.presetId preset)
        , Attr.class
            ("p-3 rounded-xl border text-left flex flex-col gap-1 cursor-pointer focus-visible:ring-2 focus-visible:ring-brand-yellow focus-visible:outline-none transition-all "
                ++ (if active then
                        "bg-brand-yellow/15 border-brand-yellow text-white shadow-md"

                    else
                        "bg-slate-900/65 border-white/10 text-slate-200 hover:border-white/30"
                   )
            )
        ]
        [ div [ Attr.class "flex items-center gap-2" ]
            [ span
                [ Attr.class
                    (if active then
                        "text-brand-yellow shrink-0"

                     else
                        "text-sky-300 shrink-0"
                    )
                ]
                [ icon |> FeatherIcons.withSize 15 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ] ]
            , span [ Attr.class "text-xs font-extrabold truncate" ]
                [ text (T.presetTitle lang preset) ]
            ]
        , p [ Attr.class "text-[11px] text-slate-300 leading-snug" ]
            [ text (T.presetDescription lang preset) ]
        ]


viewCollabPanelParts : Language -> CityRoom -> String -> String -> String -> Bool -> MainMenuActions msg -> Html msg
viewCollabPanelParts lang room roomCodeInput builderName baseUrl shareCopiedNotice actions =
    let
        inRoom =
            room.role /= LocalSolo
    in
    section
        [ Attr.attribute "data-testid" "menu-collab-panel"
        , Attr.attribute "aria-label" (T.roomHeading lang)
        , Attr.class "flex flex-col justify-between gap-4 p-4 sm:p-5 rounded-2xl bg-[#0d2235]/95 border border-white/10 shadow-xl"
        ]
        [ div [ Attr.class "flex items-center justify-between gap-2" ]
            [ div [ Attr.class "flex items-center gap-2" ]
                [ span [ Attr.class "text-brand-yellow" ]
                    [ FeatherIcons.users |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ] ]
                , h2 [ Attr.class "text-base font-extrabold text-white tracking-tight" ]
                    [ text (T.roomHeading lang) ]
                ]
            , viewRoomStatusPill lang room
            ]
        , if inRoom then
            viewCollabLobby
                { language = lang
                , room = room
                , baseUrl = baseUrl
                , shareCopiedNotice = shareCopiedNotice
                }
                actions

          else
            viewCollabJoinOrHost
                { language = lang
                , builderName = builderName
                , roomCodeInput = roomCodeInput
                }
                actions
        ]


viewRoomStatusPill : Language -> CityRoom -> Html msg
viewRoomStatusPill lang room =
    let
        ( cls, labelTxt ) =
            case room.status of
                Connected ->
                    ( "bg-emerald-500/20 border-emerald-400/50 text-emerald-300", T.connectedLabel lang )

                Connecting ->
                    ( "bg-amber-500/20 border-amber-400/50 text-amber-300", T.connectingLabel lang )

                _ ->
                    ( "bg-white/10 border-white/15 text-slate-300", T.disconnectedLabel lang )
    in
    span [ Attr.class ("px-2.5 py-0.5 rounded-full border text-[11px] font-bold " ++ cls) ]
        [ text labelTxt ]


viewCollabJoinOrHost :
    { language : Language
    , builderName : String
    , roomCodeInput : String
    }
    -> MainMenuActions msg
    -> Html msg
viewCollabJoinOrHost data actions =
    let
        lang =
            data.language

        builderInputId =
            "menu-builder-name"

        roomInputId =
            "menu-room-code"

        enterKeyDecoder =
            Decode.field "key" Decode.string
                |> Decode.andThen
                    (\k ->
                        if k == "Enter" then
                            Decode.succeed actions.onJoinRoom

                        else
                            Decode.fail "not Enter"
                    )

        enterHostDecoder =
            Decode.field "key" Decode.string
                |> Decode.andThen
                    (\k ->
                        if k == "Enter" then
                            Decode.succeed actions.onHostRoom

                        else
                            Decode.fail "not Enter"
                    )
    in
    div [ Attr.class "flex-1 flex flex-col justify-between gap-4" ]
        [ div [ Attr.class "flex flex-col gap-4" ]
            [ p [ Attr.class "text-xs text-slate-300 leading-relaxed" ]
                [ text (T.collabMenuSubtitle lang) ]
            , div [ Attr.class "flex flex-col gap-1.5" ]
                [ label
                    [ Attr.for builderInputId
                    , Attr.class "text-xs font-bold uppercase tracking-wider text-slate-300"
                    ]
                    [ text (T.builderNameLabel lang) ]
                , input
                    [ Attr.id builderInputId
                    , Attr.type_ "text"
                    , Attr.value data.builderName
                    , onInput actions.onBuilderNameChanged
                    , Html.Events.on "keydown" enterHostDecoder
                    , Attr.maxlength 24
                    , Attr.attribute "data-testid" "menu-builder-name-input"
                    , Attr.class "w-full min-h-[42px] px-3.5 rounded-xl bg-slate-900/90 border border-white/20 focus:border-brand-yellow focus:ring-1 focus:ring-brand-yellow outline-none text-sm font-semibold text-white"
                    ]
                    []
                ]
            , button
                [ Attr.type_ "button"
                , onClick actions.onHostRoom
                , Attr.attribute "data-testid" "menu-host-room-button"
                , Attr.class "w-full min-h-[48px] flex items-center justify-center gap-2 px-4 py-3 rounded-xl bg-sky-600 hover:bg-sky-500 text-white text-sm font-extrabold cursor-pointer shadow-md focus-visible:ring-2 focus-visible:ring-sky-400 focus-visible:outline-none transition-colors"
                ]
                [ FeatherIcons.share2 |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
                , span [] [ text (T.hostCollabRoomButton lang) ]
                ]
            ]
        , div [ Attr.class "flex flex-col gap-2 pt-3 border-t border-white/10" ]
            [ label
                [ Attr.for roomInputId
                , Attr.class "text-xs font-bold uppercase tracking-wider text-slate-300"
                ]
                [ text (T.joinRoomButton lang) ]
            , div [ Attr.class "flex items-center gap-2" ]
                [ input
                    [ Attr.id roomInputId
                    , Attr.type_ "text"
                    , Attr.value data.roomCodeInput
                    , onInput actions.onRoomCodeChanged
                    , Html.Events.on "keydown" enterKeyDecoder
                    , Attr.placeholder (T.joinRoomInputPlaceholder lang)
                    , Attr.attribute "autocomplete" "off"
                    , Attr.attribute "data-testid" "menu-room-code-input"
                    , Attr.class "flex-1 min-w-0 min-h-[44px] px-3.5 rounded-xl bg-slate-900/90 border border-white/20 focus:border-brand-yellow focus:ring-1 focus:ring-brand-yellow outline-none text-xs sm:text-sm font-mono text-white placeholder:text-slate-500"
                    ]
                    []
                , button
                    [ Attr.type_ "button"
                    , onClick actions.onJoinRoom
                    , Attr.attribute "data-testid" "menu-join-room-button"
                    , Attr.class "min-h-[44px] px-5 rounded-xl bg-white/15 hover:bg-brand-yellow hover:text-brand border border-white/20 text-white text-xs sm:text-sm font-extrabold cursor-pointer focus-visible:ring-2 focus-visible:ring-brand-yellow focus-visible:outline-none transition-colors"
                    ]
                    [ text (T.joinRoomButton lang) ]
                ]
            ]
        ]


viewCollabLobby :
    { language : Language
    , room : CityRoom
    , baseUrl : String
    , shareCopiedNotice : Bool
    }
    -> MainMenuActions msg
    -> Html msg
viewCollabLobby data actions =
    let
        lang =
            data.language

        roomUrl =
            makeCityRoomUrl data.baseUrl data.room.roomId
    in
    div
        [ Attr.attribute "data-testid" "menu-collab-lobby"
        , Attr.class "flex-1 flex flex-col justify-between gap-4"
        ]
        [ div [ Attr.class "flex flex-col items-center gap-3" ]
            [ div [ Attr.class "w-full flex items-center justify-between gap-2 p-3 rounded-xl bg-slate-900/90 border border-brand-yellow/40" ]
                [ div [ Attr.class "min-w-0" ]
                    [ span [ Attr.class "text-[11px] uppercase tracking-wider text-slate-400 font-bold" ]
                        [ text (T.joinRoomPlaceholder lang) ]
                    , p [ Attr.class "text-xl font-mono font-black tracking-widest text-brand-yellow", Attr.attribute "data-testid" "menu-lobby-room-code" ]
                        [ text data.room.roomId ]
                    ]
                , button
                    [ Attr.type_ "button"
                    , onClick actions.onCopyRoomLink
                    , Attr.attribute "data-testid" "menu-copy-room-link"
                    , Attr.class "inline-flex items-center gap-1.5 h-10 px-3.5 rounded-xl bg-brand-yellow text-brand text-xs font-extrabold cursor-pointer hover:brightness-110 focus-visible:ring-2 focus-visible:ring-brand-yellow focus-visible:outline-none"
                    ]
                    [ (if data.shareCopiedNotice then
                        FeatherIcons.check

                       else
                        FeatherIcons.copy
                      )
                        |> FeatherIcons.withSize 14
                        |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
                    , span [] [ text (T.copyInviteLink lang) ]
                    ]
                ]
            , viewQrCode roomUrl
            , span [ Attr.class "text-[11px] text-slate-400 text-center" ]
                [ text (T.qrInviteHint lang) ]
            , div [ Attr.class "w-full flex flex-col gap-1.5" ]
                [ span [ Attr.class "text-[11px] font-bold uppercase tracking-wider text-slate-400" ]
                    [ text (T.buildersHeading lang) ]
                , div [ Attr.class "flex flex-wrap gap-1.5" ]
                    (List.map
                        (\peer ->
                            div [ Attr.class "inline-flex items-center gap-1.5 px-2.5 py-1 rounded-full bg-slate-800 border border-white/15 text-xs" ]
                                [ span [ Attr.class "w-2.5 h-2.5 rounded-full", Attr.style "background" peer.color ] []
                                , span [ Attr.class "font-semibold" ]
                                    [ text
                                        (peer.name
                                            ++ (if peer.slot == data.room.mySlot then
                                                    " (" ++ T.youTag lang ++ ")"

                                                else
                                                    ""
                                               )
                                        )
                                    ]
                                ]
                        )
                        data.room.peers
                    )
                ]
            ]
        , div [ Attr.class "flex items-center gap-2 pt-2 border-t border-white/10" ]
            [ if data.room.status == Disconnected then
                button
                    [ Attr.type_ "button"
                    , onClick actions.onHostRoom
                    , Attr.attribute "data-testid" "menu-claim-host-button"
                    , Attr.class "flex-1 min-h-[48px] inline-flex items-center justify-center gap-2 px-4 rounded-xl bg-brand-yellow text-brand text-sm font-extrabold cursor-pointer hover:brightness-110 focus-visible:ring-2 focus-visible:ring-brand-yellow focus-visible:outline-none"
                    ]
                    [ FeatherIcons.share2 |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
                    , span [] [ text (T.claimHostButton lang) ]
                    ]

              else
                button
                    [ Attr.type_ "button"
                    , onClick actions.onEnterRoomCity
                    , Attr.attribute "data-testid" "menu-enter-room-city-button"
                    , Attr.class "flex-1 min-h-[48px] inline-flex items-center justify-center gap-2 px-4 rounded-xl bg-brand-yellow text-brand text-sm font-extrabold cursor-pointer hover:brightness-110 focus-visible:ring-2 focus-visible:ring-brand-yellow focus-visible:outline-none"
                    ]
                    [ FeatherIcons.play |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
                    , span [] [ text (T.enterRoomCityButton lang) ]
                    ]
            , button
                [ Attr.type_ "button"
                , onClick actions.onLeaveRoom
                , Attr.attribute "data-testid" "menu-leave-room-button"
                , Attr.class "min-h-[48px] px-4 rounded-xl bg-rose-600/80 hover:bg-rose-600 text-white text-xs font-extrabold cursor-pointer focus-visible:ring-2 focus-visible:ring-rose-400 focus-visible:outline-none"
                ]
                [ text (T.leaveRoomButton lang) ]
            ]
        ]


viewQrCode : String -> Html msg
viewQrCode url =
    case QRCode.fromString url of
        Ok qr ->
            div
                [ Attr.attribute "data-testid" "menu-room-qrcode"
                , Attr.style "background" "#ffffff"
                , Attr.style "padding" "10px"
                , Attr.style "border-radius" "14px"
                , Attr.style "box-shadow" "0 4px 20px rgba(0, 0, 0, 0.35)"
                , Attr.style "display" "inline-block"
                ]
                [ QRCode.toSvg
                    [ SvgAttr.width "148px"
                    , SvgAttr.height "148px"
                    ]
                    qr
                ]

        Err _ ->
            text ""


boolStr : Bool -> String
boolStr b =
    if b then
        "true"

    else
        "false"
