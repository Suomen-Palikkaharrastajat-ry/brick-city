module MainMenuTest exposing (suite)

{-| Unit tests for Brick City's Main Menu presets, URL hash routing, room
invite URL builder, timestamp formatting, HTML rendering, and bilingual localization.
-}

import City.Bounds as Bounds
import City.I18n as CityI18n
import City.Mesh as Mesh
import City.Render as Render
import City.Stats as Stats
import City.Templates as Templates exposing (CityPreset(..))
import City.Types exposing (CityGrid, CityRoom, RoomConnection(..), RoomRole(..))
import Dict
import Expect
import Html exposing (Html)
import Html.Attributes as Attr
import I18n exposing (Language(..))
import Main exposing (isCityRouteHash, isMenuHash)
import Pages.MainMenu as MainMenu exposing (MainMenuActions, MainMenuViewData, PreviewSource(..), SavedCitySummary)
import Render.Camera as Camera exposing (Camera)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Time


type TestMsg
    = NoOp
    | SetLanguageMsg Language
    | ToggleMuteMsg
    | ContinueSavedMsg
    | CityNameChangedMsg String
    | SelectGridSizeMsg Int
    | SelectPresetMsg CityPreset
    | SetPreviewSourceMsg PreviewSource
    | StartNewCityMsg
    | ImportJsonMsg
    | BuilderNameChangedMsg String
    | HostRoomMsg
    | RoomCodeChangedMsg String
    | JoinRoomMsg
    | CopyRoomLinkMsg
    | EnterRoomCityMsg
    | LeaveRoomMsg
    | MouseDownMsg Float Float
    | MouseMoveMsg Float Float
    | MouseUpMsg
    | WheelMsg Float


testActions : MainMenuActions TestMsg
testActions =
    { onSetLanguage = SetLanguageMsg
    , onToggleMute = ToggleMuteMsg
    , onContinueSaved = ContinueSavedMsg
    , onCityNameChanged = CityNameChangedMsg
    , onSelectGridSize = SelectGridSizeMsg
    , onSelectPreset = SelectPresetMsg
    , onSetPreviewSource = SetPreviewSourceMsg
    , onStartNewCity = StartNewCityMsg
    , onImportJson = ImportJsonMsg
    , onBuilderNameChanged = BuilderNameChangedMsg
    , onHostRoom = HostRoomMsg
    , onRoomCodeChanged = RoomCodeChangedMsg
    , onJoinRoom = JoinRoomMsg
    , onCopyRoomLink = CopyRoomLinkMsg
    , onEnterRoomCity = EnterRoomCityMsg
    , onLeaveRoom = LeaveRoomMsg
    , onMouseDown = MouseDownMsg
    , onMouseMove = MouseMoveMsg
    , onMouseUp = MouseUpMsg
    , onWheel = WheelMsg
    }


mockCity : CityGrid
mockCity =
    Templates.cityFromPreset Fi HarborTown "Testikaupunki" 4


mockCamera : Camera
mockCamera =
    Render.resetCameraForCity mockCity 768


soloRoom : CityRoom
soloRoom =
    { roomId = ""
    , role = LocalSolo
    , status = Disconnected
    , mySlot = 1
    , myName = "Rakentaja"
    , peers = []
    }


activeCollabRoom : CityRoom
activeCollabRoom =
    { roomId = "ABCD"
    , role = RoomHost
    , status = Connected
    , mySlot = 1
    , myName = "Host"
    , peers =
        [ { slot = 1, id = "host-1", name = "Host", color = "#f2cd37", focusedLot = Nothing }
        , { slot = 2, id = "guest-2", name = "Builder 2", color = "#00852b", focusedLot = Nothing }
        ]
    }


baseViewData : MainMenuViewData
baseViewData =
    { language = Fi
    , audioMuted = False
    , savedCity = Nothing
    , previewSource = PreviewNew
    , previewCity = mockCity
    , previewStats = Stats.computeStats mockCity
    , meshes = Mesh.empty
    , camera = mockCamera
    , time = 10.0
    , timeZone = Time.utc
    , viewportWidth = 1280
    , viewportHeight = 800
    , newCityName = "Testikaupunki"
    , gridSize = 4
    , preset = GridMetropolis
    , room = soloRoom
    , roomCodeInput = ""
    , builderName = "Rakentaja"
    , baseUrl = "https://kaupunki.palikkaharrastajat.fi/"
    , shareCopiedNotice = False
    , toast = Nothing
    }


{-| Test suite for Main Menu presets, routing, room links, localization, and HTML rendering.
-}
suite : Test
suite =
    describe "Pages.MainMenu and Starter Layout Presets"
        [ describe "Starter layout presets order, IDs, and dimensions"
            [ test "preset order is GridMetropolis, GreenValley, HarborTown (Coastal Bay), BlankCanvas" <|
                \_ ->
                    Expect.equal
                        [ GridMetropolis, GreenValley, HarborTown, BlankCanvas ]
                        Templates.allPresets
            , test "presetId mapping produces kebab-case identifiers" <|
                \_ ->
                    List.map Templates.presetId Templates.allPresets
                        |> Expect.equal [ "grid-metropolis", "green-valley", "coastal-bay", "sandbox-plain" ]
            , test "preset titles in English match expected names" <|
                \_ ->
                    List.map (CityI18n.presetTitle En) Templates.allPresets
                        |> Expect.equal [ "Grid Metropolis", "Green Valley", "Coastal Bay", "Sandbox Plain" ]
            , test "preset titles in Finnish match expected names" <|
                \_ ->
                    List.map (CityI18n.presetTitle Fi) Templates.allPresets
                        |> Expect.equal [ "Ruutukaava", "Vihreä laakso", "Rantakaupunki", "Tyhjä tonttimaa" ]
            , test "all 4 presets generate valid grids for 4x4, 5x5, 6x6, and 8x8" <|
                \_ ->
                    let
                        combos : List ( CityPreset, Int )
                        combos =
                            Templates.allPresets
                                |> List.concatMap (\p -> List.map (\s -> ( p, s )) [ 4, 5, 6, 8 ])

                        checkCombo : ( CityPreset, Int ) -> Maybe String
                        checkCombo ( preset, size ) =
                            let
                                city =
                                    Templates.cityFromPreset Fi preset "Testikaupunki" size

                                badBounds =
                                    Dict.toList city.lots
                                        |> List.filterMap
                                            (\( ( r, c ), el ) ->
                                                if r < 0 || c < 0 || r >= size || c >= size then
                                                    Just ("Out-of-grid lot at " ++ String.fromInt r ++ "," ++ String.fromInt c)

                                                else
                                                    case Bounds.validateDocument el.document of
                                                        Ok _ ->
                                                            Nothing

                                                        Err errs ->
                                                            Just (el.name ++ ": " ++ Debug.toString errs)
                                            )
                            in
                            if city.cols /= size || city.rows /= size then
                                Just ("Wrong grid size for " ++ Templates.presetId preset)

                            else if not (List.isEmpty badBounds) then
                                List.head badBounds

                            else
                                Nothing
                    in
                    combos
                        |> List.filterMap checkCombo
                        |> Expect.equal []
            , test "HarborTown / Coastal Bay includes roads, park, canal, buildings, and positive population" <|
                \_ ->
                    let
                        city =
                            Templates.cityFromPreset En HarborTown "Harbor" 4

                        stats =
                            Stats.computeStats city
                    in
                    Expect.all
                        [ \s -> Expect.greaterThan 0 s.population
                        , \s -> Expect.greaterThan 0 s.buildings
                        , \s -> Expect.greaterThan 0 s.parks
                        , \s -> Expect.greaterThan 0 s.waterfronts
                        , \s -> Expect.greaterThan 0 s.roadConnections
                        , \s -> Expect.greaterThan 0 s.totalParts
                        , \s -> Expect.greaterThan 0 s.builtLots
                        ]
                        stats
            , test "GridMetropolis builds connected perimeter/cross roads and leaves zoned empty lots" <|
                \_ ->
                    let
                        city =
                            Templates.cityFromPreset Fi GridMetropolis "Ruutu" 6

                        stats =
                            Stats.computeStats city
                    in
                    Expect.all
                        [ \s -> Expect.greaterThan 0 s.roadConnections
                        , \s -> Expect.lessThan s.totalLots s.builtLots
                        , \_ -> Expect.equal True (Dict.member ( 0, 0 ) city.lots)
                        , \_ -> Expect.equal True (Dict.member ( 5, 5 ) city.lots)
                        , \_ -> Expect.equal False (Dict.member ( 1, 1 ) city.lots)
                        ]
                        stats
            , test "GreenValley includes central park and waterfront canal with open green lots" <|
                \_ ->
                    let
                        city =
                            Templates.cityFromPreset En GreenValley "Valley" 5

                        stats =
                            Stats.computeStats city
                    in
                    Expect.all
                        [ \s -> Expect.greaterThan 0 s.parks
                        , \s -> Expect.greaterThan 0 s.waterfronts
                        , \s -> Expect.lessThan s.totalLots s.builtLots
                        ]
                        stats
            , test "BlankCanvas starts completely empty for every grid size" <|
                \_ ->
                    [ 4, 5, 6, 8 ]
                        |> List.map (\s -> Dict.size (Templates.cityFromPreset Fi BlankCanvas "Tyhjä" s).lots)
                        |> Expect.equal [ 0, 0, 0, 0 ]
            ]
        , describe "Main Menu URL hash routing and collaborative links"
            [ test "isMenuHash matches root, #/, #menu, and language hashes" <|
                \_ ->
                    [ "", "#", "#/", "#menu", "#/menu", "#lang=en", "#lang=fi", "#/lang=en" ]
                        |> List.all isMenuHash
                        |> Expect.equal True
            , test "isCityRouteHash matches solo city routes and rejects menu or room routes" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal True (isCityRouteHash "#/city")
                        , \_ -> Expect.equal True (isCityRouteHash "#city")
                        , \_ -> Expect.equal True (isCityRouteHash "#/play")
                        , \_ -> Expect.equal True (isCityRouteHash "#play")
                        , \_ -> Expect.equal False (isCityRouteHash "#/city/ABCD")
                        , \_ -> Expect.equal False (isCityRouteHash "#menu")
                        ]
                        ()
            , test "makeCityRoomUrl builds normalized room share links across base URL variations" <|
                \_ ->
                    Expect.all
                        [ \_ ->
                            MainMenu.makeCityRoomUrl "https://kaupunki.palikkaharrastajat.fi/" "ab12cd"
                                |> Expect.equal "https://kaupunki.palikkaharrastajat.fi/#/city/AB12CD"
                        , \_ ->
                            MainMenu.makeCityRoomUrl "https://kaupunki.palikkaharrastajat.fi" "xy99"
                                |> Expect.equal "https://kaupunki.palikkaharrastajat.fi/#/city/XY99"
                        , \_ ->
                            MainMenu.makeCityRoomUrl "http://localhost:5178" "k7m9"
                                |> Expect.equal "http://localhost:5178/#/city/K7M9"
                        , \_ ->
                            MainMenu.makeCityRoomUrl "" "xy99"
                                |> Expect.equal "#/city/XY99"
                        , \_ ->
                            MainMenu.makeCityRoomUrl "https://kaupunki.palikkaharrastajat.fi/index.html" "ROOM"
                                |> Expect.equal "https://kaupunki.palikkaharrastajat.fi/index.html#/city/ROOM"
                        ]
                        ()
            ]
        , describe "Main Menu I18n labels and stats formatting"
            [ test "formatTimestamp formats UTC timestamps cleanly in Finnish and English" <|
                \_ ->
                    let
                        -- 2026-11-26 21:05:00 UTC in milliseconds
                        samplePosix =
                            Time.millisToPosix 1795727100000
                    in
                    Expect.all
                        [ \_ ->
                            CityI18n.formatTimestamp Fi Time.utc samplePosix
                                |> Expect.equal "26.11.2026 klo 21.05"
                        , \_ ->
                            CityI18n.formatTimestamp En Time.utc samplePosix
                                |> Expect.equal "2026-11-26 21:05"
                        ]
                        ()
            , test "statLots and statPieces return localized labels in Fi and En" <|
                \_ ->
                    Expect.all
                        [ \_ -> CityI18n.statLots En |> Expect.equal "Lots"
                        , \_ -> CityI18n.statLots Fi |> Expect.equal "Tontit"
                        , \_ -> CityI18n.statPieces En |> Expect.equal "Pieces"
                        , \_ -> CityI18n.statPieces Fi |> Expect.equal "Palat"
                        , \_ -> CityI18n.continueGameButton En |> Expect.equal "Continue City"
                        , \_ -> CityI18n.continueGameButton Fi |> Expect.equal "Jatka peliä"
                        ]
                        ()
            , test "all preset descriptions are localized and non-empty in Fi and En" <|
                \_ ->
                    Templates.allPresets
                        |> List.all
                            (\p ->
                                not (String.isEmpty (CityI18n.presetDescription Fi p))
                                    && not (String.isEmpty (CityI18n.presetDescription En p))
                                    && (CityI18n.presetDescription Fi p /= CityI18n.presetDescription En p)
                            )
                        |> Expect.equal True
            ]
        , describe "HTML View rendering with Test.Html.Query"
            [ test "renders top header with brand title, language toggle, and audio toggle" <|
                \_ ->
                    MainMenu.view baseViewData testActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ \q -> q |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "header-navbar") ] |> Query.has [ Selector.text "Palikkakaupunki" ]
                            , \q -> q |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "menu-language-selector") ] |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "menu-lang-fi") ]
                            , \q -> q |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "menu-language-selector") ] |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "menu-lang-en") ]
                            , \q -> q |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "menu-audio-toggle") ] |> Query.has [ Selector.attribute (Attr.attribute "type" "button") ]
                            ]
            , test "renders 3D hero turntable container with stats" <|
                \_ ->
                    MainMenu.view baseViewData testActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ \q -> q |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "hero-preview") ] |> Query.has [ Selector.text "Testikaupunki • 4×4" ]
                            ]
            , test "does not render Continue City card when savedCity is Nothing" <|
                \_ ->
                    MainMenu.view baseViewData testActions
                        |> Query.fromHtml
                        |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "main-content") ]
                        |> Query.hasNot [ Selector.attribute (Attr.attribute "data-testid" "continue-game-card") ]
            , test "renders Continue City card when savedCity is present with all lot/piece counts" <|
                \_ ->
                    let
                        savedSummary =
                            { city = mockCity
                            , savedAt = Just (Time.millisToPosix 1795727100000)
                            }

                        dataWithSaved =
                            { baseViewData
                                | savedCity = Just savedSummary
                                , previewSource = PreviewSaved
                            }

                        stats =
                            Stats.computeStats mockCity
                    in
                    MainMenu.view dataWithSaved testActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ \q ->
                                q
                                    |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "continue-game-card") ]
                                    |> Query.has [ Selector.text "Testikaupunki" ]
                            , \q ->
                                q
                                    |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "continue-city-size") ]
                                    |> Query.has [ Selector.text "4×4" ]
                            , \q ->
                                q
                                    |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "continue-city-lots") ]
                                    |> Query.has [ Selector.text (String.fromInt stats.builtLots ++ "/" ++ String.fromInt stats.totalLots) ]
                            , \q ->
                                q
                                    |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "continue-city-pieces") ]
                                    |> Query.has [ Selector.text (String.fromInt stats.totalParts) ]
                            , \q ->
                                q
                                    |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "continue-city-population") ]
                                    |> Query.has [ Selector.text (String.fromInt stats.population) ]
                            , \q ->
                                q
                                    |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "continue-city-timestamp") ]
                                    |> Query.has [ Selector.text "26.11.2026 klo 21.05" ]
                            , \q ->
                                q
                                    |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "continue-game-button") ]
                                    |> Query.has [ Selector.text "Jatka peliä" ]
                            , \q ->
                                q
                                    |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "preview-saved-toggle") ]
                            ]
            , test "renders New City panel with name input, 4 grid sizes, 4 presets, and Start Building button" <|
                \_ ->
                    MainMenu.view baseViewData testActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ \q ->
                                q
                                    |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "new-city-panel") ]
                                    |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "new-city-name-input") ]
                            , \q ->
                                q
                                    |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "menu-grid-size-4") ]
                            , \q ->
                                q
                                    |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "menu-grid-size-5") ]
                            , \q ->
                                q
                                    |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "menu-grid-size-6") ]
                            , \q ->
                                q
                                    |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "menu-grid-size-8") ]
                            , \q ->
                                q
                                    |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "preset-grid-metropolis") ]
                            , \q ->
                                q
                                    |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "preset-green-valley") ]
                            , \q ->
                                q
                                    |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "preset-coastal-bay") ]
                            , \q ->
                                q
                                    |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "preset-sandbox-plain") ]
                            , \q ->
                                q
                                    |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "start-building-button") ]
                                    |> Query.has [ Selector.text "Aloita rakentaminen" ]
                            ]
            , test "renders collaborative section in solo mode with builder name, host, room code, and join button" <|
                \_ ->
                    MainMenu.view baseViewData testActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ \q ->
                                q
                                    |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "menu-collab-panel") ]
                                    |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "menu-builder-name-input") ]
                            , \q ->
                                q
                                    |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "menu-host-room-button") ]
                            , \q ->
                                q
                                    |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "menu-room-code-input") ]
                            , \q ->
                                q
                                    |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "menu-join-room-button") ]
                            ]
            , test "renders collaborative lobby in active room with code, copy link, QR code, enter and leave buttons" <|
                \_ ->
                    let
                        dataWithRoom =
                            { baseViewData | room = activeCollabRoom }
                    in
                    MainMenu.view dataWithRoom testActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ \q ->
                                q
                                    |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "menu-lobby-room-code") ]
                                    |> Query.has [ Selector.text "ABCD" ]
                            , \q ->
                                q
                                    |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "menu-copy-room-link") ]
                            , \q ->
                                q
                                    |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "menu-room-qrcode") ]
                            , \q ->
                                q
                                    |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "menu-enter-room-city-button") ]
                            , \q ->
                                q
                                    |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "menu-leave-room-button") ]
                            ]
            ]
        ]
