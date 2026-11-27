module UiFlowTest exposing (suite)

{-| Unit tests for Brick City UI workflows:

  - Organized microscale part drawer coverage (`City.Editor.categoriesForDrawer`)
  - `Pages.Build` tab switching, overlay restore, template application,
    bounds-checked `Save & Return`, and instant `TidyUp` + `Undo`
  - Streamlined Top Bar, Bottom Dock, Height Meter, Part Counter, and Bounds Safety indicator
  - Decluttered City View HUD, segmented Camera Mode dock, and floating Lot Inspector
  - Bilingual (`Fi` / `En`) labels for the decluttered City View and Builder HUD

-}

import City.Editor as Editor exposing (PartDrawer(..))
import City.I18n as T
import City.Mesh as Mesh
import City.Render as Render
import City.Stats as Stats
import City.Templates as Templates
import City.Types exposing (CityGrid, CityRoom, ElementCategory(..), LotCoord, RoomConnection(..), RoomRole(..))
import Expect
import Html.Attributes as Attr
import I18n exposing (Language(..))
import LDraw.Document as Document
import Pages.Build as Build exposing (BuilderTab(..), Msg(..), Outcome(..))
import Pages.CityView as CityView exposing (CameraMode(..), CityMenuModal(..))
import Render.Camera as Camera exposing (Camera, Navigation(..))
import Studio.Core as Core
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


type TestMsg
    = NoOp


baseConfig :
    { ldrawBase : String
    , ldrawFallbackBase : String
    , supportsPointerEvents : Bool
    , language : Language
    , width : Int
    , height : Int
    }
baseConfig =
    { ldrawBase = "/raw"
    , ldrawFallbackBase = "/raw"
    , supportsPointerEvents = True
    , language = Fi
    , width = 1280
    , height = 800
    }


sampleDocWithMisalignedBrick : Document.Document
sampleDocWithMisalignedBrick =
    Document.parse
        """0 Micro Misaligned Test
1 71 0 0 0 1 0 0 0 1 0 0 0 1 41539.dat
1 4 10 -24 0 1 0 0 0 1 0 0 0 1 3005.dat
"""


initBuildWith : Document.Document -> Build.Model
initBuildWith doc =
    Build.init baseConfig
        (LotCoord 0 0)
        { name = "Test Lot"
        , category = Custom
        , templateId = Nothing
        , document = doc
        }
        |> Tuple.first


updateModel : Msg -> Build.Model -> ( Build.Model, Outcome )
updateModel msg model =
    let
        ( next, _, outcome ) =
            Build.update msg model
    in
    ( next, outcome )


mockActions : CityView.Actions TestMsg
mockActions =
    { onOpenMainMenu = NoOp
    , onCityNameChanged = \_ -> NoOp
    , onResizeGrid = \_ -> NoOp
    , onPointerDown = \_ _ _ _ -> NoOp
    , onPointerMove = \_ _ -> NoOp
    , onPointerUp = \_ _ -> NoOp
    , onDoubleClick = \_ _ -> NoOp
    , onWheel = \_ -> NoOp
    , onGlobePress = \_ -> NoOp
    , onGlobeMove = \_ -> NoOp
    , onGlobeRelease = NoOp
    , onGlobeZoom = \_ -> NoOp
    , onGlobeZoomDone = NoOp
    , onGlobeReset = NoOp
    , onSetCameraMode = \_ -> NoOp
    , onEditSelectedLot = NoOp
    , onRotateSelectedLot = NoOp
    , onCopySelectedLot = NoOp
    , onPasteSelectedLot = NoOp
    , onClearSelectedLot = NoOp
    , onStampTemplate = \_ -> NoOp
    , onToggleLotTemplates = NoOp
    , onDeselectLot = NoOp
    , onToggleModal = \_ -> NoOp
    , onHostRoom = NoOp
    , onJoinRoom = NoOp
    , onLeaveRoom = NoOp
    , onRoomCodeInput = \_ -> NoOp
    , onCopyRoomLink = NoOp
    , onLoadStarterCity = NoOp
    , onNewEmptyCity = NoOp
    , onExportMpd = NoOp
    , onExportJson = NoOp
    , onImportJson = NoOp
    , onToggleLanguage = NoOp
    , onToggleMute = NoOp
    , onNoOp = NoOp
    }


mockCityViewData :
    { city : CityGrid
    , stats : City.Types.CityStats
    , meshes : Mesh.MeshCache
    , camera : Camera
    , selectedLot : Maybe LotCoord
    , hoveredLot : Maybe LotCoord
    , clipboard : Maybe City.Types.LotElement
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
    , actions : CityView.Actions TestMsg
    }
mockCityViewData =
    let
        city =
            Templates.defaultStarterCity Fi
    in
    { city = city
    , stats = Stats.computeStats city
    , meshes = Mesh.empty
    , camera = Render.resetCameraForCity city 800
    , selectedLot = Just (LotCoord 0 0)
    , hoveredLot = Nothing
    , clipboard = Nothing
    , showLotTemplates = True
    , room =
        { roomId = "CITY"
        , role = LocalSolo
        , status = Disconnected
        , mySlot = 1
        , myName = "Builder"
        , peers = []
        }
    , roomCodeInput = ""
    , openModal = NoModal
    , toast = Nothing
    , audioMuted = False
    , language = Fi
    , baseUrl = "https://kaupunki.palikkaharrastajat.fi"
    , time = 0.0
    , width = 1280
    , height = 800
    , actions = mockActions
    }


{-| Test suite for Brick City UI flows and builder dock state.
-}
suite : Test
suite =
    describe "Brick City UI Flows"
        [ describe "City.Editor drawer organization"
            [ test "the 3 builder part drawers combine to cover all curated categories" <|
                \_ ->
                    let
                        combined =
                            Editor.categoriesForDrawer BricksDrawer
                                ++ Editor.categoriesForDrawer PlatesTilesDrawer
                                ++ Editor.categoriesForDrawer SnotSlopesDrawer
                    in
                    Expect.equal Editor.partCategories combined
            , test "each drawer contains at least 15 microscale parts" <|
                \_ ->
                    let
                        countDrawer d =
                            Editor.categoriesForDrawer d
                                |> List.concatMap .parts
                                |> List.length
                    in
                    Expect.all
                        [ \_ -> Expect.atLeast 15 (countDrawer BricksDrawer)
                        , \_ -> Expect.atLeast 15 (countDrawer PlatesTilesDrawer)
                        , \_ -> Expect.atLeast 15 (countDrawer SnotSlopesDrawer)
                        ]
                        ()
            ]
        , describe "Pages.Build tabs, search, save, and Tidy Up"
            [ test "init defaults to BricksTab and remembers lastPartTab across modal overlays" <|
                \_ ->
                    let
                        m0 =
                            initBuildWith (Templates.emptyBaseplateDocument 71)

                        ( m1, _ ) =
                            updateModel (SelectTab SnotSlopesTab) m0

                        ( m2, _ ) =
                            updateModel (SelectTab LimitsTab) m1

                        ( m3, _ ) =
                            updateModel CloseOverlayTab m2
                    in
                    Expect.all
                        [ \_ -> Expect.equal BricksTab m0.activeTab
                        , \_ -> Expect.equal SnotSlopesTab m1.activeTab
                        , \_ -> Expect.equal LimitsTab m2.activeTab
                        , \_ -> Expect.equal SnotSlopesTab m3.activeTab
                        ]
                        ()
            , test "ToggleSearch clears query when closing" <|
                \_ ->
                    let
                        m0 =
                            initBuildWith (Templates.emptyBaseplateDocument 71)

                        ( m1, _ ) =
                            updateModel ToggleSearch m0

                        ( m2, _ ) =
                            updateModel (CatalogFilterChanged "slope") m1

                        ( m3, _ ) =
                            updateModel ToggleSearch m2
                    in
                    Expect.all
                        [ \_ -> Expect.equal True m1.catalogSearchOpen
                        , \_ -> Expect.equal "slope" m2.catalogFilter
                        , \_ -> Expect.equal False m3.catalogSearchOpen
                        , \_ -> Expect.equal "" m3.catalogFilter
                        ]
                        ()
            , test "ApplyTemplate loads template metadata and Finish emits Saved outcome" <|
                \_ ->
                    let
                        m0 =
                            initBuildWith (Templates.emptyBaseplateDocument 71)

                        ( m1, _ ) =
                            updateModel (ApplyTemplate Templates.microHouse) m0

                        ( m2, _ ) =
                            updateModel (NameChanged "  Kallio Villa  ") m1

                        ( _, outcome ) =
                            updateModel Finish m2
                    in
                    case outcome of
                        Saved info ->
                            Expect.all
                                [ \_ -> Expect.equal "Kallio Villa" info.name
                                , \_ -> Expect.equal (Just Templates.microHouse.id) info.templateId
                                , \_ -> Expect.atLeast 2 info.partCount
                                , \_ -> Expect.atMost 32 info.heightPlates
                                ]
                                ()

                        _ ->
                            Expect.fail "Expected Saved outcome from Finish"
            , test "TidyUp snaps misaligned bricks in one undoable step" <|
                \_ ->
                    let
                        m0 =
                            initBuildWith sampleDocWithMisalignedBrick

                        ( m1, _ ) =
                            updateModel TidyUp m0

                        ( m2, _ ) =
                            updateModel Undo m1
                    in
                    Expect.all
                        [ \_ -> Expect.greaterThan 0 (List.length m0.tidyFixes)
                        , \_ -> Expect.equal [] m1.tidyFixes
                        , \_ -> Expect.equal True (Core.canUndo m1.studio)
                        , \_ -> Expect.notEqual Nothing m1.notice
                        , \_ -> Expect.greaterThan 0 (List.length m2.tidyFixes)
                        ]
                        ()
            ]
        , describe "Pages.Build UI rendering & controls"
            [ test "top bar contains save button, cancel button, name input, height indicator, part counter, and tidy button" <|
                \_ ->
                    let
                        m =
                            initBuildWith sampleDocWithMisalignedBrick

                        viewHtml =
                            Build.view m
                    in
                    viewHtml
                        |> Query.fromHtml
                        |> Expect.all
                            [ \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "save-lot-button") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "cancel-lot-button") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "element-name-input") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "height-indicator") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "height-progress-bar") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "part-counter") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "top-tidy-button") ]
                            ]
            , test "bottom dock contains drawers, action toolbar with undo/redo, tidy, and bounds safety indicator" <|
                \_ ->
                    let
                        m =
                            initBuildWith (Templates.emptyBaseplateDocument 71)

                        viewHtml =
                            Build.view m
                    in
                    viewHtml
                        |> Query.fromHtml
                        |> Expect.all
                            [ \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "dock-tab-bricks") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "dock-tab-plates") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "dock-tab-snot") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "dock-tab-templates") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "undo-button") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "redo-button") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "tidy-up-button") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "bounds-safety-indicator") ]
                            ]
            , test "interactive builder buttons carry touch-target class for touch accessibility" <|
                \_ ->
                    let
                        m =
                            initBuildWith (Templates.emptyBaseplateDocument 71)

                        viewHtml =
                            Build.view m
                    in
                    viewHtml
                        |> Query.fromHtml
                        |> Query.findAll [ Selector.class "touch-target" ]
                        |> Query.count (Expect.atLeast 8)
            ]
        , describe "Pages.CityView HUD & Inspector UI rendering"
            [ test "top HUD renders frosted header with stats pill and controls" <|
                \_ ->
                    CityView.viewCity mockCityViewData
                        |> Query.fromHtml
                        |> Expect.all
                            [ \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "city-top-hud") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "nav-menu-button") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "city-name-input") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "city-stats-bar") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "stat-population") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "stat-buildings") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "stat-green") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "stat-score") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "collab-modal-button") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "city-menu-button") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "language-toggle-button") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "mute-toggle-button") ]
                            ]
            , test "bottom bar renders segmented camera mode pills and quick action bar" <|
                \_ ->
                    CityView.viewCity mockCityViewData
                        |> Query.fromHtml
                        |> Expect.all
                            [ \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "city-bottom-bar") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "camera-mode-orbit") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "toggle-walk-button") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "camera-mode-topdown") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "camera-fit-button") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "bar-edit-lot-button") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "bar-rotate-lot-button") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "bar-copy-lot-button") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "bar-close-lot-button") ]
                            ]
            , test "floating lot inspector renders coordinate badge, CTA, and template drawer" <|
                \_ ->
                    CityView.viewCity mockCityViewData
                        |> Query.fromHtml
                        |> Expect.all
                            [ \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "lot-inspector") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "edit-lot-3d-button") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "rotate-lot-button") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "copy-lot-button") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "paste-lot-button") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "clear-lot-button") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "toggle-lot-templates") ]
                            , \q -> q |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "stamp-micro-house") ]
                            ]
            ]
        , describe "City.Render camera presets"
            [ test "resetCameraForCity enters Orbit and topDownCameraForCity enters Top-down" <|
                \_ ->
                    let
                        city =
                            Templates.defaultStarterCity Fi

                        orbitCam =
                            Render.resetCameraForCity city 800

                        topCam =
                            Render.topDownCameraForCity city 800
                    in
                    Expect.all
                        [ \_ -> Expect.equal Orbit orbitCam.navigation
                        , \_ -> Expect.equal False (Render.isTopDownCamera orbitCam)
                        , \_ -> Expect.equal Orbit topCam.navigation
                        , \_ -> Expect.equal True (Render.isTopDownCamera topCam)
                        ]
                        ()
            ]
        , describe "City.I18n UI polish strings"
            [ test "provides distinct FI and EN labels for Builder and City HUD" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal "Tallenna ja palaa kaupunkiin" (T.saveAndReturnButton Fi)
                        , \_ -> Expect.equal "Save & Return" (T.saveAndReturnButton En)
                        , \_ -> Expect.equal "9 / 32 kerrosta" (T.heightMeterLabel Fi 9)
                        , \_ -> Expect.equal "9 / 32 plates" (T.heightMeterLabel En 9)
                        , \_ -> Expect.equal "Rakenna / Muokkaa" (T.actionBuildElement Fi True)
                        , \_ -> Expect.equal "Edit Element" (T.actionBuildElement En True)
                        , \_ -> Expect.equal "Ylhäältä" (T.modeTopDownLabel Fi)
                        , \_ -> Expect.equal "Top-down" (T.modeTopDownLabel En)
                        , \_ -> Expect.equal "8×8 Kunnossa" (T.boundsSafeShort Fi)
                        , \_ -> Expect.equal "8×8 Safe" (T.boundsSafeShort En)
                        , \_ -> Expect.equal "Katto lähellä" (T.boundsCeilingShort Fi)
                        , \_ -> Expect.equal "Near ceiling" (T.boundsCeilingShort En)
                        , \_ -> Expect.equal "Rajat ylitetty" (T.boundsExceededShort Fi)
                        , \_ -> Expect.equal "Over limits" (T.boundsExceededShort En)
                        , \_ -> Expect.equal "Poista valinta" (T.deselectLotLabel Fi)
                        , \_ -> Expect.equal "Deselect" (T.deselectLotLabel En)
                        ]
                        ()
            ]
        ]
