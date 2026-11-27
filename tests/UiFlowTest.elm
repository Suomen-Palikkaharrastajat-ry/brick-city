module UiFlowTest exposing (suite)

{-| Unit tests for Brick City UI workflows:

  - Organized microscale part drawer coverage (`City.Editor.categoriesForDrawer`)
  - `Pages.Build` tab switching, overlay restore, template application,
    bounds-checked `Save & Return`, and instant `TidyUp` + `Undo`
  - Bilingual (`Fi` / `En`) labels for the decluttered City View and Builder HUD

-}

import City.Editor as Editor exposing (PartDrawer(..))
import City.I18n as T
import City.Render as Render
import City.Templates as Templates
import City.Types exposing (ElementCategory(..), LotCoord)
import Expect
import I18n exposing (Language(..))
import LDraw.Document as Document
import Pages.Build as Build exposing (BuilderTab(..), Msg(..), Outcome(..))
import Render.Camera exposing (Navigation(..))
import Studio.Core as Core
import Test exposing (Test, describe, test)


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
                        ]
                        ()
            ]
        ]
