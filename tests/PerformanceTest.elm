module PerformanceTest exposing (suite)

{-| Performance architecture and benchmark verification tests for Brick City:

  - Content-addressed lot mesh caching (`Dict String LotMesh`) with zero per-frame parsing
  - Part-level geometry instancing (`Dict String CachedPart`) preventing redundant subfile walks
  - Dual-LOD lot meshes (`lodTriangleMesh` without studs vs `triangleMesh` with studs)
  - Distance-based LOD switching (`Render.lotDetailFor`)
  - Gribb–Hartmann frustum culling (`Render.visibleLotCoords`)
  - Fast quarter-turn rotation transforms (`Render.lotModelMatrix`, `Render.rotateLotCenter`)
  - BFC CCW certification preserving GPU backface culling
  - Deterministic content hashing (`hashLdrawText`)

-}

import City.Bounds as Bounds
import City.Mesh as Mesh exposing (CachedPart, LotMesh, MeshCache)
import City.Render as Render
import City.Templates as Templates
import City.Types exposing (CityGrid, ElementCategory(..), LotCoord, LotElement, hashLdrawText)
import Dict
import Expect exposing (Expectation)
import I18n exposing (Language(..))
import LDraw.Document as Document
import LDraw.Parser as Parser
import LDraw.Resolve as Resolve exposing (PartCache, PartStatus(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render.Camera as Camera exposing (Camera, Navigation(..))
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Brick City Performance & Caching Architecture"
        [ meshCacheBasicsTests
        , refreshMeshesCachingTests
        , contentAddressedDeduplicationTests
        , dualLodTriangleSavingsTests
        , lotDetailLodSwitchingTests
        , frustumCullingTests
        , rotationTransformTests
        , bfcCertificationTests
        , contentHashingTests
        ]


meshCacheBasicsTests : Test
meshCacheBasicsTests =
    describe "MeshCache Singleton & Empty State"
        [ test "Mesh.empty starts with zero lots, zero parts, and zero BFC memo entries" <|
            \_ ->
                Expect.all
                    [ \c -> Expect.equal 0 (Mesh.lotCacheCount c)
                    , \c -> Expect.equal 0 (Mesh.partCacheCount c)
                    , \c -> Expect.equal False (Mesh.hasLotMesh Mesh.emptyLotKey c)
                    , \c -> Expect.equal Nothing (Mesh.emptyLotMesh c)
                    ]
                    Mesh.empty
        , test "Mesh.emptyLotKey is canonical and emptyLotLdraw declares BFC CCW" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal "__empty_lot__" Mesh.emptyLotKey
                    , \_ -> Expect.equal True (String.contains "0 BFC CERTIFY CCW" Mesh.emptyLotLdraw)
                    , \_ -> Expect.equal True (String.contains "41539.dat" Mesh.emptyLotLdraw)
                    ]
                    ()
        , test "Mesh.emptyLotDocument parses exactly 1 placement of 41539.dat at origin" <|
            \_ ->
                let
                    placements =
                        Document.flattenedPlacements Mesh.emptyLotDocument
                in
                case placements of
                    [ p ] ->
                        Expect.all
                            [ \_ -> Expect.equal "41539.dat" p.part
                            , \_ -> Expect.equal 2 p.color
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected exactly 1 placement in emptyLotDocument"
        ]


refreshMeshesCachingTests : Test
refreshMeshesCachingTests =
    describe "refreshMeshes Lifecycle & Caching"
        [ test "requests 41539.dat when partCache is empty" <|
            \_ ->
                let
                    grid =
                        Templates.blankCity "Test Grid" 4 4

                    result =
                        Mesh.refreshMeshes grid Resolve.initCache Mesh.empty
                in
                Expect.equal [ "41539.dat" ] result.fetch
        , test "compiles emptyLotMesh when 41539.dat is present in partCache" <|
            \_ ->
                let
                    grid =
                        Templates.blankCity "Test Grid" 4 4

                    result =
                        Mesh.refreshMeshes grid mockBaseplateCache Mesh.empty
                in
                Expect.all
                    [ \_ -> Expect.equal [] result.fetch
                    , \_ -> Expect.equal 1 (Mesh.lotCacheCount result.meshes)
                    , \_ -> Expect.equal True (Mesh.hasLotMesh Mesh.emptyLotKey result.meshes)
                    , \_ ->
                        case Mesh.emptyLotMesh result.meshes of
                            Just m ->
                                Expect.all
                                    [ \_ -> Expect.equal True m.bfcCertified
                                    , \_ -> Expect.greaterThan m.lodTriangleCount m.triangleCount
                                    ]
                                    ()

                            Nothing ->
                                Expect.fail "Expected emptyLotMesh to be Just"
                    ]
                    ()
        , test "re-running refreshMeshes with identical city and cache causes zero recompilations" <|
            \_ ->
                let
                    grid =
                        Templates.blankCity "Test Grid" 4 4

                    res1 =
                        Mesh.refreshMeshes grid mockBaseplateCache Mesh.empty

                    res2 =
                        Mesh.refreshMeshes grid mockBaseplateCache res1.meshes
                in
                Expect.all
                    [ \_ -> Expect.equal [] res2.fetch
                    , \_ -> Expect.equal (Mesh.lotCacheCount res1.meshes) (Mesh.lotCacheCount res2.meshes)
                    , \_ -> Expect.equal (Mesh.partCacheCount res1.meshes) (Mesh.partCacheCount res2.meshes)
                    , \_ -> Expect.equal (Dict.keys res1.meshes.lots) (Dict.keys res2.meshes.lots)
                    ]
                    ()
        ]


contentAddressedDeduplicationTests : Test
contentAddressedDeduplicationTests =
    describe "Content-Addressed Mesh Deduplication"
        [ test "multiple grid lots sharing identical LDraw text share 1 compiled mesh in cache" <|
            \_ ->
                let
                    lotA1 =
                        makeTestLot "Road A1" 0

                    lotA2 =
                        makeTestLot "Road A2" 1

                    lotA3 =
                        makeTestLot "Road A3" 2

                    lotA4 =
                        makeTestLot "Road A4" 3

                    grid : CityGrid
                    grid =
                        { cityName = "Deduplication Test"
                        , cols = 4
                        , rows = 4
                        , lots =
                            Dict.fromList
                                [ ( ( 0, 0 ), lotA1 )
                                , ( ( 0, 1 ), lotA2 )
                                , ( ( 1, 0 ), lotA3 )
                                , ( ( 1, 1 ), lotA4 )
                                ]
                        , version = 1
                        }

                    res =
                        Mesh.refreshMeshes grid mockBaseplateCache Mesh.empty
                in
                Expect.all
                    -- 4 lots placed in grid, but only 1 unique element mesh + 1 emptyLotMesh in cache!
                    [ \_ -> Expect.equal 2 (Mesh.lotCacheCount res.meshes)
                    , \_ -> Expect.equal True (Mesh.hasLotMesh lotA1.contentHash res.meshes)
                    , \_ -> Expect.equal True (Mesh.hasLotMesh lotA2.contentHash res.meshes)
                    , \_ -> Expect.equal True (Mesh.hasLotMesh lotA3.contentHash res.meshes)
                    , \_ -> Expect.equal True (Mesh.hasLotMesh lotA4.contentHash res.meshes)
                    , \_ ->
                        -- All 4 lots have identical contentHash
                        Expect.equal lotA1.contentHash lotA2.contentHash
                    ]
                    ()
        , test "compiles only new contentHash when a lot is modified, preserving existing cache" <|
            \_ ->
                let
                    roadLot =
                        makeTestLot "Road" 0

                    towerLot =
                        makeCustomLot "Tower" "3001.dat" 0

                    grid1 : CityGrid
                    grid1 =
                        { cityName = "Dynamic Edit Test"
                        , cols = 4
                        , rows = 4
                        , lots = Dict.singleton ( 0, 0 ) roadLot
                        , version = 1
                        }

                    res1 =
                        Mesh.refreshMeshes grid1 mockExpandedCache Mesh.empty

                    grid2 : CityGrid
                    grid2 =
                        { grid1 | lots = Dict.insert ( 0, 1 ) towerLot grid1.lots }

                    res2 =
                        Mesh.refreshMeshes grid2 mockExpandedCache res1.meshes
                in
                Expect.all
                    [ \_ -> Expect.equal 2 (Mesh.lotCacheCount res1.meshes)
                    , \_ -> Expect.equal 3 (Mesh.lotCacheCount res2.meshes)
                    , \_ -> Expect.equal True (Mesh.hasLotMesh roadLot.contentHash res2.meshes)
                    , \_ -> Expect.equal True (Mesh.hasLotMesh towerLot.contentHash res2.meshes)
                    , \_ -> Expect.equal True (Mesh.hasLotMesh Mesh.emptyLotKey res2.meshes)
                    ]
                    ()
        , test "8x8 GridMetropolis achieves > 80% lot mesh deduplication across 39 built lots" <|
            \_ ->
                let
                    city =
                        Templates.cityFromPreset En Templates.GridMetropolis "Metropolis" 8

                    builtLots =
                        Dict.size city.lots

                    uniqueHashes =
                        city.lots
                            |> Dict.values
                            |> List.map .contentHash
                            |> Set.fromList
                            |> Set.size

                    dedupRatio =
                        100.0 * toFloat (builtLots - uniqueHashes) / toFloat builtLots
                in
                Expect.all
                    [ \_ -> Expect.equal 39 builtLots
                    , \_ -> Expect.atMost 8 uniqueHashes
                    , \_ -> Expect.greaterThan 80.0 dedupRatio
                    ]
                    ()
        ]


dualLodTriangleSavingsTests : Test
dualLodTriangleSavingsTests =
    describe "Dual-LOD Meshes & Stud Culling Triangle Savings"
        [ test "building with studs yields lower triangle count in lodTriangleMesh" <|
            \_ ->
                let
                    lot =
                        makeTestLot "Studded Plate" 0

                    grid : CityGrid
                    grid =
                        { cityName = "LOD Test"
                        , cols = 4
                        , rows = 4
                        , lots = Dict.singleton ( 0, 0 ) lot
                        , version = 1
                        }

                    res =
                        Mesh.refreshMeshes grid mockBaseplateCache Mesh.empty
                in
                case Mesh.getLotMesh lot.contentHash res.meshes of
                    Just m ->
                        let
                            savingsPct =
                                100.0 * toFloat (m.triangleCount - m.lodTriangleCount) / toFloat m.triangleCount
                        in
                        Expect.all
                            [ \_ -> Expect.greaterThan m.lodTriangleCount m.triangleCount
                            , \_ -> Expect.greaterThan 0.0 savingsPct
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Expected lot mesh to be compiled"
        , test "flat model without studs shares same triangle count between full and LOD" <|
            \_ ->
                let
                    flatLot =
                        makeFlatLot "Flat Tile Lot" 0

                    grid : CityGrid
                    grid =
                        { cityName = "Flat Test"
                        , cols = 4
                        , rows = 4
                        , lots = Dict.singleton ( 0, 0 ) flatLot
                        , version = 1
                        }

                    res =
                        Mesh.refreshMeshes grid mockExpandedCache Mesh.empty
                in
                case Mesh.getLotMesh flatLot.contentHash res.meshes of
                    Just m ->
                        Expect.equal m.triangleCount m.lodTriangleCount

                    Nothing ->
                        Expect.fail "Expected flat lot mesh to be compiled"
        ]


lotDetailLodSwitchingTests : Test
lotDetailLodSwitchingTests =
    describe "Distance LOD & Edge Width Logic (Render.lotDetailFor)"
        [ test "distant unhighlighted building switches to LOD (NoStuds) and disables edge lines" <|
            \_ ->
                let
                    detail =
                        Render.lotDetailFor
                            { isEmpty = False
                            , isHighlighted = False
                            , lotScreenPx = 60.0
                            }
                in
                Expect.all
                    [ \_ -> Expect.equal True detail.useLod
                    , \_ -> Expect.within (Expect.Absolute 0.01) 0.0 detail.edgeWidth
                    ]
                    ()
        , test "foreground unhighlighted building uses full detail with edge lines" <|
            \_ ->
                let
                    detail =
                        Render.lotDetailFor
                            { isEmpty = False
                            , isHighlighted = False
                            , lotScreenPx = 150.0
                            }
                in
                Expect.all
                    [ \_ -> Expect.equal False detail.useLod
                    , \_ -> Expect.greaterThan 0.3 detail.edgeWidth
                    ]
                    ()
        , test "highlighted building keeps full detail and boosted edge lines even at distance" <|
            \_ ->
                let
                    detail =
                        Render.lotDetailFor
                            { isEmpty = False
                            , isHighlighted = True
                            , lotScreenPx = 60.0
                            }
                in
                Expect.all
                    [ \_ -> Expect.equal False detail.useLod
                    , \_ -> Expect.greaterThan 0.0 detail.edgeWidth
                    ]
                    ()
        , test "distant empty grass lot switches to LOD and skips edge lines" <|
            \_ ->
                let
                    detail =
                        Render.lotDetailFor
                            { isEmpty = True
                            , isHighlighted = False
                            , lotScreenPx = 100.0
                            }
                in
                Expect.all
                    [ \_ -> Expect.equal True detail.useLod
                    , \_ -> Expect.within (Expect.Absolute 0.01) 0.0 detail.edgeWidth
                    ]
                    ()
        , test "foreground empty grass lot uses full detail" <|
            \_ ->
                let
                    detail =
                        Render.lotDetailFor
                            { isEmpty = True
                            , isHighlighted = False
                            , lotScreenPx = 200.0
                            }
                in
                Expect.all
                    [ \_ -> Expect.equal False detail.useLod
                    , \_ -> Expect.greaterThan 0.3 detail.edgeWidth
                    ]
                    ()
        ]


frustumCullingTests : Test
frustumCullingTests =
    describe "Gribb–Hartmann Frustum Culling (Render.visibleLotCoords)"
        [ test "camera framing the city sees all lots within the frustum" <|
            \_ ->
                let
                    grid =
                        Templates.blankCity "Frustum Grid" 4 4

                    cam =
                        Render.resetCameraForCity grid 600

                    visible =
                        Render.visibleLotCoords cam 800 600 grid Mesh.empty
                in
                Expect.equal 16 (List.length visible)
        , test "camera positioned far away looking opposite direction culls all lots" <|
            \_ ->
                let
                    grid =
                        Templates.blankCity "Frustum Grid" 4 4

                    cam =
                        Camera.init
                            |> (\c -> { c | target = vec3 5000 0 5000, distance = 200, elevation = 0.5, azimuth = 0 })

                    visible =
                        Render.visibleLotCoords cam 800 600 grid Mesh.empty
                in
                Expect.equal 0 (List.length visible)
        , test "isTopDownCamera correctly detects overhead plan view" <|
            \_ ->
                let
                    topDownCam =
                        Render.topDownCameraForCity (Templates.blankCity "Test" 4 4) 600

                    orbitCam =
                        Render.resetCameraForCity (Templates.blankCity "Test" 4 4) 600

                    walkCam =
                        { topDownCam | navigation = Walk }
                in
                Expect.all
                    [ \_ -> Expect.equal True (Render.isTopDownCamera topDownCam)
                    , \_ -> Expect.equal False (Render.isTopDownCamera orbitCam)
                    , \_ -> Expect.equal False (Render.isTopDownCamera walkCam)
                    ]
                    ()
        ]


rotationTransformTests : Test
rotationTransformTests =
    describe "Quarter-Turn Rotation Math Invariance"
        [ test "rotateLotCenter exactly matches Mat4.transform lotModelMatrix for all 4 rotations" <|
            \_ ->
                let
                    cx =
                        160.0

                    cz =
                        -80.0

                    testPt =
                        vec3 40.0 24.0 60.0

                    verifyRotation rot =
                        let
                            rotatedPt =
                                Render.rotateLotCenter rot cx cz testPt

                            matrixPt =
                                Mat4.transform (Render.lotModelMatrix cx cz rot) testPt

                            diff =
                                Vec3.distance rotatedPt matrixPt
                        in
                        Expect.within (Expect.Absolute 0.001) 0.0 diff
                in
                Expect.all
                    [ \_ -> verifyRotation 0
                    , \_ -> verifyRotation 1
                    , \_ -> verifyRotation 2
                    , \_ -> verifyRotation 3
                    ]
                    ()
        , test "rotation is strictly cyclic modulo 4" <|
            \_ ->
                let
                    cx =
                        0.0

                    cz =
                        0.0

                    pt =
                        vec3 10.0 5.0 -15.0

                    r0 =
                        Render.rotateLotCenter 0 cx cz pt

                    r4 =
                        Render.rotateLotCenter 4 cx cz pt

                    r1 =
                        Render.rotateLotCenter 1 cx cz pt

                    r5 =
                        Render.rotateLotCenter 5 cx cz pt
                in
                Expect.all
                    [ \_ -> Expect.equal r0 r4
                    , \_ -> Expect.equal r1 r5
                    ]
                    ()
        ]


bfcCertificationTests : Test
bfcCertificationTests =
    describe "Backface Culling Certification"
        [ test "standard BFC CCW model compiles to bfcCertified = True" <|
            \_ ->
                let
                    lot =
                        makeTestLot "BFC Safe Lot" 0

                    grid : CityGrid
                    grid =
                        { cityName = "BFC Test"
                        , cols = 4
                        , rows = 4
                        , lots = Dict.singleton ( 0, 0 ) lot
                        , version = 1
                        }

                    res =
                        Mesh.refreshMeshes grid mockBaseplateCache Mesh.empty
                in
                case Mesh.getLotMesh lot.contentHash res.meshes of
                    Just m ->
                        Expect.equal True m.bfcCertified

                    Nothing ->
                        Expect.fail "Expected lot mesh to be compiled"
        , test "model containing BFC NOCERTIFY disables backface culling" <|
            \_ ->
                let
                    badLdraw =
                        "0 FILE non-bfc.ldr\n0 Name: Non-BFC Lot\n0 BFC NOCERTIFY\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 41539.dat"

                    doc =
                        Document.parse badLdraw

                    badLot : LotElement
                    badLot =
                        { id = "non-bfc-1"
                        , name = "Non-BFC"
                        , category = Custom
                        , ldrawText = badLdraw
                        , document = doc
                        , contentHash = hashLdrawText badLdraw
                        , templateId = Nothing
                        , rotation = 0
                        , updatedAt = 1000
                        , updatedBy = "builder"
                        , partCount = 1
                        , heightPlates = 1
                        }

                    grid : CityGrid
                    grid =
                        { cityName = "Non-BFC Test"
                        , cols = 4
                        , rows = 4
                        , lots = Dict.singleton ( 0, 0 ) badLot
                        , version = 1
                        }

                    res =
                        Mesh.refreshMeshes grid mockBaseplateCache Mesh.empty
                in
                case Mesh.getLotMesh badLot.contentHash res.meshes of
                    Just m ->
                        Expect.equal False m.bfcCertified

                    Nothing ->
                        Expect.fail "Expected non-bfc lot mesh to be compiled"
        ]


contentHashingTests : Test
contentHashingTests =
    describe "Deterministic Content Hashing (hashLdrawText)"
        [ test "identical LDraw string always produces identical hash" <|
            \_ ->
                let
                    text =
                        "0 FILE test.ldr\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"

                    h1 =
                        hashLdrawText text

                    h2 =
                        hashLdrawText text
                in
                Expect.equal h1 h2
        , test "changing a single character alters the content hash" <|
            \_ ->
                let
                    t1 =
                        "1 16 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"

                    t2 =
                        "1 16 0 0 0 1 0 0 0 1 0 0 0 1 3004.dat"
                in
                Expect.notEqual (hashLdrawText t1) (hashLdrawText t2)
        , test "hash contains non-empty tokens and character count" <|
            \_ ->
                let
                    text =
                        "0 FILE sample.ldr"

                    h =
                        hashLdrawText text

                    parts =
                        String.split ":" h
                in
                Expect.all
                    [ \_ -> Expect.equal 3 (List.length parts)
                    , \_ -> Expect.equal (Just (String.fromInt (String.length text))) (List.head (List.reverse parts))
                    ]
                    ()
        ]



-- HELPER FIXTURES & MOCK CACHES


mockBaseplateCache : PartCache
mockBaseplateCache =
    let
        baseplateContent =
            String.join "\n"
                [ "0 BFC CERTIFY CCW"
                , "4 16 -80 0 -80 80 0 -80 80 0 80 -80 0 80"
                , "1 16 -70 -4 -70 1 0 0 0 1 0 0 0 1 stud.dat"
                , "1 16 70 -4 70 1 0 0 0 1 0 0 0 1 stud.dat"
                ]

        studContent =
            String.join "\n"
                [ "0 BFC CERTIFY CCW"
                , "4 16 -6 0 -6 6 0 -6 6 0 6 -6 0 6"
                ]
    in
    Dict.fromList
        [ ( "41539.dat", Loaded (Parser.parseFile baseplateContent) )
        , ( "stud.dat", Loaded (Parser.parseFile studContent) )
        ]


mockExpandedCache : PartCache
mockExpandedCache =
    let
        brickContent =
            String.join "\n"
                [ "0 BFC CERTIFY CCW"
                , "4 16 -40 -24 -20 40 -24 -20 40 0 -20 -40 0 -20"
                , "1 16 0 -28 0 1 0 0 0 1 0 0 0 1 stud.dat"
                ]

        flatTileContent =
            String.join "\n"
                [ "0 BFC CERTIFY CCW"
                , "4 16 -20 -8 -20 20 -8 -20 20 0 -20 -20 0 -20"
                ]
    in
    mockBaseplateCache
        |> Dict.insert "3001.dat" (Loaded (Parser.parseFile brickContent))
        |> Dict.insert "3069b.dat" (Loaded (Parser.parseFile flatTileContent))


sharedTestLdraw : String
sharedTestLdraw =
    String.join "\n"
        [ "0 FILE test-lot.ldr"
        , "0 Name: Test Lot"
        , "0 BFC CERTIFY CCW"
        , "1 4 0 0 0 1 0 0 0 1 0 0 0 1 41539.dat"
        ]


makeTestLot : String -> Int -> LotElement
makeTestLot instanceId rotation =
    let
        ldraw =
            sharedTestLdraw

        doc =
            Document.parse ldraw
    in
    { id = instanceId
    , name = "Test Lot"
    , category = Road
    , ldrawText = ldraw
    , document = doc
    , contentHash = hashLdrawText ldraw
    , templateId = Nothing
    , rotation = modBy 4 rotation
    , updatedAt = 1000
    , updatedBy = "builder"
    , partCount = 1
    , heightPlates = 1
    }


makeCustomLot : String -> String -> Int -> LotElement
makeCustomLot name part rotation =
    let
        ldraw =
            String.join "\n"
                [ "0 FILE custom-lot.ldr"
                , "0 Name: " ++ name
                , "0 BFC CERTIFY CCW"
                , "1 4 0 0 0 1 0 0 0 1 0 0 0 1 41539.dat"
                , "1 1 0 -24 0 1 0 0 0 1 0 0 0 1 " ++ part
                ]

        doc =
            Document.parse ldraw
    in
    { id = name
    , name = name
    , category = Commercial
    , ldrawText = ldraw
    , document = doc
    , contentHash = hashLdrawText ldraw
    , templateId = Nothing
    , rotation = modBy 4 rotation
    , updatedAt = 1000
    , updatedBy = "builder"
    , partCount = 2
    , heightPlates = 4
    }


makeFlatLot : String -> Int -> LotElement
makeFlatLot name rotation =
    let
        ldraw =
            String.join "\n"
                [ "0 FILE flat-lot.ldr"
                , "0 Name: " ++ name
                , "0 BFC CERTIFY CCW"
                , "1 1 0 0 0 1 0 0 0 1 0 0 0 1 3069b.dat"
                ]

        doc =
            Document.parse ldraw
    in
    { id = name
    , name = name
    , category = Park
    , ldrawText = ldraw
    , document = doc
    , contentHash = hashLdrawText ldraw
    , templateId = Nothing
    , rotation = modBy 4 rotation
    , updatedAt = 1000
    , updatedBy = "builder"
    , partCount = 1
    , heightPlates = 1
    }
