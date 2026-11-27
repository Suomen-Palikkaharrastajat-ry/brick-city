module TemplatesTest exposing (suite)

{-| Legality and build health verification for all starter lot templates.

Each starter element build is verified build-by-build for:

  - Strict 8x8 stud footprint (within -80..80 LDU in X and Z)
  - Strict height limit of 32 plates (within 0..256 LDU, topBodyY >= -256)
  - Intact 8x8 baseplate (`41539.dat`) at ground level (0, 0, 0)
  - Matching part count and declared height
  - Build health: zero misaligned parts and zero overlapping parts via `Studio.Health`
  - Valid catalog parts from the curated microscale palette (`Editor.parts`)

-}

import City.Bounds as Bounds
import City.Editor as Editor
import City.Templates as Templates exposing (Template)
import City.Types exposing (BoundsIssue)
import EmbeddedParts
import Expect
import LDraw.Connectivity as Connectivity
import LDraw.Document as Document exposing (Document, Placement)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Set exposing (Set)
import Studio.Health as Health
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Starter Templates Legality (Build by Build)"
        [ test "all 9 templates are registered in Templates.all with expected IDs" <|
            \_ ->
                let
                    expectedIds =
                        [ "straight-road"
                        , "corner-road"
                        , "intersection"
                        , "micro-house"
                        , "high-rise"
                        , "skyscraper"
                        , "central-park"
                        , "industrial-plant"
                        , "waterfront-canal"
                        ]

                    actualIds =
                        List.map .id Templates.all
                in
                Expect.equal expectedIds actualIds
        , test "all 9 starter templates have exact expected part counts and heights" <|
            \_ ->
                let
                    metrics =
                        Templates.all
                            |> List.map (\t -> ( t.id, t.partCount, t.heightPlates ))
                in
                Expect.equal
                    [ ( "straight-road", 25, 5 )
                    , ( "corner-road", 16, 5 )
                    , ( "intersection", 22, 1 )
                    , ( "micro-house", 15, 9 )
                    , ( "high-rise", 14, 19 )
                    , ( "skyscraper", 17, 29 )
                    , ( "central-park", 23, 9 )
                    , ( "industrial-plant", 17, 10 )
                    , ( "waterfront-canal", 20, 9 )
                    ]
                    metrics
        , describe "Individual template legality checks"
            (List.map templateLegalityTest Templates.all)
        ]


templateLegalityTest : Template -> Test
templateLegalityTest tmpl =
    let
        doc : Document
        doc =
            Document.parse tmpl.ldrawText

        placements : List Placement
        placements =
            Document.mainPlacements doc

        bounds : Bounds.DocumentBounds
        bounds =
            Bounds.documentBounds doc

        healthIssues : List Health.Issue
        healthIssues =
            Health.check EmbeddedParts.cache doc Set.empty

        misalignedIssues : List Health.Issue
        misalignedIssues =
            List.filter
                (\issue ->
                    case issue of
                        Health.Misaligned _ ->
                            True

                        Health.Overlapping _ ->
                            False
                )
                healthIssues

        overlappingIssues : List Health.Issue
        overlappingIssues =
            List.filter
                (\issue ->
                    case issue of
                        Health.Overlapping _ ->
                            True

                        Health.Misaligned _ ->
                            False
                )
                healthIssues
    in
    describe ("Template: " ++ tmpl.id ++ " (" ++ tmpl.nameEn ++ ")")
        [ test "passes Bounds.validateDocument with declared metrics" <|
            \_ ->
                case Bounds.validateDocument doc of
                    Ok info ->
                        Expect.all
                            [ \_ -> Expect.equal tmpl.partCount info.partCount
                            , \_ -> Expect.equal tmpl.heightPlates info.heightPlates
                            , \_ -> Expect.atMost 32 info.heightPlates
                            , \_ -> Expect.atLeast 1 info.partCount
                            ]
                            ()

                    Err issues ->
                        Expect.fail ("Bounds validation failed: " ++ Debug.toString issues)
        , test "strictly complies with 8x8 stud footprint [-80..80] LDU in X and Z" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.greaterThan -80.5 bounds.minX
                    , \_ -> Expect.lessThan 80.5 bounds.maxX
                    , \_ -> Expect.greaterThan -80.5 bounds.minZ
                    , \_ -> Expect.lessThan 80.5 bounds.maxZ
                    , \_ -> Expect.equal Set.empty (Bounds.violatingPlacementIds doc)
                    ]
                    ()
        , test "strictly complies with height limit <= 32 plates (<= 256.0 LDU)" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.atMost 32 bounds.heightPlates
                    , \_ -> Expect.greaterThan -256.5 bounds.topBodyY
                    ]
                    ()
        , test "starts with an intact 8x8 baseplate (41539.dat) at (0, 0, 0)" <|
            \_ ->
                case List.head placements of
                    Just baseplate ->
                        let
                            pos =
                                Mat4.transform baseplate.matrix (vec3 0 0 0)
                        in
                        Expect.all
                            [ \_ -> Expect.equal "41539.dat" baseplate.part
                            , \_ -> Expect.within (Expect.Absolute 0.01) 0.0 (Vec3.getX pos)
                            , \_ -> Expect.within (Expect.Absolute 0.01) 0.0 (Vec3.getY pos)
                            , \_ -> Expect.within (Expect.Absolute 0.01) 0.0 (Vec3.getZ pos)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Missing baseplate"
        , test "every part belongs to Editor.parts or is the 8x8 baseplate" <|
            \_ ->
                let
                    unknownParts =
                        placements
                            |> List.map .part
                            |> List.filter (\part -> not (isKnownPart part))
                in
                Expect.equal [] unknownParts
        , test "Studio.Health finds zero misaligned parts" <|
            \_ ->
                Expect.equal [] misalignedIssues
        , test "Studio.Health finds zero overlapping parts" <|
            \_ ->
                Expect.equal [] overlappingIssues
        , test "every non-baseplate part connects to studs below" <|
            \_ ->
                let
                    unconnected =
                        placements
                            |> List.filter (\p -> not (Editor.isBaseplatePlacement p))
                            |> List.filter (\p -> not (hasStudConnection placements p))
                            |> List.map
                                (\p ->
                                    let
                                        pos =
                                            Mat4.transform p.matrix (vec3 0 0 0) |> Vec3.toRecord
                                    in
                                    ( p.part, p.id, ( pos.x, pos.y, pos.z ) )
                                )
                in
                Expect.equal [] unconnected
        ]


hasStudConnection : List Placement -> Placement -> Bool
hasStudConnection allPlacements p =
    let
        pConn =
            Connectivity.extractPartConnectivity p.part (\_ -> Nothing)

        pAntiStuds =
            pConn.connections
                |> List.filter (\c -> c.connType == Connectivity.AntiStud)
                |> List.map (Connectivity.transformPoint p.matrix p.id)

        otherStuds =
            allPlacements
                |> List.filter (\q -> q.id /= p.id)
                |> List.concatMap
                    (\q ->
                        let
                            qConn =
                                Connectivity.extractPartConnectivity q.part (\_ -> Nothing)
                        in
                        qConn.connections
                            |> List.filter (\c -> c.connType == Connectivity.Stud)
                            |> List.map (Connectivity.transformPoint q.matrix q.id)
                    )
    in
    List.any
        (\anti ->
            List.any
                (\stud ->
                    (Vec3.length (Vec3.sub anti.worldPosition stud.worldPosition) <= 0.5)
                        && (Vec3.dot anti.worldNormal stud.worldNormal < -0.7)
                )
                otherStuds
        )
        pAntiStuds


isKnownPart : String -> Bool
isKnownPart part =
    (part == "41539.dat")
        || List.any (\entry -> entry.file == part) Editor.parts
