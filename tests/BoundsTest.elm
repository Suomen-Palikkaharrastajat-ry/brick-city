module BoundsTest exposing (suite)

{-| Unit tests for microscale 8x8 lot bounds, height limit 32 enforcement,
grid math, starter template validity, and editor catalog connectivity.
-}

import City.Bounds as Bounds
import City.Editor as Editor
import City.Templates as Templates
import City.Types exposing (BoundsIssue(..), LotCoord)
import Expect
import LDraw.Connectivity as Connectivity
import LDraw.Document as Document
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "City.Bounds and 8x8 Microscale Rules"
        [ describe "Grid math"
            [ test "adjacent lots in 4x4 grid are spaced 160 LDU (8 studs) apart" <|
                \_ ->
                    let
                        a =
                            Bounds.lotWorldCenter 4 4 (LotCoord 0 0)

                        b =
                            Bounds.lotWorldCenter 4 4 (LotCoord 0 1)

                        c =
                            Bounds.lotWorldCenter 4 4 (LotCoord 1 0)
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.01) 160.0 (b.x - a.x)
                        , \_ -> Expect.within (Expect.Absolute 0.01) 160.0 (c.z - a.z)
                        , \_ -> Expect.within (Expect.Absolute 0.01) -240.0 a.x
                        , \_ -> Expect.within (Expect.Absolute 0.01) -240.0 a.z
                        ]
                        ()
            , test "clampGridSize restricts dimensions to 2..8" <|
                \_ ->
                    [ Bounds.clampGridSize 0, Bounds.clampGridSize 4, Bounds.clampGridSize 12 ]
                        |> Expect.equal [ 2, 4, 8 ]
            ]
        , describe "8x8 footprint and height 32 validation"
            [ test "empty 8x8 baseplate at origin is valid with height 0" <|
                \_ ->
                    let
                        doc =
                            Templates.emptyBaseplateDocument 71
                    in
                    Bounds.validateDocument doc
                        |> Expect.equal (Ok { heightPlates = 0, partCount = 1 })
            , test "stack reaching exactly 32 plates (256 LDU) passes validation" <|
                \_ ->
                    let
                        doc =
                            Document.parse
                                (String.join "\n"
                                    [ "1 71 0 0 0 1 0 0 0 1 0 0 0 1 41539.dat"
                                    , "1 15 0 -256 0 1 0 0 0 1 0 0 0 1 3024.dat"
                                    ]
                                )
                    in
                    Bounds.validateDocument doc
                        |> Expect.equal (Ok { heightPlates = 32, partCount = 2 })
            , test "stack at 33 plates (264 LDU) fails with ExceedsMaxHeight 33" <|
                \_ ->
                    let
                        doc =
                            Document.parse
                                (String.join "\n"
                                    [ "1 71 0 0 0 1 0 0 0 1 0 0 0 1 41539.dat"
                                    , "1 15 0 -264 0 1 0 0 0 1 0 0 0 1 3024.dat"
                                    ]
                                )
                    in
                    Bounds.validateDocument doc
                        |> Expect.equal (Err [ ExceedsMaxHeight 33 ])
            , test "part overhanging +X past +80 LDU fails with OutOfBoundsX" <|
                \_ ->
                    let
                        doc =
                            Document.parse
                                (String.join "\n"
                                    [ "1 71 0 0 0 1 0 0 0 1 0 0 0 1 41539.dat"
                                    , "1 4 90 -24 0 1 0 0 0 1 0 0 0 1 3005.dat"
                                    ]
                                )
                    in
                    case Bounds.validateDocument doc of
                        Err [ OutOfBoundsX _ maxX ] ->
                            Expect.greaterThan 80.5 maxX

                        other ->
                            Expect.fail ("Expected OutOfBoundsX, got " ++ Debug.toString other)
            , test "part overhanging -Z past -80 LDU fails with OutOfBoundsZ" <|
                \_ ->
                    let
                        doc =
                            Document.parse
                                (String.join "\n"
                                    [ "1 71 0 0 0 1 0 0 0 1 0 0 0 1 41539.dat"
                                    , "1 4 0 -24 -90 1 0 0 0 1 0 0 0 1 3005.dat"
                                    ]
                                )
                    in
                    case Bounds.validateDocument doc of
                        Err [ OutOfBoundsZ minZ _ ] ->
                            Expect.lessThan -80.5 minZ

                        other ->
                            Expect.fail ("Expected OutOfBoundsZ, got " ++ Debug.toString other)
            , test "part placed below baseplate fails with BelowBaseplate" <|
                \_ ->
                    let
                        doc =
                            Document.parse
                                (String.join "\n"
                                    [ "1 71 0 0 0 1 0 0 0 1 0 0 0 1 41539.dat"
                                    , "1 4 0 8 0 1 0 0 0 1 0 0 0 1 3005.dat"
                                    ]
                                )
                    in
                    case Bounds.validateDocument doc of
                        Err [ BelowBaseplate y ] ->
                            Expect.greaterThan 8.5 y

                        other ->
                            Expect.fail ("Expected BelowBaseplate, got " ++ Debug.toString other)
            , test "empty document fails with EmptyElement" <|
                \_ ->
                    Bounds.validateDocument Document.empty
                        |> Expect.equal (Err [ EmptyElement ])
            , test "violatingPlacementIds flags only the out-of-bounds placement" <|
                \_ ->
                    let
                        doc =
                            Document.parse
                                (String.join "\n"
                                    [ "1 71 0 0 0 1 0 0 0 1 0 0 0 1 41539.dat"
                                    , "1 15 10 -24 10 1 0 0 0 1 0 0 0 1 3005.dat"
                                    , "1 4 90 -24 0 1 0 0 0 1 0 0 0 1 3005.dat"
                                    ]
                                )
                    in
                    Bounds.violatingPlacementIds doc
                        |> Set.size
                        |> Expect.equal 1
            ]
        , describe "Starter templates and catalog"
            [ test "all 9 starter templates fit strictly within 8x8 bounds and height 32" <|
                \_ ->
                    Templates.all
                        |> List.map
                            (\tmpl ->
                                ( tmpl.id
                                , Bounds.validateDocument (Document.parse tmpl.ldrawText)
                                )
                            )
                        |> List.filterMap
                            (\( id, res ) ->
                                case res of
                                    Ok info ->
                                        if info.heightPlates <= 32 && info.partCount > 1 then
                                            Nothing

                                        else
                                            Just (id ++ " invalid metrics")

                                    Err errs ->
                                        Just (id ++ ": " ++ Debug.toString errs)
                            )
                        |> Expect.equal []
            , test "every curated microscale editor part has Connectivity metadata" <|
                \_ ->
                    Editor.parts
                        |> List.filter (\entry -> Connectivity.knownCatalogPart entry.file == Nothing)
                        |> List.map .file
                        |> Expect.equal []
            ]
        ]
