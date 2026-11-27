module TemplatesTest exposing (suite)

{-| Legality and build health verification for all starter lot templates.

Each starter element build is verified build-by-build for:

  - Strict 8x8 stud footprint (within -80..80 LDU in X and Z)
  - Strict height limit of 32 plates (within 0..256 LDU)
  - Intact 8x8 baseplate (`41539.dat`) at ground level (0, 0, 0)
  - Matching part count and declared height
  - Build health: no misaligned parts and no overlapping parts via `Studio.Health`
  - Valid catalog parts from the curated microscale palette

-}

import City.Bounds as Bounds
import City.Editor as Editor
import City.Templates as Templates exposing (Template)
import City.Types exposing (BoundsIssue)
import EmbeddedParts
import Expect
import LDraw.Document as Document exposing (Document, Placement)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Set exposing (Set)
import Studio.Health as Health
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Starter Templates Legality (Build by Build)"
        [ test "all 9 templates are registered in Templates.all" <|
            \_ ->
                Expect.equal 9 (List.length Templates.all)
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

        healthIssues : List Health.Issue
        healthIssues =
            Health.check EmbeddedParts.cache doc Set.empty
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
        , test "starts with an 8x8 baseplate (41539.dat) at (0, 0, 0)" <|
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
        , test "all parts belong to the catalog or embedded parts" <|
            \_ ->
                let
                    unknownParts =
                        placements
                            |> List.map .part
                            |> List.filter (\part -> not (isKnownPart part))
                in
                Expect.equal [] unknownParts
        , test "Studio.Health finds no overlapping or misaligned parts" <|
            \_ ->
                Expect.equal [] healthIssues
        ]


isKnownPart : String -> Bool
isKnownPart part =
    List.any (\entry -> entry.file == part) Editor.parts
        || part
        == "41539.dat"
        || part
        == "3001.dat"
        || part
        == "3004.dat"
        || part
        == "3005.dat"
        || part
        == "3020.dat"
        || part
        == "3022.dat"
        || part
        == "3023.dat"
        || part
        == "3024.dat"
        || part
        == "3062b.dat"
        || part
        == "3069b.dat"
        || part
        == "3941.dat"
        || part
        == "4589.dat"
        || part
        == "4740.dat"
        || part
        == "6141.dat"
        || part
        == "2412b.dat"
        || part
        == "2877.dat"
        || part
        == "3659.dat"
        || part
        == "3665.dat"
        || part
        == "3665b.dat"
        || part
        == "4032b.dat"
        || part
        == "54200.dat"
        || part
        == "85861.dat"
        || part
        == "85984.dat"
        || part
        == "87079.dat"
        || part
        == "98283.dat"
        || part
        == "24866.dat"
        || part
        == "2431.dat"
        || part
        == "3039.dat"
