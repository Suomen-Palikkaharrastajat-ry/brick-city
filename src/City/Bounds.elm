module City.Bounds exposing
    ( DocumentBounds
    , baseplateBottomY
    , clampGridSize
    , documentBounds
    , isDocumentValid
    , lotGapLdu
    , lotHalfLdu
    , lotRotationMatrix
    , lotSizeLdu
    , lotStuds
    , lotWorldCenter
    , maxElementParts
    , maxGridSize
    , maxHeightLdu
    , maxHeightPlates
    , minGridSize
    , placementBounds
    , plateHeight
    , studPitch
    , validateDocument
    , violatingPlacementIds
    )

{-| Microscale 8x8 lot geometry, coordinate transforms, and bounding validation.

Every Brick City lot is an 8x8 stud plate (`41539.dat`) spanning `-80..80` LDU
in X and Z (`160 x 160` LDU), with a strict vertical ceiling of **32 plates**
(`256` LDU) above the baseplate top (`Y = 0` upward to `Y = -256` in LDraw).

-}

import City.Types exposing (BoundsIssue(..), LotCoord)
import LDraw.Connectivity as Connectivity exposing (BoundingBox3D, ConnectionType(..))
import LDraw.Document as Document exposing (Document, Placement)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Set exposing (Set)


{-| Horizontal distance between adjacent stud centers in LDraw units.
-}
studPitch : Float
studPitch =
    20.0


{-| Height of one LEGO plate in LDraw units.
-}
plateHeight : Float
plateHeight =
    8.0


{-| Number of studs along each side of one city lot.
-}
lotStuds : Int
lotStuds =
    8


{-| Full side length of one 8x8 lot in LDraw units (`8 * 20 = 160`).
-}
lotSizeLdu : Float
lotSizeLdu =
    toFloat lotStuds * studPitch


{-| Half side length of one 8x8 lot in LDraw units (`80.0`).
-}
lotHalfLdu : Float
lotHalfLdu =
    lotSizeLdu / 2.0


{-| Gap between adjacent lots in LDU (`0.0` so roads and canals link flush).
-}
lotGapLdu : Float
lotGapLdu =
    0.0


{-| Maximum vertical building height in plates above the baseplate top.
-}
maxHeightPlates : Int
maxHeightPlates =
    32


{-| Maximum vertical building height in LDraw units (`32 * 8 = 256`).
-}
maxHeightLdu : Float
maxHeightLdu =
    toFloat maxHeightPlates * plateHeight


{-| Bottom Y of the 8x8 baseplate in LDraw units (`Y = 8` down from top `0`).
-}
baseplateBottomY : Float
baseplateBottomY =
    8.0


{-| Maximum number of parts allowed inside a single 8x8 lot element.
-}
maxElementParts : Int
maxElementParts =
    400


{-| Minimum grid dimension (2x2 lots).
-}
minGridSize : Int
minGridSize =
    2


{-| Maximum grid dimension (8x8 lots).
-}
maxGridSize : Int
maxGridSize =
    8


{-| Clamp a requested grid row/column count to `[minGridSize, maxGridSize]`.
-}
clampGridSize : Int -> Int
clampGridSize n =
    clamp minGridSize maxGridSize n


{-| World-space center `(x, z)` in LDU of `coord` inside a `cols x rows` city grid.
Centered around `(0, 0)` so the city dioramas remain symmetrical at any size.
-}
lotWorldCenter : Int -> Int -> LotCoord -> { x : Float, z : Float }
lotWorldCenter cols rows coord =
    let
        step =
            lotSizeLdu + lotGapLdu
    in
    { x = (toFloat coord.col - toFloat (cols - 1) / 2.0) * step
    , z = (toFloat coord.row - toFloat (rows - 1) / 2.0) * step
    }


{-| Rotation matrix about the Y axis for a quarter-turn step `0..3`.
-}
lotRotationMatrix : Int -> Mat4
lotRotationMatrix quarterTurns =
    let
        angle =
            toFloat (modBy 4 quarterTurns) * (pi / 2.0)
    in
    Mat4.makeRotate angle (vec3 0 1 0)


{-| Tolerance for floating-point matrix rotations on exact 8x8 boundaries.
-}
boundsEpsilon : Float
boundsEpsilon =
    0.5


{-| Measured LDraw-space bounds of an element document.
-}
type alias DocumentBounds =
    { minX : Float
    , maxX : Float
    , minZ : Float
    , maxZ : Float
    , topBodyY : Float
    , maxBottomY : Float
    , heightPlates : Int
    , partCount : Int
    }


{-| Measure the bounding box, body height in plates, and flattened part count
of an LDraw `Document`.
-}
documentBounds : Document -> DocumentBounds
documentBounds doc =
    let
        placements =
            Document.flattenedPlacements doc

        boxes =
            List.filterMap placementLeafBounds placements
    in
    case boxes of
        [] ->
            { minX = 0
            , maxX = 0
            , minZ = 0
            , maxZ = 0
            , topBodyY = 0
            , maxBottomY = 0
            , heightPlates = 0
            , partCount = 0
            }

        first :: rest ->
            let
                merged =
                    List.foldl
                        (\b acc ->
                            { minX = min b.minX acc.minX
                            , maxX = max b.maxX acc.maxX
                            , minZ = min b.minZ acc.minZ
                            , maxZ = max b.maxZ acc.maxZ
                            , topBodyY = min b.topBodyY acc.topBodyY
                            , maxBottomY = max b.maxBottomY acc.maxBottomY
                            }
                        )
                        first
                        rest

                heightPlates =
                    max 0 (ceiling (-(min 0 merged.topBodyY) / plateHeight - 0.01))
            in
            { minX = merged.minX
            , maxX = merged.maxX
            , minZ = merged.minZ
            , maxZ = merged.maxZ
            , topBodyY = merged.topBodyY
            , maxBottomY = merged.maxBottomY
            , heightPlates = heightPlates
            , partCount = List.length placements
            }


{-| Measure the world-space footprint and vertical extent of a single main
placement (recursively expanding submodels).
-}
placementBounds : Document -> Placement -> Maybe { minX : Float, maxX : Float, minZ : Float, maxZ : Float, topBodyY : Float, maxBottomY : Float, heightPlates : Int }
placementBounds doc placement =
    let
        leafBoxes =
            expandPlacement doc 0 Mat4.identity placement
                |> List.filterMap placementLeafBounds
    in
    case leafBoxes of
        [] ->
            Nothing

        first :: rest ->
            let
                merged =
                    List.foldl
                        (\b acc ->
                            { minX = min b.minX acc.minX
                            , maxX = max b.maxX acc.maxX
                            , minZ = min b.minZ acc.minZ
                            , maxZ = max b.maxZ acc.maxZ
                            , topBodyY = min b.topBodyY acc.topBodyY
                            , maxBottomY = max b.maxBottomY acc.maxBottomY
                            }
                        )
                        first
                        rest
            in
            Just
                { minX = merged.minX
                , maxX = merged.maxX
                , minZ = merged.minZ
                , maxZ = merged.maxZ
                , topBodyY = merged.topBodyY
                , maxBottomY = merged.maxBottomY
                , heightPlates = max 0 (ceiling (-(min 0 merged.topBodyY) / plateHeight - 0.01))
                }


{-| Validate that a `Document` fits strictly inside the 8x8 stud footprint
(`-80..80` LDU in X and Z), stays within max height `32` plates (`256` LDU),
does not sink below the baseplate (`Y <= 8`), is non-empty, and has at most
`maxElementParts` parts.
-}
validateDocument : Document -> Result (List BoundsIssue) { heightPlates : Int, partCount : Int }
validateDocument doc =
    let
        measured =
            documentBounds doc

        issues =
            List.concat
                [ if measured.partCount == 0 then
                    [ EmptyElement ]

                  else
                    []
                , if measured.partCount > maxElementParts then
                    [ TooManyParts measured.partCount ]

                  else
                    []
                , if measured.partCount > 0 && (measured.minX < -lotHalfLdu - boundsEpsilon || measured.maxX > lotHalfLdu + boundsEpsilon) then
                    [ OutOfBoundsX measured.minX measured.maxX ]

                  else
                    []
                , if measured.partCount > 0 && (measured.minZ < -lotHalfLdu - boundsEpsilon || measured.maxZ > lotHalfLdu + boundsEpsilon) then
                    [ OutOfBoundsZ measured.minZ measured.maxZ ]

                  else
                    []
                , if measured.partCount > 0 && (measured.topBodyY < -maxHeightLdu - boundsEpsilon || measured.heightPlates > maxHeightPlates) then
                    [ ExceedsMaxHeight measured.heightPlates ]

                  else
                    []
                , if measured.partCount > 0 && measured.maxBottomY > baseplateBottomY + boundsEpsilon then
                    [ BelowBaseplate measured.maxBottomY ]

                  else
                    []
                ]
    in
    if List.isEmpty issues then
        Ok { heightPlates = measured.heightPlates, partCount = measured.partCount }

    else
        Err issues


{-| Convenience check: `True` when `validateDocument` produces `Ok`.
-}
isDocumentValid : Document -> Bool
isDocumentValid doc =
    case validateDocument doc of
        Ok _ ->
            True

        Err _ ->
            False


{-| IDs of main-model placements that violate the 8x8 footprint or 32-plate
height limit.
-}
violatingPlacementIds : Document -> Set Int
violatingPlacementIds doc =
    Document.mainPlacements doc
        |> List.filter
            (\p ->
                case placementBounds doc p of
                    Just b ->
                        b.minX
                            < -lotHalfLdu
                            - boundsEpsilon
                            || b.maxX
                            > lotHalfLdu
                            + boundsEpsilon
                            || b.minZ
                            < -lotHalfLdu
                            - boundsEpsilon
                            || b.maxZ
                            > lotHalfLdu
                            + boundsEpsilon
                            || b.topBodyY
                            < -maxHeightLdu
                            - boundsEpsilon
                            || b.heightPlates
                            > maxHeightPlates
                            || b.maxBottomY
                            > baseplateBottomY
                            + boundsEpsilon

                    Nothing ->
                        False
            )
        |> List.map .id
        |> Set.fromList


expandPlacement : Document -> Int -> Mat4 -> Placement -> List Placement
expandPlacement doc depth parent p =
    let
        matrix =
            Mat4.mul parent p.matrix
    in
    if depth < 8 && Document.isSubmodelReference p doc then
        Document.placementsOf p.part doc
            |> List.concatMap (expandPlacement doc (depth + 1) matrix)

    else
        [ { p | matrix = matrix } ]


type alias LeafBounds =
    { minX : Float
    , maxX : Float
    , minZ : Float
    , maxZ : Float
    , topBodyY : Float
    , maxBottomY : Float
    }


placementLeafBounds : Placement -> Maybe LeafBounds
placementLeafBounds p =
    let
        connMaybe =
            Connectivity.knownCatalogPart p.part

        localBoxes =
            case connMaybe of
                Just conn ->
                    conn.boundingBoxes

                Nothing ->
                    [ { min = vec3 -10 -4 -10, max = vec3 10 24 10 } ]

        hasTopStuds =
            case connMaybe of
                Just conn ->
                    List.any
                        (\c ->
                            if c.connType == Stud then
                                let
                                    origin =
                                        Mat4.transform p.matrix (vec3 0 0 0)

                                    tip =
                                        Mat4.transform p.matrix c.normal
                                in
                                Vec3.getY (Vec3.sub tip origin) < -0.7

                            else
                                False
                        )
                        conn.connections

                Nothing ->
                    True

        transformBox box =
            let
                corners =
                    [ vec3 (Vec3.getX box.min) (Vec3.getY box.min) (Vec3.getZ box.min)
                    , vec3 (Vec3.getX box.max) (Vec3.getY box.min) (Vec3.getZ box.min)
                    , vec3 (Vec3.getX box.min) (Vec3.getY box.max) (Vec3.getZ box.min)
                    , vec3 (Vec3.getX box.min) (Vec3.getY box.min) (Vec3.getZ box.max)
                    , vec3 (Vec3.getX box.max) (Vec3.getY box.max) (Vec3.getZ box.min)
                    , vec3 (Vec3.getX box.max) (Vec3.getY box.min) (Vec3.getZ box.max)
                    , vec3 (Vec3.getX box.min) (Vec3.getY box.max) (Vec3.getZ box.max)
                    , vec3 (Vec3.getX box.max) (Vec3.getY box.max) (Vec3.getZ box.max)
                    ]
                        |> List.map (Mat4.transform p.matrix >> Vec3.toRecord)
            in
            case corners of
                [] ->
                    Nothing

                first :: rest ->
                    let
                        ext =
                            List.foldl
                                (\pt acc ->
                                    { minX = min pt.x acc.minX
                                    , maxX = max pt.x acc.maxX
                                    , minY = min pt.y acc.minY
                                    , maxY = max pt.y acc.maxY
                                    , minZ = min pt.z acc.minZ
                                    , maxZ = max pt.z acc.maxZ
                                    }
                                )
                                { minX = first.x
                                , maxX = first.x
                                , minY = first.y
                                , maxY = first.y
                                , minZ = first.z
                                , maxZ = first.z
                                }
                                rest

                        studCap =
                            if hasTopStuds then
                                4.0

                            else
                                0.0
                    in
                    Just
                        { minX = ext.minX
                        , maxX = ext.maxX
                        , minZ = ext.minZ
                        , maxZ = ext.maxZ
                        , topBodyY = ext.minY + studCap
                        , maxBottomY = ext.maxY
                        }
    in
    case List.filterMap transformBox localBoxes of
        [] ->
            Nothing

        first :: rest ->
            Just
                (List.foldl
                    (\b acc ->
                        { minX = min b.minX acc.minX
                        , maxX = max b.maxX acc.maxX
                        , minZ = min b.minZ acc.minZ
                        , maxZ = max b.maxZ acc.maxZ
                        , topBodyY = min b.topBodyY acc.topBodyY
                        , maxBottomY = max b.maxBottomY acc.maxBottomY
                        }
                    )
                    first
                    rest
                )
