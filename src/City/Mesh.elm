module City.Mesh exposing
    ( CachedPart
    , LotMesh
    , MeshCache
    , backgroundDetailOptions
    , buildLotMeshWith
    , detailOptions
    , documentProgress
    , empty
    , emptyLotDocument
    , emptyLotKey
    , emptyLotLdraw
    , emptyLotMesh
    , getLotMesh
    , hasLotMesh
    , lotCacheCount
    , partCacheCount
    , partProgress
    , refreshMeshes
    )

{-| WebGL mesh builder, part-level geometry cache, and content-addressed lot
cache for 8x8 microscale city elements (`City.Mesh`).

Performance architecture:

1.  **Zero per-frame parsing**: `refreshMeshes` consumes pre-parsed
    `Document` ASTs stored on `LotElement` (and `emptyLotDocument` for open
    grass lots) rather than re-parsing `.ldr` text on every `PartFetched` or
    build step.
2.  **Part-level local geometry instancing (`parts : Dict String CachedPart`)**:
    Each distinct catalog part (e.g. `3005.dat`, `41539.dat`) is flattened
    once in local space and reused across all lots in the city, eliminating
    redundant recursive subfile walks and primitive normal computations.
3.  **Dual-LOD lot meshes**: Each `LotMesh` uploads both a foreground
    `BoxStuds` surface mesh (`triangleMesh`) + edge mesh (`lineMesh`, with
    zero conditional lines) and a lightweight `NoStuds` surface mesh
    (`lodTriangleMesh`) for distant/background city blocks.
4.  **Detail-aware BFC CCW certification**: Back-face culling
    (`bfcCertified = True`) skips substituted top studs (`stud*`) and
    omitted underside hollows (`stud3*`, `stud4*`) so BFC-certified bricks
    always cull back faces on the GPU.

-}

import City.Types exposing (CityGrid)
import Dict exposing (Dict)
import Game.PartLoading as PartLoading exposing (PartProgress)
import LDraw.Colors as Colors
import LDraw.Document as Document exposing (Document, Placement)
import LDraw.Geometry as Geometry
import LDraw.Resolve as Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4)
import Render.EdgeShader exposing (EdgeVertex)
import Render.Mesh exposing (Vertex)
import Set exposing (Set)
import WebGL


{-| Pre-flattened local-space geometry for one distinct catalog part, shared
across every lot that places it.
-}
type alias CachedPart =
    { triangles : List ( Vertex, Vertex, Vertex )
    , lodTriangles : List ( Vertex, Vertex, Vertex )
    , hasStuds : Bool
    , lines : List ( Vec3, Vec3 )
    , bfcCertified : Bool
    }


{-| Compiled WebGL geometry (foreground + background LOD) for one unique 8x8
element.
-}
type alias LotMesh =
    { sourceText : String
    , triangleMesh : WebGL.Mesh Vertex
    , lodTriangleMesh : WebGL.Mesh Vertex
    , lineMesh : WebGL.Mesh EdgeVertex
    , hasLines : Bool
    , bfcCertified : Bool
    , boundsMin : Vec3
    , boundsMax : Vec3
    , localCenter : Vec3
    , boundingRadius : Float
    , triangleCount : Int
    , lodTriangleCount : Int
    }


{-| Content-addressed lot mesh cache (`lots`), shared catalog part geometry
cache (`parts`), and memoized BFC CCW certification status (`bfcMemo`).
-}
type alias MeshCache =
    { lots : Dict String LotMesh
    , parts : Dict String CachedPart
    , bfcMemo : Dict String Bool
    }


{-| Empty `MeshCache`.
-}
empty : MeshCache
empty =
    { lots = Dict.empty
    , parts = Dict.empty
    , bfcMemo = Dict.empty
    }


{-| Number of compiled lot meshes currently cached.
-}
lotCacheCount : MeshCache -> Int
lotCacheCount cache =
    Dict.size cache.lots


{-| Number of distinct catalog parts pre-flattened in `MeshCache`.
-}
partCacheCount : MeshCache -> Int
partCacheCount cache =
    Dict.size cache.parts


{-| Cache key for the empty 8x8 grass parcel mesh.
-}
emptyLotKey : String
emptyLotKey =
    "__empty_lot__"


{-| LDraw model text for an open unbuilt 8x8 lot (green 8x8 plate `41539.dat`).
-}
emptyLotLdraw : String
emptyLotLdraw =
    "0 FILE empty-lot.ldr\n0 Name: Empty 8x8 Lot\n0 BFC CERTIFY CCW\n1 2 0 0 0 1 0 0 0 1 0 0 0 1 41539.dat"


{-| Pre-parsed `Document` for `emptyLotLdraw` (parsed once at module init).
-}
emptyLotDocument : Document
emptyLotDocument =
    Document.parse emptyLotLdraw


{-| Fast foreground city-scale geometry detail options: Y-up conversion,
`BoxStuds` (10 triangles per stud instead of 100+), no underside hollows,
crisp edges, and no conditional lines.
-}
detailOptions : Geometry.DetailOptions
detailOptions =
    { flipYZ = True
    , trackInherit = False
    , studs = Geometry.BoxStuds
    , includeHollows = False
    , includeLines = True
    , includeConditionals = False
    }


{-| Lightweight background diorama detail options: Y-up conversion, `NoStuds`,
no underside hollows, no edge lines, and no conditional lines.
-}
backgroundDetailOptions : Geometry.DetailOptions
backgroundDetailOptions =
    { flipYZ = True
    , trackInherit = False
    , studs = Geometry.NoStuds
    , includeHollows = False
    , includeLines = False
    , includeConditionals = False
    }


localDetailOptions : Geometry.DetailOptions
localDetailOptions =
    { flipYZ = False
    , trackInherit = True
    , studs = Geometry.BoxStuds
    , includeHollows = False
    , includeLines = True
    , includeConditionals = False
    }


localBackgroundOptions : Geometry.DetailOptions
localBackgroundOptions =
    { flipYZ = False
    , trackInherit = True
    , studs = Geometry.NoStuds
    , includeHollows = False
    , includeLines = False
    , includeConditionals = False
    }


yUpFlip : Mat4
yUpFlip =
    Mat4.makeScale3 1 -1 -1


maxCachedLots : Int
maxCachedLots =
    96


{-| Part files still needed for one raw LDraw text string.
-}
partProgress : String -> PartCache -> PartProgress
partProgress ldraw cache =
    documentProgress (Document.parse ldraw) cache


{-| Part files still needed for an already-parsed `Document`.
-}
documentProgress : Document -> PartCache -> PartProgress
documentProgress doc cache =
    PartLoading.progress (PartLoading.flattenedParts doc) doc cache


{-| Refresh `MeshCache` for every unique element in `city` plus the empty lot
baseplate, returning the updated cache, updated `PartCache` (with missing parts
marked loading), and the list of part filenames to fetch. Only missing or
modified lot hashes are built; unchanged lots and cached catalog parts are
reused without re-parsing or re-flattening.
-}
refreshMeshes :
    CityGrid
    -> PartCache
    -> MeshCache
    -> { meshes : MeshCache, cache : PartCache, fetch : List String }
refreshMeshes city rawCache meshes =
    let
        uniqueEntries : List ( String, String, Document )
        uniqueEntries =
            Dict.foldl
                (\_ el acc -> Dict.insert el.contentHash ( el.ldrawText, el.document ) acc)
                (Dict.singleton emptyLotKey ( emptyLotLdraw, emptyLotDocument ))
                city.lots
                |> Dict.toList
                |> List.map (\( key, ( ldraw, doc ) ) -> ( key, ldraw, doc ))

        unbuiltStates :
            List
                { key : String
                , ldraw : String
                , doc : Document
                , placements : List Placement
                , prog : PartProgress
                }
        unbuiltStates =
            uniqueEntries
                |> List.filter
                    (\( key, ldraw, _ ) ->
                        case Dict.get key meshes.lots of
                            Just existing ->
                                existing.sourceText /= ldraw

                            Nothing ->
                                True
                    )
                |> List.map
                    (\( key, ldraw, doc ) ->
                        let
                            placements =
                                Document.flattenedPlacements doc

                            prog =
                                PartLoading.progress (List.map .part placements) doc rawCache
                        in
                        { key = key
                        , ldraw = ldraw
                        , doc = doc
                        , placements = placements
                        , prog = prog
                        }
                    )

        ( builtLots, nextParts, nextBfcMemo ) =
            List.foldl
                (\item ( accLots, accParts, accMemo ) ->
                    if List.isEmpty item.prog.needed && item.prog.inFlight == 0 then
                        let
                            ( lotMesh, updatedParts, updatedMemo ) =
                                compileLotMesh item.doc item.ldraw item.placements rawCache accParts accMemo
                        in
                        ( Dict.insert item.key lotMesh accLots, updatedParts, updatedMemo )

                    else
                        ( accLots, accParts, accMemo )
                )
                ( meshes.lots, meshes.parts, meshes.bfcMemo )
                unbuiltStates

        prunedLots =
            if Dict.size builtLots > maxCachedLots then
                let
                    activeKeys =
                        Dict.foldl (\_ el acc -> Set.insert el.contentHash acc)
                            (Set.singleton emptyLotKey)
                            city.lots
                in
                Dict.filter (\k _ -> Set.member k activeKeys) builtLots

            else
                builtLots

        needed =
            unbuiltStates
                |> List.concatMap (\item -> item.prog.needed)
                |> PartLoading.uniqueNames
    in
    { meshes =
        { lots = prunedLots
        , parts = nextParts
        , bfcMemo = nextBfcMemo
        }
    , cache = Resolve.markLoading needed rawCache
    , fetch = needed
    }


{-| Look up the compiled mesh for a lot's `contentHash` (or key).
-}
getLotMesh : String -> MeshCache -> Maybe LotMesh
getLotMesh key meshes =
    Dict.get key meshes.lots


{-| Return `True` when the compiled mesh for `key` is already in `MeshCache`.
-}
hasLotMesh : String -> MeshCache -> Bool
hasLotMesh key meshes =
    Dict.member key meshes.lots


{-| Look up the compiled mesh for an empty grass parcel.
-}
emptyLotMesh : MeshCache -> Maybe LotMesh
emptyLotMesh meshes =
    Dict.get emptyLotKey meshes.lots


{-| Build a `LotMesh` directly from a `Document` using explicit `DetailOptions`.
Useful for testing and custom detail presets.
-}
buildLotMeshWith : Geometry.DetailOptions -> Document -> PartCache -> LotMesh
buildLotMeshWith options doc rawCache =
    let
        subfiles =
            Document.subfiles doc

        cache =
            List.foldl (\( name, lines ) acc -> Dict.insert name (Loaded lines) acc)
                rawCache
                subfiles

        placements =
            Document.flattenedPlacements doc

        toRef p =
            SubFileRef { color = p.color, transform = p.matrix, file = p.part }

        geom =
            Geometry.flattenWithDetail options (List.map toRef placements) cache 16 Mat4.identity

        noStudGeom =
            if options.studs == Geometry.NoStuds then
                geom

            else
                Geometry.flattenWithDetail
                    { options | studs = Geometry.NoStuds, includeLines = False }
                    (List.map toRef placements)
                    cache
                    16
                    Mat4.identity

        ( partsBfc, _ ) =
            arePartsBfcCertifiedFor options cache Dict.empty (List.map .part placements)

        bfcCertified =
            docIsBfcSafe doc && partsBfc

        triCount =
            List.length geom.triangles

        lodTriCount =
            List.length noStudGeom.triangles

        triMesh =
            WebGL.triangles geom.triangles

        lodMesh =
            if lodTriCount == triCount then
                triMesh

            else
                WebGL.triangles noStudGeom.triangles

        lineMesh =
            if options.includeLines then
                buildLineMesh geom.lines

            else
                WebGL.triangles []
    in
    assembleLotMesh "" triMesh lodMesh lineMesh (options.includeLines && not (List.isEmpty geom.lines)) bfcCertified geom.triangles triCount lodTriCount


compileLotMesh :
    Document
    -> String
    -> List Placement
    -> PartCache
    -> Dict String CachedPart
    -> Dict String Bool
    -> ( LotMesh, Dict String CachedPart, Dict String Bool )
compileLotMesh doc sourceText placements rawCache partsCache bfcMemo =
    let
        subfiles =
            Document.subfiles doc

        canUsePartCache =
            List.isEmpty subfiles && not (docHasRawGeometry doc)
    in
    if canUsePartCache then
        let
            ( updatedParts, updatedMemo ) =
                ensureCachedParts placements rawCache partsCache bfcMemo

            stamped =
                stampAllPlacements placements updatedParts

            triCount =
                List.length stamped.fullTris

            lodTriCount =
                if stamped.anyHasStuds then
                    List.length stamped.lodTris

                else
                    triCount

            triMesh =
                WebGL.triangles stamped.fullTris

            lodMesh =
                if stamped.anyHasStuds then
                    WebGL.triangles stamped.lodTris

                else
                    triMesh

            hasLines =
                not (List.isEmpty stamped.lines)

            lineMesh =
                if hasLines then
                    buildLineMesh stamped.lines

                else
                    WebGL.triangles []

            bfcCertified =
                docIsBfcSafe doc && stamped.allBfc
        in
        ( assembleLotMesh sourceText triMesh lodMesh lineMesh hasLines bfcCertified stamped.fullTris triCount lodTriCount
        , updatedParts
        , updatedMemo
        )

    else
        let
            cache =
                List.foldl (\( name, lines ) acc -> Dict.insert name (Loaded lines) acc)
                    rawCache
                    subfiles

            toRef p =
                SubFileRef { color = p.color, transform = p.matrix, file = p.part }

            refs =
                List.map toRef placements

            geom =
                Geometry.flattenWithDetail detailOptions refs cache 16 Mat4.identity

            lodGeom =
                Geometry.flattenWithDetail backgroundDetailOptions refs cache 16 Mat4.identity

            ( partsBfc, updatedMemo ) =
                arePartsBfcCertifiedFor detailOptions cache bfcMemo (List.map .part placements)

            bfcCertified =
                docIsBfcSafe doc && partsBfc

            triCount =
                List.length geom.triangles

            lodTriCount =
                List.length lodGeom.triangles

            triMesh =
                WebGL.triangles geom.triangles

            lodMesh =
                if lodTriCount == triCount then
                    triMesh

                else
                    WebGL.triangles lodGeom.triangles

            hasLines =
                not (List.isEmpty geom.lines)

            lineMesh =
                if hasLines then
                    buildLineMesh geom.lines

                else
                    WebGL.triangles []
        in
        ( assembleLotMesh sourceText triMesh lodMesh lineMesh hasLines bfcCertified geom.triangles triCount lodTriCount
        , partsCache
        , updatedMemo
        )


assembleLotMesh :
    String
    -> WebGL.Mesh Vertex
    -> WebGL.Mesh Vertex
    -> WebGL.Mesh EdgeVertex
    -> Bool
    -> Bool
    -> List ( Vertex, Vertex, Vertex )
    -> Int
    -> Int
    -> LotMesh
assembleLotMesh sourceText triMesh lodMesh lineMesh hasLines bfcCertified tris triCount lodTriCount =
    let
        ( boundsMin, boundsMax ) =
            boundsOf tris

        localCenter =
            Vec3.scale 0.5 (Vec3.add boundsMin boundsMax)

        boundingRadius =
            max 1.0 (0.5 * Vec3.length (Vec3.sub boundsMax boundsMin))
    in
    { sourceText = sourceText
    , triangleMesh = triMesh
    , lodTriangleMesh = lodMesh
    , lineMesh = lineMesh
    , hasLines = hasLines
    , bfcCertified = bfcCertified
    , boundsMin = boundsMin
    , boundsMax = boundsMax
    , localCenter = localCenter
    , boundingRadius = boundingRadius
    , triangleCount = triCount
    , lodTriangleCount = lodTriCount
    }


ensureCachedParts :
    List Placement
    -> PartCache
    -> Dict String CachedPart
    -> Dict String Bool
    -> ( Dict String CachedPart, Dict String Bool )
ensureCachedParts placements rawCache partsCache bfcMemo =
    List.foldl
        (\p ( accParts, accMemo ) ->
            if Dict.member p.part accParts then
                ( accParts, accMemo )

            else
                case Dict.get p.part rawCache of
                    Just (Loaded partLines) ->
                        let
                            ( bfcOk, nextMemo ) =
                                subfileBfcFor localDetailOptions rawCache Set.empty p.part accMemo

                            cached =
                                buildCachedPart partLines rawCache bfcOk
                        in
                        ( Dict.insert p.part cached accParts, nextMemo )

                    _ ->
                        ( accParts, accMemo )
        )
        ( partsCache, bfcMemo )
        placements


buildCachedPart : List LDrawLine -> PartCache -> Bool -> CachedPart
buildCachedPart partLines rawCache bfcOk =
    let
        fullGeom =
            Geometry.flattenWithDetail localDetailOptions partLines rawCache 16 Mat4.identity

        bodyGeom =
            Geometry.flattenWithDetail localBackgroundOptions partLines rawCache 16 Mat4.identity

        hasStuds =
            List.length fullGeom.triangles /= List.length bodyGeom.triangles
    in
    { triangles = fullGeom.triangles
    , lodTriangles =
        if hasStuds then
            bodyGeom.triangles

        else
            fullGeom.triangles
    , hasStuds = hasStuds
    , lines = fullGeom.lines
    , bfcCertified = bfcOk
    }


type alias StampedPlacements =
    { fullTris : List ( Vertex, Vertex, Vertex )
    , lodTris : List ( Vertex, Vertex, Vertex )
    , lines : List ( Vec3, Vec3 )
    , anyHasStuds : Bool
    , allBfc : Bool
    }


stampAllPlacements :
    List Placement
    -> Dict String CachedPart
    -> StampedPlacements
stampAllPlacements placements partsCache =
    let
        anyStuds =
            List.any
                (\p ->
                    Dict.get p.part partsCache
                        |> Maybe.map .hasStuds
                        |> Maybe.withDefault False
                )
                placements

        step p acc =
            case Dict.get p.part partsCache of
                Nothing ->
                    { acc | allBfc = False }

                Just cached ->
                    let
                        worldMat =
                            Mat4.mul yUpFlip p.matrix

                        flipped =
                            determinant3 p.matrix < 0

                        resolvedColor =
                            if p.color == 16 || p.color == -1 then
                                Colors.toVec4 (Colors.resolveColor 15 p.color)

                            else
                                Colors.toVec4 (Colors.resolveColor 16 p.color)

                        nextFull =
                            stampTriangles worldMat flipped resolvedColor cached.triangles acc.fullTris

                        nextLod =
                            if anyStuds then
                                stampTriangles worldMat flipped resolvedColor cached.lodTriangles acc.lodTris

                            else
                                acc.lodTris

                        nextLines =
                            stampLines worldMat cached.lines acc.lines
                    in
                    { fullTris = nextFull
                    , lodTris = nextLod
                    , lines = nextLines
                    , anyHasStuds = anyStuds
                    , allBfc = acc.allBfc && cached.bfcCertified
                    }
    in
    List.foldl step
        { fullTris = []
        , lodTris = []
        , lines = []
        , anyHasStuds = anyStuds
        , allBfc = True
        }
        placements


stampTriangles :
    Mat4
    -> Bool
    -> Vec4
    -> List ( Vertex, Vertex, Vertex )
    -> List ( Vertex, Vertex, Vertex )
    -> List ( Vertex, Vertex, Vertex )
stampTriangles worldMat flipped resolvedColor tris acc =
    List.foldl
        (\( v1, v2, v3 ) out ->
            let
                p1 =
                    Mat4.transform worldMat v1.position

                p2 =
                    Mat4.transform worldMat v2.position

                p3 =
                    Mat4.transform worldMat v3.position

                ( ( a, va ), ( b, vb ), ( c, vc ) ) =
                    if flipped then
                        ( ( p1, v1 ), ( p3, v3 ), ( p2, v2 ) )

                    else
                        ( ( p1, v1 ), ( p2, v2 ), ( p3, v3 ) )

                normal =
                    faceNormal a b c

                mk pos v =
                    { position = pos
                    , normal = normal
                    , color =
                        if v.inherit > 0.5 then
                            resolvedColor

                        else
                            v.color
                    , inherit = 0.0
                    }
            in
            ( mk a va, mk b vb, mk c vc ) :: out
        )
        acc
        tris


stampLines : Mat4 -> List ( Vec3, Vec3 ) -> List ( Vec3, Vec3 ) -> List ( Vec3, Vec3 )
stampLines worldMat lines acc =
    List.foldl
        (\( p1, p2 ) out ->
            ( Mat4.transform worldMat p1, Mat4.transform worldMat p2 ) :: out
        )
        acc
        lines


buildLineMesh : List ( Vec3, Vec3 ) -> WebGL.Mesh EdgeVertex
buildLineMesh lines =
    let
        v side pos other =
            { position = pos, other = other, side = side }

        step ( p1, p2 ) acc =
            ( v -1 p1 p2, v 1 p1 p2, v 1 p2 p1 )
                :: ( v -1 p1 p2, v 1 p2 p1, v -1 p2 p1 )
                :: acc
    in
    WebGL.triangles (List.foldl step [] lines)


docHasRawGeometry : Document -> Bool
docHasRawGeometry doc =
    let
        entryIsRaw entry =
            case entry of
                Document.PlacementEntry _ ->
                    False

                Document.Raw text ->
                    let
                        t =
                            String.trim text
                    in
                    String.startsWith "2 " t
                        || String.startsWith "3 " t
                        || String.startsWith "4 " t
                        || String.startsWith "5 " t
    in
    List.any (\sub -> List.any entryIsRaw sub.entries) doc.submodels


docIsBfcSafe : Document -> Bool
docIsBfcSafe doc =
    let
        submodelSafe sub =
            let
                hasCcw =
                    List.any
                        (\e ->
                            case e of
                                Document.Raw text ->
                                    String.contains "BFC CERTIFY CCW" text

                                Document.PlacementEntry _ ->
                                    False
                        )
                        sub.entries

                entrySafe entry =
                    case entry of
                        Document.PlacementEntry _ ->
                            True

                        Document.Raw text ->
                            let
                                t =
                                    String.trim text
                            in
                            not
                                (String.contains "BFC CERTIFY CW" t
                                    || String.contains "BFC NOCERTIFY" t
                                    || String.contains "BFC NOCLIP" t
                                    || String.contains "BFC INVERTNEXT" t
                                    || (not hasCcw && (String.startsWith "3 " t || String.startsWith "4 " t))
                                )
            in
            List.all entrySafe sub.entries
    in
    List.all submodelSafe doc.submodels


arePartsBfcCertifiedFor :
    Geometry.DetailOptions
    -> PartCache
    -> Dict String Bool
    -> List String
    -> ( Bool, Dict String Bool )
arePartsBfcCertifiedFor options cache memo partNames =
    List.foldl
        (\name ( ok, accMemo ) ->
            if not ok then
                ( False, accMemo )

            else
                subfileBfcFor options cache Set.empty name accMemo
        )
        ( True, memo )
        partNames


subfileBfcFor :
    Geometry.DetailOptions
    -> PartCache
    -> Set String
    -> String
    -> Dict String Bool
    -> ( Bool, Dict String Bool )
subfileBfcFor options cache visiting name memo =
    let
        base =
            baseFileName name
    in
    if not options.includeHollows && isHollowSubfile base then
        ( True, memo )

    else if options.studs /= Geometry.FullStuds && isTopStudSubfile base then
        ( True, memo )

    else if Set.member name visiting then
        ( True, memo )

    else
        case Dict.get name memo of
            Just cached ->
                ( cached, memo )

            Nothing ->
                case Dict.get name cache of
                    Just (Loaded subLines) ->
                        if fileBfcCertified subLines then
                            let
                                ( childrenOk, nextMemo ) =
                                    linesSubfilesBfc options cache (Set.insert name visiting) subLines memo
                            in
                            ( childrenOk, Dict.insert name childrenOk nextMemo )

                        else
                            ( False, Dict.insert name False memo )

                    _ ->
                        ( False, Dict.insert name False memo )


linesSubfilesBfc :
    Geometry.DetailOptions
    -> PartCache
    -> Set String
    -> List LDrawLine
    -> Dict String Bool
    -> ( Bool, Dict String Bool )
linesSubfilesBfc options cache visiting lines memo =
    List.foldl
        (\line ( ok, accMemo ) ->
            if not ok then
                ( False, accMemo )

            else
                case line of
                    SubFileRef ref ->
                        subfileBfcFor options cache visiting ref.file accMemo

                    _ ->
                        ( True, accMemo )
        )
        ( True, memo )
        lines


fileBfcCertified : List LDrawLine -> Bool
fileBfcCertified lines =
    let
        step line ( hasCcw, clean ) =
            case line of
                Comment text ->
                    if
                        String.contains "BFC CERTIFY CW" text
                            || String.contains "BFC NOCERTIFY" text
                            || String.contains "BFC NOCLIP" text
                    then
                        ( hasCcw, False )

                    else if String.contains "BFC CERTIFY CCW" text then
                        ( True, clean )

                    else
                        ( hasCcw, clean )

                _ ->
                    ( hasCcw, clean )

        ( ccw, ok ) =
            List.foldl step ( False, True ) lines
    in
    ccw && ok


baseFileName : String -> String
baseFileName raw =
    let
        norm =
            raw
                |> String.toLower
                |> String.replace "\\" "/"
    in
    case List.reverse (String.split "/" norm) of
        last :: _ ->
            last

        [] ->
            norm


isHollowSubfile : String -> Bool
isHollowSubfile base =
    String.startsWith "stud3" base || String.startsWith "stud4" base


isTopStudSubfile : String -> Bool
isTopStudSubfile base =
    String.startsWith "stud" base
        && not (isHollowSubfile base)
        && not (String.startsWith "studel" base)
        && not (String.startsWith "studtube" base)
        && not (String.startsWith "studholder" base)


determinant3 : Mat4 -> Float
determinant3 mat =
    let
        m =
            Mat4.toRecord mat
    in
    (m.m11 * ((m.m22 * m.m33) - (m.m23 * m.m32)))
        - (m.m12 * ((m.m21 * m.m33) - (m.m23 * m.m31)))
        + (m.m13 * ((m.m21 * m.m32) - (m.m22 * m.m31)))


faceNormal : Vec3 -> Vec3 -> Vec3 -> Vec3
faceNormal p1 p2 p3 =
    let
        edge1 =
            Vec3.sub p2 p1

        edge2 =
            Vec3.sub p3 p1

        cross =
            Vec3.cross edge1 edge2

        len =
            Vec3.length cross
    in
    if len < 1.0e-8 then
        vec3 0 1 0

    else
        Vec3.scale (1.0 / len) cross


boundsOf : List ( Vertex, Vertex, Vertex ) -> ( Vec3, Vec3 )
boundsOf tris =
    let
        grow v acc =
            let
                p =
                    Vec3.toRecord v.position
            in
            { minX = min acc.minX p.x
            , minY = min acc.minY p.y
            , minZ = min acc.minZ p.z
            , maxX = max acc.maxX p.x
            , maxY = max acc.maxY p.y
            , maxZ = max acc.maxZ p.z
            }

        b =
            List.foldl (\( v1, v2, v3 ) acc -> acc |> grow v1 |> grow v2 |> grow v3)
                { minX = 1.0e9, minY = 1.0e9, minZ = 1.0e9, maxX = -1.0e9, maxY = -1.0e9, maxZ = -1.0e9 }
                tris
    in
    if b.minX > b.maxX then
        ( vec3 0 0 0, vec3 0 0 0 )

    else
        ( vec3 b.minX b.minY b.minZ, vec3 b.maxX b.maxY b.maxZ )
