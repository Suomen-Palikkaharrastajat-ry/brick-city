module City.Mesh exposing
    ( LotMesh
    , MeshCache
    , detailOptions
    , emptyLotKey
    , emptyLotLdraw
    , emptyLotMesh
    , getLotMesh
    , partProgress
    , refreshMeshes
    )

{-| WebGL mesh builder and content-addressed cache for 8x8 microscale city
elements (`City.Mesh`).

Every unique `ldrawText` across the city is flattened once through
`LDraw.Geometry.flattenWithDetail` (with `BoxStuds` for crisp, high-FPS
city-scale rendering) and uploaded to the GPU once via `Render.Scene.buildScene`.
Rotating or stamping multiple copies of a road or building reuses the exact
same GPU mesh with zero extra geometry flattening.

-}

import City.Types exposing (CityGrid)
import Dict exposing (Dict)
import Game.BrickProps as BrickProps
import Game.PartLoading as PartLoading exposing (PartProgress)
import LDraw.Document as Document exposing (Document)
import LDraw.Geometry as Geometry
import LDraw.Parser as Parser
import LDraw.Resolve as Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render.Mesh exposing (Vertex)
import Render.Scene as Scene exposing (Scene)


{-| Compiled WebGL geometry for one unique 8x8 element.
-}
type alias LotMesh =
    { scene : Scene
    , boundsMin : Vec3
    , boundsMax : Vec3
    , triangleCount : Int
    }


{-| Content-addressed mesh cache keyed by `ldrawText` (and `emptyLotKey` for
open grass parcels).
-}
type alias MeshCache =
    Dict String LotMesh


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


{-| Fast city-scale geometry detail options: Y-up conversion, `BoxStuds`
(10 triangles per stud instead of 100+), no underside hollows, crisp edges.
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


{-| Part files still needed for one LDraw text.
-}
partProgress : String -> PartCache -> PartProgress
partProgress ldraw cache =
    let
        doc =
            Document.parse ldraw
    in
    PartLoading.progress (PartLoading.flattenedParts doc) doc cache


{-| Refresh `MeshCache` for every unique element in `city` plus the empty lot
baseplate, returning the updated cache, updated `PartCache` (with missing parts
marked loading), and the list of part filenames to fetch.
-}
refreshMeshes :
    CityGrid
    -> PartCache
    -> MeshCache
    -> { meshes : MeshCache, cache : PartCache, fetch : List String }
refreshMeshes city rawCache meshes =
    let
        sources =
            ( emptyLotKey, emptyLotLdraw )
                :: (Dict.values city.lots
                        |> List.map (\el -> ( el.ldrawText, el.ldrawText ))
                   )

        uniqueSources =
            Dict.fromList sources |> Dict.toList

        states =
            uniqueSources
                |> List.filter (\( key, _ ) -> not (Dict.member key meshes))
                |> List.map (\( key, ldraw ) -> ( key, ldraw, partProgress ldraw rawCache ))

        built =
            List.foldl
                (\( key, ldraw, prog ) acc ->
                    if List.isEmpty prog.needed && prog.inFlight == 0 then
                        Dict.insert key (buildLotMesh ldraw rawCache) acc

                    else
                        acc
                )
                meshes
                states

        needed =
            states
                |> List.concatMap (\( _, _, prog ) -> prog.needed)
                |> PartLoading.uniqueNames
    in
    { meshes = built
    , cache = Resolve.markLoading needed rawCache
    , fetch = needed
    }


{-| Look up the compiled mesh for a `ldrawText` key.
-}
getLotMesh : String -> MeshCache -> Maybe LotMesh
getLotMesh key meshes =
    Dict.get key meshes


{-| Look up the compiled mesh for an empty grass parcel.
-}
emptyLotMesh : MeshCache -> Maybe LotMesh
emptyLotMesh meshes =
    Dict.get emptyLotKey meshes


buildLotMesh : String -> PartCache -> LotMesh
buildLotMesh ldraw rawCache =
    let
        doc =
            Document.parse ldraw

        cache =
            List.foldl (\( name, lines ) acc -> Dict.insert name (Loaded lines) acc)
                rawCache
                (Document.subfiles doc)

        placements =
            Document.flattenedPlacements doc

        toRef p =
            SubFileRef { color = p.color, transform = p.matrix, file = p.part }

        geom =
            Geometry.flattenWithDetail detailOptions (List.map toRef placements) cache 16 Mat4.identity

        bfcCertified =
            docIsBfcSafe doc && BrickProps.arePartsBfcCertified cache (List.map .part placements)

        ( boundsMin, boundsMax ) =
            boundsOf geom.triangles
    in
    { scene =
        Scene.buildScene
            { triangles = geom.triangles
            , lines = geom.lines
            , conditionalLines = []
            }
            bfcCertified
    , boundsMin = boundsMin
    , boundsMax = boundsMax
    , triangleCount = List.length geom.triangles
    }


docIsBfcSafe : Document -> Bool
docIsBfcSafe doc =
    let
        entrySafe entry =
            case entry of
                Document.PlacementEntry _ ->
                    True

                Document.Raw text ->
                    not
                        (String.contains "BFC CERTIFY CW" text
                            || String.contains "BFC NOCERTIFY" text
                            || String.contains "BFC NOCLIP" text
                            || String.contains "BFC INVERTNEXT" text
                        )
    in
    List.all (\sub -> List.all entrySafe sub.entries) doc.submodels


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
