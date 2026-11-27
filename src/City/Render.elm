module City.Render exposing
    ( cityGroundEntity
    , isTopDownCamera
    , lotDetailFor
    , lotHighlightEntity
    , lotModelMatrix
    , pickLotAt
    , renderCityCanvas
    , renderCityPreview
    , resetCameraForCity
    , rotateLotCenter
    , topDownCameraForCity
    , visibleLotCoords
    )

{-| Full 3D interactive WebGL City Diorama renderer (`City.Render`):
renders the LEGO sky dome (`Game.BrickLook`), the dark architectural city
table plinth, every visible occupied and open 8x8 lot (`City.Mesh`), the yellow
selection frame, and remote collaborator focus rings (`PeerPresence`).

Performance optimizations:

  - **Zero per-frame VBO uploads**: the ground plinth, fallback slabs, and
    hover/selection/peer highlight rings use pre-compiled static `WebGL.Mesh`
    CAFs driven by uniform transforms and `u_color`.
  - **Single-pass frame precomputation**: camera matrices, view-projection,
    Gribb–Hartmann frustum planes (`Render.Instanced.frustumFromMatrix`), and
    clamped `Render.Style.Style` are computed once per frame outside the lot
    loop.
  - **Frustum & distance LOD culling**: off-screen lots are skipped, distant
    background lots switch to stud-free `lodTriangleMesh`, and expensive
    alpha-blended `EdgeShader` passes (plus unused `conditionalEntity` passes)
    are omitted on distant lots and unhighlighted empty grass parcels.
  - **Pass-batched draw order + BFC**: all opaque surface entities render
    together with `WebGL.Settings.cullFace WebGL.Settings.back` before all
    foreground edge entities, minimizing WebGL shader program and blend/depth
    state switches.

-}

import City.Bounds as Bounds
import City.Mesh as Mesh exposing (LotMesh, MeshCache)
import City.Types exposing (CityGrid, LotCoord, PeerPresence)
import Dict
import Game.BrickLook as BrickLook
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Render.Camera as Camera exposing (Camera, Navigation(..))
import Render.EdgeShader as EdgeShader
import Render.Instanced as Instanced exposing (Frustum)
import Render.Pick as Pick
import Render.Shader as Shader
import Render.Style as Style
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Settings
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest


type alias FlatVertex =
    { position : Vec3 }


type alias FlatUniforms =
    { viewMatrix : Mat4
    , projectionMatrix : Mat4
    , modelMatrix : Mat4
    , u_color : Vec4
    }


type alias SurfaceVertex =
    { position : Vec3
    , normal : Vec3
    , color : Vec3
    }


type alias CitySurfaceUniforms =
    { modelMatrix : Mat4
    , viewMatrix : Mat4
    , projectionMatrix : Mat4
    , u_eyePos : Vec3
    , u_sunDir : Vec3
    , u_fogColor : Vec3
    , u_fogNear : Float
    , u_fogFar : Float
    , u_fogMax : Float
    , u_skyAmbient : Vec3
    , u_groundAmbient : Vec3
    , u_studPitch : Float
    , u_plateStuds : Float
    , u_gloss : Float
    }


flatVertexShader : Shader FlatVertex FlatUniforms {}
flatVertexShader =
    [glsl|
        attribute vec3 position;

        uniform mat4 viewMatrix;
        uniform mat4 projectionMatrix;
        uniform mat4 modelMatrix;

        void main() {
            gl_Position = projectionMatrix * viewMatrix * (modelMatrix * vec4(position, 1.0));
        }
    |]


flatFragmentShader : Shader {} FlatUniforms {}
flatFragmentShader =
    [glsl|
        precision mediump float;

        uniform vec4 u_color;

        void main() {
            gl_FragColor = u_color;
        }
    |]


surfaceVertexShader : Shader SurfaceVertex CitySurfaceUniforms BrickLook.SurfaceVaryings
surfaceVertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec3 color;

        uniform mat4 modelMatrix;
        uniform mat4 viewMatrix;
        uniform mat4 projectionMatrix;

        varying vec3 v_worldPos;
        varying vec3 v_normal;
        varying vec3 v_color;
        varying float v_studs;

        void main() {
            vec4 worldPos = modelMatrix * vec4(position, 1.0);
            v_worldPos = worldPos.xyz;
            v_normal = normal;
            v_color = color;
            v_studs = 1.0;
            gl_Position = projectionMatrix * viewMatrix * worldPos;
        }
    |]


cityStyle : Style.Style
cityStyle =
    Style.clampStyle Style.defaultStyle


citySunDir : Vec3
citySunDir =
    Vec3.normalize (vec3 0.55 0.8 0.45)


fovHalfTan : Float
fovHalfTan =
    tan (degrees Camera.fovYDegrees / 2.0)


bfcCullSettings : List WebGL.Settings.Setting
bfcCullSettings =
    [ DepthTest.default, WebGL.Settings.cullFace WebGL.Settings.back ]


depthOnlySettings : List WebGL.Settings.Setting
depthOnlySettings =
    [ DepthTest.default ]


edgePassSettings : List WebGL.Settings.Setting
edgePassSettings =
    [ DepthTest.lessOrEqual { write = False, near = 0, far = 1 }
    , Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
    ]


highlightSettings : List WebGL.Settings.Setting
highlightSettings =
    [ DepthTest.lessOrEqual { write = False, near = 0, far = 1 }
    , Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
    , WebGL.Settings.cullFace WebGL.Settings.back
    ]


groundMesh : Mesh SurfaceVertex
groundMesh =
    let
        col =
            vec3 0.18 0.22 0.28

        n =
            vec3 0 1 0

        v1 =
            { position = vec3 -1.0 0.0 -1.0, normal = n, color = col }

        v2 =
            { position = vec3 1.0 0.0 -1.0, normal = n, color = col }

        v3 =
            { position = vec3 1.0 0.0 1.0, normal = n, color = col }

        v4 =
            { position = vec3 -1.0 0.0 1.0, normal = n, color = col }
    in
    WebGL.triangles [ ( v1, v3, v2 ), ( v1, v4, v3 ) ]


fallbackSlabMesh : Mesh FlatVertex
fallbackSlabMesh =
    let
        h =
            Bounds.lotHalfLdu - 2.0

        y =
            0.2

        v1 =
            { position = vec3 -h y -h }

        v2 =
            { position = vec3 h y -h }

        v3 =
            { position = vec3 h y h }

        v4 =
            { position = vec3 -h y h }
    in
    WebGL.triangles [ ( v1, v3, v2 ), ( v1, v4, v3 ) ]


buildHighlightRingMesh : Float -> Mesh FlatVertex
buildHighlightRingMesh borderWidth =
    let
        outer =
            Bounds.lotHalfLdu + 1.5

        inner =
            Bounds.lotHalfLdu - borderWidth

        y =
            2.4

        quad x1 x2 z1 z2 =
            [ ( { position = vec3 x1 y z1 }
              , { position = vec3 x2 y z2 }
              , { position = vec3 x2 y z1 }
              )
            , ( { position = vec3 x1 y z1 }
              , { position = vec3 x1 y z2 }
              , { position = vec3 x2 y z2 }
              )
            ]
    in
    WebGL.triangles
        (List.concat
            [ quad -outer outer -outer -inner
            , quad -outer outer inner outer
            , quad -outer -inner -inner inner
            , quad inner outer -inner inner
            ]
        )


hoverHighlightMesh : Mesh FlatVertex
hoverHighlightMesh =
    buildHighlightRingMesh 4.0


peerHighlightMesh : Mesh FlatVertex
peerHighlightMesh =
    buildHighlightRingMesh 5.0


selectedHighlightMesh : Mesh FlatVertex
selectedHighlightMesh =
    buildHighlightRingMesh 6.0


highlightMeshForWidth : Float -> Mesh FlatVertex
highlightMeshForWidth borderWidth =
    if abs (borderWidth - 4.0) < 0.01 then
        hoverHighlightMesh

    else if abs (borderWidth - 5.0) < 0.01 then
        peerHighlightMesh

    else if abs (borderWidth - 6.0) < 0.01 then
        selectedHighlightMesh

    else
        buildHighlightRingMesh borderWidth


{-| Exact world model matrix for a lot centered at `(cx, 0, cz)` with
quarter-turn Y rotation `modBy 4 rotation` (`0..3`), avoiding per-frame
trigonometry and matrix multiplication.
-}
lotModelMatrix : Float -> Float -> Int -> Mat4
lotModelMatrix cx cz rotation =
    case modBy 4 rotation of
        1 ->
            Mat4.fromRecord
                { m11 = 0
                , m12 = 0
                , m13 = -1
                , m14 = cx
                , m21 = 0
                , m22 = 1
                , m23 = 0
                , m24 = 0
                , m31 = 1
                , m32 = 0
                , m33 = 0
                , m34 = cz
                , m41 = 0
                , m42 = 0
                , m43 = 0
                , m44 = 1
                }

        2 ->
            Mat4.fromRecord
                { m11 = -1
                , m12 = 0
                , m13 = 0
                , m14 = cx
                , m21 = 0
                , m22 = 1
                , m23 = 0
                , m24 = 0
                , m31 = 0
                , m32 = 0
                , m33 = -1
                , m34 = cz
                , m41 = 0
                , m42 = 0
                , m43 = 0
                , m44 = 1
                }

        3 ->
            Mat4.fromRecord
                { m11 = 0
                , m12 = 0
                , m13 = 1
                , m14 = cx
                , m21 = 0
                , m22 = 1
                , m23 = 0
                , m24 = 0
                , m31 = -1
                , m32 = 0
                , m33 = 0
                , m34 = cz
                , m41 = 0
                , m42 = 0
                , m43 = 0
                , m44 = 1
                }

        _ ->
            Mat4.fromRecord
                { m11 = 1
                , m12 = 0
                , m13 = 0
                , m14 = cx
                , m21 = 0
                , m22 = 1
                , m23 = 0
                , m24 = 0
                , m31 = 0
                , m32 = 0
                , m33 = 1
                , m34 = cz
                , m41 = 0
                , m42 = 0
                , m43 = 0
                , m44 = 1
                }


{-| Transform a lot-local bounding center by quarter-turn Y rotation and world
translation `(cx, 0, cz)` without allocating a matrix.
-}
rotateLotCenter : Int -> Float -> Float -> Vec3 -> Vec3
rotateLotCenter rotation cx cz localCenter =
    let
        c =
            Vec3.toRecord localCenter
    in
    case modBy 4 rotation of
        1 ->
            vec3 (cx - c.z) c.y (cz + c.x)

        2 ->
            vec3 (cx - c.x) c.y (cz - c.z)

        3 ->
            vec3 (cx + c.z) c.y (cz - c.x)

        _ ->
            vec3 (cx + c.x) c.y (cz + c.z)


{-| Choose distance LOD (`useLod`: switch to `NoStuds` mesh) and smooth edge
line width (`edgeWidth <= 0.3` skips the `EdgeShader` pass) for a lot from its
projected width in screen pixels.
-}
lotDetailFor : { isEmpty : Bool, isHighlighted : Bool, lotScreenPx : Float } -> { useLod : Bool, edgeWidth : Float }
lotDetailFor info =
    if info.isEmpty then
        let
            useLod =
                not info.isHighlighted && info.lotScreenPx < 130.0

            edgeWidth =
                if info.isHighlighted && info.lotScreenPx > 80.0 then
                    1.2

                else if info.lotScreenPx > 180.0 then
                    clamp 0.0 1.2 ((info.lotScreenPx - 180.0) / 60.0 * 1.2)

                else
                    0.0
        in
        { useLod = useLod, edgeWidth = edgeWidth }

    else
        let
            useLod =
                not info.isHighlighted && info.lotScreenPx < 85.0

            edgeWidth =
                if info.isHighlighted then
                    clamp 0.0 1.4 ((info.lotScreenPx - 45.0) / 45.0 * 1.4)

                else
                    clamp 0.0 1.4 ((info.lotScreenPx - 95.0) / 55.0 * 1.4)
        in
        { useLod = useLod, edgeWidth = edgeWidth }


{-| Return the list of lot coordinates whose bounding spheres intersect the
active camera frustum. Exposed for unit testing and frustum verification.
-}
visibleLotCoords : Camera -> Int -> Int -> CityGrid -> MeshCache -> List LotCoord
visibleLotCoords camera width height city meshes =
    let
        w =
            max 1 width

        h =
            max 1 height

        aspect =
            toFloat w / toFloat h

        viewMat =
            Camera.viewMatrix camera

        projMat =
            Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera)

        frustum =
            Instanced.frustumFromMatrix (Mat4.mul projMat viewMat)

        step r c acc =
            if r >= city.rows then
                List.reverse acc

            else if c >= city.cols then
                step (r + 1) 0 acc

            else
                let
                    coord =
                        LotCoord r c

                    center =
                        Bounds.lotWorldCenter city.cols city.rows coord

                    visible =
                        case Dict.get ( r, c ) city.lots of
                            Just el ->
                                case Mesh.getLotMesh el.contentHash meshes of
                                    Just m ->
                                        Instanced.sphereInFrustum
                                            frustum
                                            (rotateLotCenter el.rotation center.x center.z m.localCenter)
                                            m.boundingRadius

                                    Nothing ->
                                        Instanced.sphereInFrustum frustum (vec3 center.x 4.0 center.z) 115.0

                            Nothing ->
                                case Mesh.emptyLotMesh meshes of
                                    Just m ->
                                        Instanced.sphereInFrustum
                                            frustum
                                            (rotateLotCenter 0 center.x center.z m.localCenter)
                                            m.boundingRadius

                                    Nothing ->
                                        Instanced.sphereInFrustum frustum (vec3 center.x 4.0 center.z) 115.0
                in
                step r
                    (c + 1)
                    (if visible then
                        coord :: acc

                     else
                        acc
                    )
    in
    step 0 0 []


{-| Frame the camera cleanly around a `cols x rows` city grid.
-}
resetCameraForCity : CityGrid -> Int -> Camera
resetCameraForCity city viewportHeight =
    frameCityCamera city viewportHeight 0.68 0.62 520.0 1.42 (vec3 0 24 0)


{-| Frame the camera straight overhead in top-down plan view around a `cols x rows` city grid.
-}
topDownCameraForCity : CityGrid -> Int -> Camera
topDownCameraForCity city viewportHeight =
    frameCityCamera city viewportHeight 0.0 (pi / 2.0 - 0.04) 620.0 1.48 (vec3 0 0 0)


{-| True when the camera is positioned near the overhead top-down plan view.
-}
isTopDownCamera : Camera -> Bool
isTopDownCamera cam =
    cam.navigation == Orbit && cam.elevation >= 1.35


frameCityCamera : CityGrid -> Int -> Float -> Float -> Float -> Float -> Vec3 -> Camera
frameCityCamera city viewportHeight azimuth elevation minDist distFactor target =
    let
        span =
            max city.cols city.rows |> toFloat |> (*) Bounds.lotSizeLdu

        dist =
            max minDist (span * distFactor)
    in
    Camera.init
        |> (\c -> { c | azimuth = azimuth, elevation = elevation, distance = dist, target = target })
        |> Camera.setZoomRange 140.0 (max 2400.0 (span * 4.5))
        |> Camera.setSceneRadius (span * 0.9)
        |> Camera.setViewportHeight (toFloat (max 1 viewportHeight))
        |> Camera.setNavigation Orbit


{-| Pick which `(row, col)` lot in `city` lies under screen pixel `(x, y)`
on a canvas of `(width, height)`. Rejects off-grid rays in one coarse city-bounds
test, intersects occupied lot bounding slabs, and falls back to O(1) `Y = 0`
ground-plane coordinates for empty grass parcels.
-}
pickLotAt : Camera -> Int -> Int -> Float -> Float -> CityGrid -> Maybe LotCoord
pickLotAt camera width height mouseX mouseY city =
    case Pick.screenRay camera width height mouseX mouseY of
        Nothing ->
            Nothing

        Just ( rayOrigin, rayDir ) ->
            let
                halfCityX =
                    toFloat city.cols * Bounds.lotSizeLdu / 2.0 + 4.0

                halfCityZ =
                    toFloat city.rows * Bounds.lotSizeLdu / 2.0 + 4.0

                cityMin =
                    vec3 -halfCityX -10.0 -halfCityZ

                cityMax =
                    vec3 halfCityX (Bounds.maxHeightLdu + 8.0) halfCityZ
            in
            case Pick.rayAabbHit rayOrigin rayDir cityMin cityMax of
                Nothing ->
                    Nothing

                Just _ ->
                    let
                        half =
                            Bounds.lotHalfLdu - 0.5

                        checkOccupied ( r, c ) el best =
                            if r < 0 || c < 0 || r >= city.rows || c >= city.cols then
                                best

                            else
                                let
                                    coord =
                                        LotCoord r c

                                    center =
                                        Bounds.lotWorldCenter city.cols city.rows coord

                                    heightLdu =
                                        max 12.0 (toFloat el.heightPlates * Bounds.plateHeight)

                                    minCorner =
                                        vec3 (center.x - half) -8.0 (center.z - half)

                                    maxCorner =
                                        vec3 (center.x + half) heightLdu (center.z + half)
                                in
                                case Pick.rayAabbHit rayOrigin rayDir minCorner maxCorner of
                                    Just t ->
                                        case best of
                                            Just ( bestT, _ ) ->
                                                if t < bestT then
                                                    Just ( t, coord )

                                                else
                                                    best

                                            Nothing ->
                                                Just ( t, coord )

                                    Nothing ->
                                        best

                        boxHit =
                            Dict.foldl checkOccupied Nothing city.lots
                                |> Maybe.map Tuple.second
                    in
                    case boxHit of
                        Just found ->
                            Just found

                        Nothing ->
                            case Pick.rayGroundHit rayOrigin rayDir of
                                Just ( _, groundPt ) ->
                                    let
                                        p =
                                            Vec3.toRecord groundPt

                                        step =
                                            Bounds.lotSizeLdu + Bounds.lotGapLdu

                                        col =
                                            round (p.x / step + toFloat (city.cols - 1) / 2.0)

                                        row =
                                            round (p.z / step + toFloat (city.rows - 1) / 2.0)
                                    in
                                    if row >= 0 && row < city.rows && col >= 0 && col < city.cols then
                                        Just (LotCoord row col)

                                    else
                                        Nothing

                                Nothing ->
                                    Nothing


buildCityEntities :
    { city : CityGrid
    , meshes : MeshCache
    , camera : Camera
    , selectedLot : Maybe LotCoord
    , hoveredLot : Maybe LotCoord
    , peers : List PeerPresence
    , mySlot : Int
    , time : Float
    , width : Int
    , height : Int
    }
    -> List Entity
buildCityEntities cfg =
    let
        aspect =
            toFloat cfg.width / toFloat cfg.height

        near =
            Camera.nearPlane cfg.camera

        far =
            Camera.farPlane cfg.camera

        viewMat =
            Camera.viewMatrix cfg.camera

        projMat =
            Camera.projectionMatrix aspect near far

        viewProj =
            Mat4.mul projMat viewMat

        frustum =
            Instanced.frustumFromMatrix viewProj

        eye =
            Camera.position cfg.camera

        projScaleY =
            toFloat cfg.height / (2.0 * fovHalfTan)

        wFloat =
            toFloat cfg.width

        hFloat =
            toFloat cfg.height

        sky =
            BrickLook.skyEntity
                { viewProjection = viewProj
                , eye = eye
                , zenith = vec3 0.22 0.49 0.82
                , horizon = vec3 0.68 0.84 0.96
                , sunDir = citySunDir
                , time = cfg.time
                }

        ground =
            cityGroundEntity cfg.city viewMat projMat eye citySunDir

        ( surfaceEntities, edgeEntities ) =
            collectLotPasses
                { city = cfg.city
                , meshes = cfg.meshes
                , selectedLot = cfg.selectedLot
                , hoveredLot = cfg.hoveredLot
                , frustum = frustum
                , viewMat = viewMat
                , projMat = projMat
                , eye = eye
                , projScaleY = projScaleY
                , viewportW = wFloat
                , viewportH = hFloat
                }

        hoverEntities =
            case cfg.hoveredLot of
                Just coord ->
                    if Just coord /= cfg.selectedLot then
                        [ lotHighlightEntity cfg.city coord (vec4 1.0 1.0 1.0 0.55) 4.0 viewMat projMat ]

                    else
                        []

                Nothing ->
                    []

        selectionEntities =
            case cfg.selectedLot of
                Just coord ->
                    [ lotHighlightEntity cfg.city coord (vec4 0.98 0.78 0.04 0.95) 6.0 viewMat projMat ]

                Nothing ->
                    []

        peerEntities =
            cfg.peers
                |> List.filter (\p -> p.slot /= cfg.mySlot)
                |> List.filterMap
                    (\p ->
                        p.focusedLot
                            |> Maybe.map
                                (\coord ->
                                    lotHighlightEntity cfg.city coord (hexToVec4 p.color 0.85) 5.0 viewMat projMat
                                )
                    )
    in
    sky
        :: ground
        :: surfaceEntities
        ++ edgeEntities
        ++ hoverEntities
        ++ selectionEntities
        ++ peerEntities


{-| Render the interactive 3D City Diorama WebGL canvas.
-}
renderCityCanvas :
    { city : CityGrid
    , meshes : MeshCache
    , camera : Camera
    , selectedLot : Maybe LotCoord
    , hoveredLot : Maybe LotCoord
    , peers : List PeerPresence
    , mySlot : Int
    , time : Float
    , width : Int
    , height : Int
    , onPointerDown : Float -> Float -> Int -> Bool -> msg
    , onPointerMove : Float -> Float -> msg
    , onPointerUp : Float -> Float -> msg
    , onDoubleClick : Float -> Float -> msg
    , onWheel : Float -> msg
    }
    -> Html msg
renderCityCanvas cfg =
    let
        w =
            max 1 cfg.width

        h =
            max 1 cfg.height

        entities =
            buildCityEntities
                { city = cfg.city
                , meshes = cfg.meshes
                , camera = cfg.camera
                , selectedLot = cfg.selectedLot
                , hoveredLot = cfg.hoveredLot
                , peers = cfg.peers
                , mySlot = cfg.mySlot
                , time = cfg.time
                , width = w
                , height = h
                }

        pointerDownDecoder =
            Decode.map4 cfg.onPointerDown
                (Decode.field "offsetX" Decode.float)
                (Decode.field "offsetY" Decode.float)
                (Decode.field "button" Decode.int)
                (Decode.field "shiftKey" Decode.bool)

        pointerPosDecoder fn =
            Decode.map2 fn
                (Decode.field "offsetX" Decode.float)
                (Decode.field "offsetY" Decode.float)

        wheelDecoder =
            Decode.field "deltaY" Decode.float
                |> Decode.map (\dy -> ( cfg.onWheel dy, True ))
    in
    WebGL.toHtmlWith
        [ WebGL.alpha False
        , WebGL.depth 1
        , WebGL.antialias
        , WebGL.clearColor 0.06 0.12 0.2 1.0
        ]
        [ Attr.width w
        , Attr.height h
        , Attr.class "block w-full h-full touch-none cursor-grab active:cursor-grabbing"
        , Attr.attribute "data-testid" "city-webgl-canvas"
        , Html.Events.on "pointerdown" pointerDownDecoder
        , Html.Events.on "pointermove" (pointerPosDecoder cfg.onPointerMove)
        , Html.Events.on "pointerup" (pointerPosDecoder cfg.onPointerUp)
        , Html.Events.on "dblclick" (pointerPosDecoder cfg.onDoubleClick)
        , Html.Events.preventDefaultOn "contextmenu" (Decode.succeed ( cfg.onPointerUp 0 0, True ))
        , Html.Events.preventDefaultOn "wheel" wheelDecoder
        ]
        entities


{-| Render the interactive 3D turntable preview diorama on the Main Menu.
Reuses the same pass-batched, frustum-culled, LOD-optimized renderer as
`renderCityCanvas`.
-}
renderCityPreview :
    { city : CityGrid
    , meshes : MeshCache
    , camera : Camera
    , time : Float
    , width : Int
    , height : Int
    , onMouseDown : Float -> Float -> msg
    , onMouseMove : Float -> Float -> msg
    , onMouseUp : msg
    , onWheel : Float -> msg
    }
    -> Html msg
renderCityPreview cfg =
    let
        w =
            max 1 cfg.width

        h =
            max 1 cfg.height

        entities =
            buildCityEntities
                { city = cfg.city
                , meshes = cfg.meshes
                , camera = cfg.camera
                , selectedLot = Nothing
                , hoveredLot = Nothing
                , peers = []
                , mySlot = 0
                , time = cfg.time
                , width = w
                , height = h
                }

        mousePosDecoder fn =
            Decode.map2 fn
                (Decode.field "offsetX" Decode.float)
                (Decode.field "offsetY" Decode.float)

        wheelDecoder =
            Decode.field "deltaY" Decode.float
                |> Decode.map (\dy -> ( cfg.onWheel dy, True ))
    in
    WebGL.toHtmlWith
        [ WebGL.alpha False
        , WebGL.depth 1
        , WebGL.antialias
        , WebGL.clearColor 0.06 0.12 0.2 1.0
        ]
        [ Attr.width w
        , Attr.height h
        , Attr.class "block w-full h-full cursor-grab active:cursor-grabbing"
        , Attr.attribute "data-testid" "city-preview-canvas"
        , Html.Events.on "mousedown" (mousePosDecoder cfg.onMouseDown)
        , Html.Events.on "mousemove" (mousePosDecoder cfg.onMouseMove)
        , Html.Events.onMouseUp cfg.onMouseUp
        , Html.Events.onMouseLeave cfg.onMouseUp
        , Html.Events.preventDefaultOn "wheel" wheelDecoder
        ]
        entities


collectLotPasses :
    { city : CityGrid
    , meshes : MeshCache
    , selectedLot : Maybe LotCoord
    , hoveredLot : Maybe LotCoord
    , frustum : Frustum
    , viewMat : Mat4
    , projMat : Mat4
    , eye : Vec3
    , projScaleY : Float
    , viewportW : Float
    , viewportH : Float
    }
    -> ( List Entity, List Entity )
collectLotPasses ctx =
    let
        emptyMesh =
            Mesh.emptyLotMesh ctx.meshes

        isHighlightedCoord r c =
            let
                selMatch =
                    case ctx.selectedLot of
                        Just s ->
                            s.row == r && s.col == c

                        Nothing ->
                            False

                hovMatch =
                    case ctx.hoveredLot of
                        Just h ->
                            h.row == r && h.col == c

                        Nothing ->
                            False
            in
            selMatch || hovMatch

        addLotMesh isEmpty centerX centerZ rotation isHighlighted m ( surfAcc, edgeAcc ) =
            let
                worldCenter =
                    rotateLotCenter rotation centerX centerZ m.localCenter
            in
            if not (Instanced.sphereInFrustum ctx.frustum worldCenter m.boundingRadius) then
                ( surfAcc, edgeAcc )

            else
                let
                    dist =
                        Vec3.distance ctx.eye worldCenter

                    lotScreenPx =
                        (Bounds.lotSizeLdu / max 1.0 dist) * ctx.projScaleY

                    detail =
                        lotDetailFor
                            { isEmpty = isEmpty
                            , isHighlighted = isHighlighted
                            , lotScreenPx = lotScreenPx
                            }

                    modelMat =
                        lotModelMatrix centerX centerZ rotation

                    triMesh =
                        if detail.useLod then
                            m.lodTriangleMesh

                        else
                            m.triangleMesh

                    settings =
                        if m.bfcCertified then
                            bfcCullSettings

                        else
                            depthOnlySettings

                    surfEntity =
                        WebGL.entityWith
                            settings
                            Shader.vertexShader
                            Shader.fragmentShader
                            triMesh
                            { modelMatrix = modelMat
                            , viewMatrix = ctx.viewMat
                            , projectionMatrix = ctx.projMat
                            , viewPosition = ctx.eye
                            , lightDirection = cityStyle.lightDirection
                            , instanceColor = Shader.noInstanceColor
                            , ambientStrength = cityStyle.ambientStrength
                            , lightStrength = cityStyle.lightStrength
                            , specularStrength = cityStyle.specularStrength
                            , specularPower = cityStyle.specularPower
                            , rimStrength = cityStyle.rimStrength
                            , rimPower = cityStyle.rimPower
                            , vibrance = cityStyle.vibrance
                            , metallic = 0.0
                            , u_ghostMode = 0.0
                            , u_ghostAlpha = 1.0
                            }

                    nextEdges =
                        if not detail.useLod && detail.edgeWidth > 0.3 && m.hasLines then
                            WebGL.entityWith
                                edgePassSettings
                                EdgeShader.vertexShader
                                EdgeShader.fragmentShader
                                m.lineMesh
                                { modelMatrix = modelMat
                                , viewMatrix = ctx.viewMat
                                , projectionMatrix = ctx.projMat
                                , edgeColor = cityStyle.edgeColor
                                , viewportWidth = ctx.viewportW
                                , viewportHeight = ctx.viewportH
                                , lineWidth = detail.edgeWidth
                                }
                                :: edgeAcc

                        else
                            edgeAcc
                in
                ( surfEntity :: surfAcc, nextEdges )

        step r c acc =
            if r >= ctx.city.rows then
                acc

            else if c >= ctx.city.cols then
                step (r + 1) 0 acc

            else
                let
                    coord =
                        LotCoord r c

                    center =
                        Bounds.lotWorldCenter ctx.city.cols ctx.city.rows coord

                    highlighted =
                        isHighlightedCoord r c

                    nextAcc =
                        case Dict.get ( r, c ) ctx.city.lots of
                            Just el ->
                                case Mesh.getLotMesh el.contentHash ctx.meshes of
                                    Just m ->
                                        addLotMesh False center.x center.z el.rotation highlighted m acc

                                    Nothing ->
                                        if Instanced.sphereInFrustum ctx.frustum (vec3 center.x 4.0 center.z) 115.0 then
                                            ( fallbackSlabEntity center.x center.z (vec4 0.35 0.45 0.58 0.9) ctx.viewMat ctx.projMat :: Tuple.first acc
                                            , Tuple.second acc
                                            )

                                        else
                                            acc

                            Nothing ->
                                case emptyMesh of
                                    Just m ->
                                        addLotMesh True center.x center.z 0 highlighted m acc

                                    Nothing ->
                                        if Instanced.sphereInFrustum ctx.frustum (vec3 center.x 4.0 center.z) 115.0 then
                                            ( fallbackSlabEntity center.x center.z (vec4 0.14 0.47 0.25 1.0) ctx.viewMat ctx.projMat :: Tuple.first acc
                                            , Tuple.second acc
                                            )

                                        else
                                            acc
                in
                step r (c + 1) nextAcc
    in
    step 0 0 ( [], [] )


{-| Draw the studded LEGO base table plinth beneath the city grid using
`Game.BrickLook.surfaceFragmentShader` and a static pre-compiled quad mesh.
-}
cityGroundEntity : CityGrid -> Mat4 -> Mat4 -> Vec3 -> Vec3 -> Entity
cityGroundEntity city viewMat projMat eye sunDir =
    let
        halfX =
            (toFloat city.cols * Bounds.lotSizeLdu) / 2.0 + 60.0

        halfZ =
            (toFloat city.rows * Bounds.lotSizeLdu) / 2.0 + 60.0

        y =
            -Bounds.baseplateBottomY - 0.1

        modelMat =
            Mat4.makeTranslate3 0.0 y 0.0
                |> Mat4.scale3 halfX 1.0 halfZ
    in
    WebGL.entityWith
        bfcCullSettings
        surfaceVertexShader
        BrickLook.surfaceFragmentShader
        groundMesh
        { modelMatrix = modelMat
        , viewMatrix = viewMat
        , projectionMatrix = projMat
        , u_eyePos = eye
        , u_sunDir = sunDir
        , u_fogColor = vec3 0.65 0.8 0.94
        , u_fogNear = 1800.0
        , u_fogFar = 5200.0
        , u_fogMax = 0.75
        , u_skyAmbient = vec3 0.68 0.82 0.96
        , u_groundAmbient = vec3 0.28 0.3 0.34
        , u_studPitch = Bounds.studPitch
        , u_plateStuds = 8.0
        , u_gloss = 0.08
        }


fallbackSlabEntity : Float -> Float -> Vec4 -> Mat4 -> Mat4 -> Entity
fallbackSlabEntity cx cz col viewMat projMat =
    WebGL.entityWith
        bfcCullSettings
        flatVertexShader
        flatFragmentShader
        fallbackSlabMesh
        { viewMatrix = viewMat
        , projectionMatrix = projMat
        , modelMatrix = Mat4.makeTranslate3 cx 0.0 cz
        , u_color = col
        }


{-| Render a glowing perimeter highlight frame around one 8x8 lot in the city,
reusing pre-compiled static ring meshes.
-}
lotHighlightEntity : CityGrid -> LotCoord -> Vec4 -> Float -> Mat4 -> Mat4 -> Entity
lotHighlightEntity city coord color borderWidth viewMat projMat =
    let
        center =
            Bounds.lotWorldCenter city.cols city.rows coord
    in
    WebGL.entityWith
        highlightSettings
        flatVertexShader
        flatFragmentShader
        (highlightMeshForWidth borderWidth)
        { viewMatrix = viewMat
        , projectionMatrix = projMat
        , modelMatrix = Mat4.makeTranslate3 center.x 0.0 center.z
        , u_color = color
        }


hexToVec4 : String -> Float -> Vec4
hexToVec4 hex alpha =
    case String.toLower (String.trim hex) of
        "#ef4444" ->
            vec4 0.94 0.27 0.27 alpha

        "#eab308" ->
            vec4 0.92 0.7 0.03 alpha

        "#10b981" ->
            vec4 0.06 0.73 0.51 alpha

        "#a855f7" ->
            vec4 0.66 0.33 0.97 alpha

        "#f97316" ->
            vec4 0.98 0.45 0.09 alpha

        "#06b6d4" ->
            vec4 0.02 0.71 0.83 alpha

        "#ec4899" ->
            vec4 0.93 0.28 0.6 alpha

        _ ->
            vec4 0.01 0.52 0.78 alpha
