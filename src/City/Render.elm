module City.Render exposing
    ( cityGroundEntity
    , lotHighlightEntity
    , pickLotAt
    , renderCityCanvas
    , resetCameraForCity
    )

{-| Full 3D interactive WebGL City Diorama renderer (`City.Render`):
renders the LEGO sky dome (`Game.BrickLook`), the dark architectural city
table plinth, every occupied and open 8x8 lot (`City.Mesh`), the yellow
selection frame, and remote collaborator focus rings (`PeerPresence`).

Supports camera orbit, pan, zoom, street-level Walk mode (`Render.Camera`),
and raycasted lot picking (`Render.Pick`).

-}

import City.Bounds as Bounds
import City.Mesh as Mesh exposing (MeshCache)
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
import Render.Pick as Pick
import Render.Scene as Scene
import Render.Style as Style
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest


type alias FlatVertex =
    { position : Vec3
    , color : Vec4
    }


type alias FlatUniforms =
    { viewMatrix : Mat4
    , projectionMatrix : Mat4
    , modelMatrix : Mat4
    }


type alias SurfaceVertex =
    { position : Vec3
    , normal : Vec3
    , color : Vec3
    }


type alias CitySurfaceUniforms =
    { viewMatrix : Mat4
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


flatVertexShader : Shader FlatVertex FlatUniforms { vColor : Vec4 }
flatVertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec4 color;

        uniform mat4 viewMatrix;
        uniform mat4 projectionMatrix;
        uniform mat4 modelMatrix;

        varying vec4 vColor;

        void main() {
            vColor = color;
            gl_Position = projectionMatrix * viewMatrix * (modelMatrix * vec4(position, 1.0));
        }
    |]


flatFragmentShader : Shader {} FlatUniforms { vColor : Vec4 }
flatFragmentShader =
    [glsl|
        precision mediump float;

        varying vec4 vColor;

        void main() {
            gl_FragColor = vColor;
        }
    |]


surfaceVertexShader : Shader SurfaceVertex CitySurfaceUniforms BrickLook.SurfaceVaryings
surfaceVertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec3 color;

        uniform mat4 viewMatrix;
        uniform mat4 projectionMatrix;

        varying vec3 v_worldPos;
        varying vec3 v_normal;
        varying vec3 v_color;
        varying float v_studs;

        void main() {
            v_worldPos = position;
            v_normal = normal;
            v_color = color;
            v_studs = 1.0;
            gl_Position = projectionMatrix * viewMatrix * vec4(position, 1.0);
        }
    |]


{-| Frame the camera cleanly around a `cols x rows` city grid.
-}
resetCameraForCity : CityGrid -> Int -> Camera
resetCameraForCity city viewportHeight =
    let
        span =
            max city.cols city.rows |> toFloat |> (*) Bounds.lotSizeLdu

        dist =
            max 520.0 (span * 1.42)
    in
    Camera.init
        |> (\c -> { c | azimuth = 0.68, elevation = 0.62, distance = dist, target = vec3 0 24 0 })
        |> Camera.setZoomRange 140.0 (max 2400.0 (span * 4.5))
        |> Camera.setSceneRadius (span * 0.9)
        |> Camera.setViewportHeight (toFloat (max 1 viewportHeight))
        |> Camera.setNavigation Orbit


{-| Pick which `(row, col)` lot in `city` lies under screen pixel `(x, y)`
on a canvas of `(width, height)`. Intersects the lot bounding slabs (height up
to 256 LDU for built lots or baseplate for empty lots) and falls back to the
`Y = 0` ground plane so clicking tall towers or open lots both feel natural.
-}
pickLotAt : Camera -> Int -> Int -> Float -> Float -> CityGrid -> Maybe LotCoord
pickLotAt camera width height mouseX mouseY city =
    case Pick.screenRay camera width height mouseX mouseY of
        Nothing ->
            Nothing

        Just ( rayOrigin, rayDir ) ->
            let
                lotCandidates =
                    List.range 0 (city.rows - 1)
                        |> List.concatMap
                            (\r ->
                                List.range 0 (city.cols - 1)
                                    |> List.map (\c -> LotCoord r c)
                            )

                hitDistance coord =
                    let
                        center =
                            Bounds.lotWorldCenter city.cols city.rows coord

                        half =
                            Bounds.lotHalfLdu - 0.5

                        heightLdu =
                            case Dict.get ( coord.row, coord.col ) city.lots of
                                Just el ->
                                    max 12.0 (toFloat el.heightPlates * Bounds.plateHeight)

                                Nothing ->
                                    8.0

                        minCorner =
                            vec3 (center.x - half) -8.0 (center.z - half)

                        maxCorner =
                            vec3 (center.x + half) heightLdu (center.z + half)
                    in
                    Pick.rayAabbHit rayOrigin rayDir minCorner maxCorner
                        |> Maybe.map (\t -> ( t, coord ))

                boxHit =
                    lotCandidates
                        |> List.filterMap hitDistance
                        |> List.sortBy Tuple.first
                        |> List.head
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

        aspect =
            toFloat w / toFloat h

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

        eye =
            Camera.position cfg.camera

        sunDir =
            Vec3.normalize (vec3 0.55 0.8 0.45)

        sky =
            BrickLook.skyEntity
                { viewProjection = viewProj
                , eye = eye
                , zenith = vec3 0.22 0.49 0.82
                , horizon = vec3 0.68 0.84 0.96
                , sunDir = sunDir
                , time = cfg.time
                }

        ground =
            cityGroundEntity cfg.city viewMat projMat eye sunDir

        style =
            Style.defaultStyle

        lotEntities =
            List.range 0 (cfg.city.rows - 1)
                |> List.concatMap
                    (\r ->
                        List.range 0 (cfg.city.cols - 1)
                            |> List.concatMap
                                (\c ->
                                    let
                                        coord =
                                            LotCoord r c

                                        center =
                                            Bounds.lotWorldCenter cfg.city.cols cfg.city.rows coord
                                    in
                                    case Dict.get ( r, c ) cfg.city.lots of
                                        Just el ->
                                            case Mesh.getLotMesh el.ldrawText cfg.meshes of
                                                Just m ->
                                                    let
                                                        angle =
                                                            -(toFloat (modBy 4 el.rotation)) * (pi / 2.0)

                                                        modelMat =
                                                            Mat4.makeTranslate3 center.x 0.0 center.z
                                                                |> Mat4.rotate angle (vec3 0 1 0)
                                                    in
                                                    Scene.renderSceneWithTransform modelMat m.scene cfg.camera style w h

                                                Nothing ->
                                                    fallbackSlabEntity center.x center.z (vec4 0.35 0.45 0.58 0.9) viewMat projMat

                                        Nothing ->
                                            case Mesh.emptyLotMesh cfg.meshes of
                                                Just m ->
                                                    let
                                                        modelMat =
                                                            Mat4.makeTranslate3 center.x 0.0 center.z
                                                    in
                                                    Scene.renderSceneWithTransform modelMat m.scene cfg.camera style w h

                                                Nothing ->
                                                    fallbackSlabEntity center.x center.z (vec4 0.14 0.47 0.25 1.0) viewMat projMat
                                )
                    )

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

        entities =
            [ sky, ground ]
                ++ lotEntities
                ++ hoverEntities
                ++ selectionEntities
                ++ peerEntities

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


{-| Draw the studded LEGO base table plinth beneath the city grid using
`Game.BrickLook.surfaceFragmentShader`.
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

        col =
            vec3 0.18 0.22 0.28

        n =
            vec3 0 1 0

        v1 =
            { position = vec3 -halfX y -halfZ, normal = n, color = col }

        v2 =
            { position = vec3 halfX y -halfZ, normal = n, color = col }

        v3 =
            { position = vec3 halfX y halfZ, normal = n, color = col }

        v4 =
            { position = vec3 -halfX y halfZ, normal = n, color = col }

        mesh =
            WebGL.triangles [ ( v1, v3, v2 ), ( v1, v4, v3 ) ]
    in
    WebGL.entity
        surfaceVertexShader
        BrickLook.surfaceFragmentShader
        mesh
        { viewMatrix = viewMat
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


fallbackSlabEntity : Float -> Float -> Vec4 -> Mat4 -> Mat4 -> List Entity
fallbackSlabEntity cx cz col viewMat projMat =
    let
        h =
            Bounds.lotHalfLdu - 2.0

        y =
            0.2

        v1 =
            { position = vec3 -h y -h, color = col }

        v2 =
            { position = vec3 h y -h, color = col }

        v3 =
            { position = vec3 h y h, color = col }

        v4 =
            { position = vec3 -h y h, color = col }

        mesh =
            WebGL.triangles [ ( v1, v3, v2 ), ( v1, v4, v3 ) ]
    in
    [ WebGL.entity
        flatVertexShader
        flatFragmentShader
        mesh
        { viewMatrix = viewMat
        , projectionMatrix = projMat
        , modelMatrix = Mat4.makeTranslate3 cx 0.0 cz
        }
    ]


{-| Render a glowing perimeter highlight frame around one 8x8 lot in the city.
-}
lotHighlightEntity : CityGrid -> LotCoord -> Vec4 -> Float -> Mat4 -> Mat4 -> Entity
lotHighlightEntity city coord color borderWidth viewMat projMat =
    let
        center =
            Bounds.lotWorldCenter city.cols city.rows coord

        outer =
            Bounds.lotHalfLdu + 1.5

        inner =
            Bounds.lotHalfLdu - borderWidth

        y =
            2.4

        quad x1 x2 z1 z2 =
            [ ( { position = vec3 x1 y z1, color = color }
              , { position = vec3 x2 y z2, color = color }
              , { position = vec3 x2 y z1, color = color }
              )
            , ( { position = vec3 x1 y z1, color = color }
              , { position = vec3 x1 y z2, color = color }
              , { position = vec3 x2 y z2, color = color }
              )
            ]

        tris =
            List.concat
                [ quad -outer outer -outer -inner
                , quad -outer outer inner outer
                , quad -outer -inner -inner inner
                , quad inner outer -inner inner
                ]

        settings =
            [ DepthTest.lessOrEqual { write = False, near = 0, far = 1 }
            , Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
            ]
    in
    WebGL.entityWith
        settings
        flatVertexShader
        flatFragmentShader
        (WebGL.triangles tris)
        { viewMatrix = viewMat
        , projectionMatrix = projMat
        , modelMatrix = Mat4.makeTranslate3 center.x 0.0 center.z
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
