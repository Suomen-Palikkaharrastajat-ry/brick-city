module City.LotGuide exposing (entities)

{-| 3D drafting-table surround, 8x8 stud footprint border, and max-height 32
plates (`256` LDU) boundary cage rendered inside the Element Editor (`Pages.Build`).
-}

import City.Bounds as Bounds
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Render.Camera as Camera exposing (Camera)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest


type alias Vertex =
    { position : Vec3
    , color : Vec4
    }


type alias Uniforms =
    { viewMatrix : Mat4
    , projectionMatrix : Mat4
    }


vertexShader : Shader Vertex Uniforms { vColor : Vec4 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec4 color;

        uniform mat4 viewMatrix;
        uniform mat4 projectionMatrix;

        varying vec4 vColor;

        void main() {
            vColor = color;
            gl_Position = projectionMatrix * viewMatrix * vec4(position, 1.0);
        }
    |]


fragmentShader : Shader {} Uniforms { vColor : Vec4 }
fragmentShader =
    [glsl|
        precision mediump float;

        varying vec4 vColor;

        void main() {
            gl_FragColor = vColor;
        }
    |]


quad : Vertex -> Vertex -> Vertex -> Vertex -> List ( Vertex, Vertex, Vertex )
quad v1 v2 v3 v4 =
    [ ( v1, v2, v3 )
    , ( v1, v3, v4 )
    ]


horizQuad : Float -> Float -> Float -> Float -> Float -> Vec4 -> List ( Vertex, Vertex, Vertex )
horizQuad y x1 x2 z1 z2 col =
    quad
        { position = vec3 x1 y z1, color = col }
        { position = vec3 x2 y z1, color = col }
        { position = vec3 x2 y z2, color = col }
        { position = vec3 x1 y z2, color = col }


boxTriangles : Float -> Float -> Float -> Float -> Float -> Float -> Vec4 -> List ( Vertex, Vertex, Vertex )
boxTriangles x1 y1 z1 x2 y2 z2 col =
    let
        v000 =
            { position = vec3 x1 y1 z1, color = col }

        v100 =
            { position = vec3 x2 y1 z1, color = col }

        v110 =
            { position = vec3 x2 y2 z1, color = col }

        v010 =
            { position = vec3 x1 y2 z1, color = col }

        v001 =
            { position = vec3 x1 y1 z2, color = col }

        v101 =
            { position = vec3 x2 y1 z2, color = col }

        v111 =
            { position = vec3 x2 y2 z2, color = col }

        v011 =
            { position = vec3 x1 y2 z2, color = col }
    in
    List.concat
        [ quad v000 v100 v110 v010
        , quad v101 v001 v011 v111
        , quad v001 v000 v010 v011
        , quad v100 v101 v111 v110
        , quad v010 v110 v111 v011
        , quad v001 v101 v100 v000
        ]


buildFloorMesh : Mesh Vertex
buildFloorMesh =
    let
        floorY =
            -Bounds.baseplateBottomY - 0.1

        table =
            horizQuad floorY -320 320 -320 320 (vec4 0.08 0.11 0.16 0.92)

        outerLot =
            horizQuad (floorY + 0.05) -92 92 -92 92 (vec4 0.14 0.18 0.25 0.95)

        curbBorder =
            let
                c =
                    vec4 0.98 0.78 0.04 0.85

                h =
                    Bounds.lotHalfLdu

                t =
                    3.0
            in
            List.concat
                [ horizQuad (floorY + 0.1) (-h - t) (h + t) (-h - t) -h c
                , horizQuad (floorY + 0.1) (-h - t) (h + t) h (h + t) c
                , horizQuad (floorY + 0.1) (-h - t) -h -h h c
                , horizQuad (floorY + 0.1) h (h + t) -h h c
                ]

        gridLines =
            let
                lineCol =
                    vec4 0.25 0.34 0.48 0.32
            in
            List.range -3 3
                |> List.concatMap
                    (\i ->
                        let
                            p =
                                toFloat i * 40.0
                        in
                        horizQuad (floorY + 0.03) (p - 0.6) (p + 0.6) -240 240 lineCol
                            ++ horizQuad (floorY + 0.03) -240 240 (p - 0.6) (p + 0.6) lineCol
                    )
    in
    WebGL.triangles (table ++ outerLot ++ gridLines ++ curbBorder)


buildCageMesh : Vec4 -> Mesh Vertex
buildCageMesh railColor =
    let
        h =
            Bounds.lotHalfLdu

        topY =
            Bounds.maxHeightLdu

        botY =
            0.0

        postR =
            1.1

        railR =
            1.2

        posts =
            [ ( -h, -h ), ( h, -h ), ( -h, h ), ( h, h ) ]
                |> List.concatMap
                    (\( px, pz ) ->
                        boxTriangles (px - postR) botY (pz - postR) (px + postR) topY (pz + postR) railColor
                    )

        topFrame =
            List.concat
                [ boxTriangles (-h - railR) (topY - railR) (-h - railR) (h + railR) (topY + railR) (-h + railR) railColor
                , boxTriangles (-h - railR) (topY - railR) (h - railR) (h + railR) (topY + railR) (h + railR) railColor
                , boxTriangles (-h - railR) (topY - railR) -h (-h + railR) (topY + railR) h railColor
                , boxTriangles (h - railR) (topY - railR) -h (h + railR) (topY + railR) h railColor
                ]

        tickCol =
            vec4 0.58 0.72 0.9 0.35

        ticks =
            [ 64.0, 128.0, 192.0 ]
                |> List.concatMap
                    (\ty ->
                        [ ( -h, -h ), ( h, -h ), ( -h, h ), ( h, h ) ]
                            |> List.concatMap
                                (\( px, pz ) ->
                                    boxTriangles (px - 2.6) (ty - 0.8) (pz - 2.6) (px + 2.6) (ty + 0.8) (pz + 2.6) tickCol
                                )
                    )
    in
    WebGL.triangles (posts ++ topFrame ++ ticks)


floorMesh : Mesh Vertex
floorMesh =
    buildFloorMesh


normalCageMesh : Mesh Vertex
normalCageMesh =
    buildCageMesh (vec4 0.22 0.74 0.97 0.55)


warningCageMesh : Mesh Vertex
warningCageMesh =
    buildCageMesh (vec4 0.98 0.78 0.04 0.78)


dangerCageMesh : Mesh Vertex
dangerCageMesh =
    buildCageMesh (vec4 0.94 0.27 0.27 0.88)


{-| Render the 8x8 drafting floor and 32-plate max height wireframe cage inside
`Studio.Core.viewWithExtra`.
-}
entities : { heightPlates : Int, hasViolations : Bool } -> Camera -> Float -> List Entity
entities status camera aspect =
    let
        uniforms =
            { viewMatrix = Camera.viewMatrix camera
            , projectionMatrix = Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera)
            }

        settings =
            [ DepthTest.lessOrEqual { write = False, near = 0, far = 1 }
            , Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
            ]

        cage =
            if status.hasViolations || status.heightPlates > Bounds.maxHeightPlates then
                dangerCageMesh

            else if status.heightPlates >= 28 then
                warningCageMesh

            else
                normalCageMesh
    in
    [ WebGL.entityWith settings vertexShader fragmentShader floorMesh uniforms
    , WebGL.entityWith settings vertexShader fragmentShader cage uniforms
    ]
