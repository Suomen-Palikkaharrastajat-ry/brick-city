module City.Ingest exposing
    ( DecodedElement
    , decodeElementInput
    , encodeElementLink
    , exportCityMpd
    , modelTitle
    , validateElement
    )

{-| Import, validate, and export 8x8 microscale elements and combined city
`.mpd` files.

Supports `#b/...` compressed build links (`Studio.HashPayload`), `#model=...`
links (`Studio.UrlPayload`), and raw `.ldr`/`.mpd` text, strictly enforcing
8x8 bounds (`-80..80` X/Z) and max height `32` plates (`256` LDU).

-}

import Base64
import City.Bounds as Bounds
import City.Types exposing (BoundsIssue, CityGrid, LotCoord)
import Dict
import Flate
import LDraw.Document as Document exposing (Document)
import LDraw.Writer as Writer
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3
import Studio.HashPayload as HashPayload
import Studio.UrlPayload as UrlPayload


{-| A decoded element document and display title.
-}
type alias DecodedElement =
    { document : Document
    , title : String
    }


{-| Decode a `#b/...` link, `#model=...` link, or raw LDraw `.ldr`/`.mpd` text.
-}
decodeElementInput : Maybe (List Int) -> String -> Result String DecodedElement
decodeElementInput maybeBytes rawInput =
    let
        trimmed =
            String.trim rawInput
    in
    case maybeBytes |> Maybe.andThen HashPayload.decodeBytes of
        Just payload ->
            Ok { document = payload.document, title = modelTitle payload.document }

        Nothing ->
            if String.isEmpty trimmed then
                Err "Paste a build link or LDraw text first."

            else if isRawLdrText trimmed then
                let
                    doc =
                        Document.parse trimmed
                in
                Ok { document = doc, title = modelTitle doc }

            else
                case HashPayload.parseDirectBase64 trimmed of
                    Just b64 ->
                        case decodeBase64Payload b64 of
                            Just doc ->
                                Ok { document = doc, title = modelTitle doc }

                            Nothing ->
                                Err "That build link is damaged and could not be read."

                    Nothing ->
                        case (UrlPayload.decodeHash trimmed).modelText of
                            Just text ->
                                let
                                    doc =
                                        Document.parse text
                                in
                                Ok { document = doc, title = modelTitle doc }

                            Nothing ->
                                Err "Could not read build link or LDraw text."


{-| Validate an element against Brick City's strict 8x8 stud footprint and
32-plate max height rules.
-}
validateElement : Document -> Result (List BoundsIssue) { heightPlates : Int, partCount : Int }
validateElement =
    Bounds.validateDocument


{-| Encode a single 8x8 element `Document` as a compact `#b/...` build link.
-}
encodeElementLink : Document -> String
encodeElementLink doc =
    HashPayload.encode { mode = HashPayload.FreeBuilding, document = doc, kit = Nothing }
        |> Flate.deflate
        |> Base64.fromBytes
        |> Maybe.map (\b64 -> "#b/" ++ HashPayload.standardToBase64Url b64)
        |> Maybe.withDefault ""


{-| Export the entire `CityGrid` as a valid multi-part LDraw `.mpd` file where
each occupied lot becomes a named submodel placed at its world grid coordinate.
-}
exportCityMpd : CityGrid -> String
exportCityMpd city =
    let
        orderedLots =
            Dict.toList city.lots
                |> List.filter (\( ( r, c ), _ ) -> r >= 0 && c >= 0 && r < city.rows && c < city.cols)
                |> List.sortBy Tuple.first

        submodelName r c =
            "lot-" ++ String.fromInt r ++ "-" ++ String.fromInt c ++ ".ldr"

        lotPlacement ( ( r, c ), el ) =
            let
                center =
                    Bounds.lotWorldCenter city.cols city.rows (LotCoord r c)

                rot =
                    modBy 4 el.rotation

                mat =
                    case rot of
                        1 ->
                            "0 0 1 0 1 0 -1 0 0"

                        2 ->
                            "-1 0 0 0 1 0 0 0 -1"

                        3 ->
                            "0 0 -1 0 1 0 1 0 0"

                        _ ->
                            "1 0 0 0 1 0 0 0 1"
            in
            String.join " "
                [ "1"
                , "16"
                , String.fromFloat center.x
                , "0"
                , String.fromFloat center.z
                , mat
                , submodelName r c
                ]

        lotSubmodel ( ( r, c ), el ) =
            let
                doc =
                    Document.parse el.ldrawText

                placedLines =
                    Document.flattenedPlacements doc
                        |> List.map
                            (\p ->
                                let
                                    pos =
                                        Mat4.transform p.matrix (Vec3.vec3 0 0 0) |> Vec3.toRecord

                                    m =
                                        Mat4.toRecord p.matrix
                                in
                                String.join " "
                                    [ "1"
                                    , String.fromInt p.color
                                    , String.fromFloat pos.x
                                    , String.fromFloat pos.y
                                    , String.fromFloat pos.z
                                    , String.fromFloat m.m11
                                    , String.fromFloat m.m12
                                    , String.fromFloat m.m13
                                    , String.fromFloat m.m21
                                    , String.fromFloat m.m22
                                    , String.fromFloat m.m23
                                    , String.fromFloat m.m31
                                    , String.fromFloat m.m32
                                    , String.fromFloat m.m33
                                    , p.part
                                    ]
                            )
            in
            [ "0 FILE " ++ submodelName r c
            , "0 Name: " ++ el.name
            , "0 BFC CERTIFY CCW"
            ]
                ++ placedLines
                ++ [ "0 NOFILE" ]

        mainLines =
            [ "0 FILE city.ldr"
            , "0 Name: " ++ city.cityName
            , "0 Author: Brick City"
            , "0 BFC CERTIFY CCW"
            ]
                ++ List.map lotPlacement orderedLots
                ++ [ "0 NOFILE" ]
    in
    String.join "\n" (mainLines ++ List.concatMap lotSubmodel orderedLots)


{-| Extract a human-readable title from an LDraw `Document`.
-}
modelTitle : Document -> String
modelTitle doc =
    let
        main =
            Document.mainSubmodel doc

        fromMeta entry =
            case entry of
                Document.Raw raw ->
                    let
                        t =
                            String.trim raw
                    in
                    if String.startsWith "0 Name:" t then
                        Just (String.trim (String.dropLeft 7 t))

                    else
                        Nothing

                Document.PlacementEntry _ ->
                    Nothing

        title =
            List.filterMap fromMeta main.entries
                |> List.head
                |> Maybe.withDefault main.name
    in
    if String.isEmpty (String.trim title) then
        "8×8 Lot"

    else if String.endsWith ".ldr" (String.toLower title) || String.endsWith ".mpd" (String.toLower title) then
        String.dropRight 4 title

    else
        title


isRawLdrText : String -> Bool
isRawLdrText input =
    String.lines input
        |> List.any
            (\line ->
                let
                    t =
                        String.trim line
                in
                String.startsWith "1 " t || String.startsWith "0 FILE" t || String.startsWith "0 Name:" t
            )


decodeBase64Payload : String -> Maybe Document
decodeBase64Payload b64 =
    Base64.toBytes (HashPayload.base64UrlToStandard b64)
        |> Maybe.andThen Flate.inflate
        |> Maybe.andThen HashPayload.decode
        |> Maybe.map .document
        |> Maybe.map (\doc -> Document.parse (Writer.writeDocument doc))
