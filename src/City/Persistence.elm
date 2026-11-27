module City.Persistence exposing
    ( RemoteLotUpdate
    , RoomUpdate(..)
    , decodeCityJson
    , decodeCityValue
    , decodeLotElement
    , decodeRemoteLotUpdate
    , decodeRoomState
    , encodeCityJson
    , encodeCityValue
    , encodeLotElement
    , encodeLotFocus
    , encodeLotUpdate
    )

{-| JSON serialization and deserialization for Brick City `localStorage`
persistence, JSON export/import, and Cloudflare signaling/WebRTC room sync.
-}

import City.Bounds as Bounds
import City.Types
    exposing
        ( CityGrid
        , CityRoom
        , LotCoord
        , LotElement
        , PeerPresence
        , RoomConnection(..)
        , RoomRole(..)
        , categoryFromString
        , categoryToString
        , hashLdrawText
        )
import Dict
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import LDraw.Document as Document


{-| Incoming incremental lot update message from `city-sync.js`.
-}
type alias RemoteLotUpdate =
    { row : Int
    , col : Int
    , element : Maybe LotElement
    , senderSlot : Int
    }


{-| Room status update emitted by `city-sync.js`.
-}
type RoomUpdate
    = InRoom CityRoom
    | NoRoom
    | Unreadable


{-| Encode a `CityGrid` to a compact JSON string for `localStorage` or download.
-}
encodeCityJson : CityGrid -> String
encodeCityJson city =
    Encode.encode 0 (encodeCityValue city)


{-| Encode a `CityGrid` as a `Json.Encode.Value` for ports and sync.
-}
encodeCityValue : CityGrid -> Encode.Value
encodeCityValue city =
    let
        lotPairs =
            Dict.toList city.lots
                |> List.map
                    (\( ( r, c ), el ) ->
                        ( String.fromInt r ++ "," ++ String.fromInt c, encodeLotElement el )
                    )
    in
    Encode.object
        [ ( "cityName", Encode.string city.cityName )
        , ( "cols", Encode.int city.cols )
        , ( "rows", Encode.int city.rows )
        , ( "version", Encode.int city.version )
        , ( "lots", Encode.object lotPairs )
        ]


{-| Encode a single `LotElement` to JSON.
-}
encodeLotElement : LotElement -> Encode.Value
encodeLotElement el =
    Encode.object
        ([ ( "id", Encode.string el.id )
         , ( "name", Encode.string el.name )
         , ( "category", Encode.string (categoryToString el.category) )
         , ( "ldrawText", Encode.string el.ldrawText )
         , ( "rotation", Encode.int el.rotation )
         , ( "updatedAt", Encode.int el.updatedAt )
         , ( "updatedBy", Encode.string el.updatedBy )
         , ( "partCount", Encode.int el.partCount )
         , ( "heightPlates", Encode.int el.heightPlates )
         ]
            ++ (case el.templateId of
                    Just tid ->
                        [ ( "templateId", Encode.string tid ) ]

                    Nothing ->
                        []
               )
        )


{-| Encode an outgoing lot update payload for `Ports.sendLotUpdate`.
-}
encodeLotUpdate : LotCoord -> Maybe LotElement -> Encode.Value
encodeLotUpdate coord maybeEl =
    Encode.object
        [ ( "row", Encode.int coord.row )
        , ( "col", Encode.int coord.col )
        , ( "element"
          , case maybeEl of
                Just el ->
                    encodeLotElement el

                Nothing ->
                    Encode.null
          )
        ]


{-| Encode the local user's focused lot coordinate for `Ports.sendLotFocus`.
-}
encodeLotFocus : Maybe LotCoord -> Encode.Value
encodeLotFocus maybeCoord =
    case maybeCoord of
        Just coord ->
            Encode.object
                [ ( "row", Encode.int coord.row )
                , ( "col", Encode.int coord.col )
                ]

        Nothing ->
            Encode.null


{-| Decode a JSON string into a `CityGrid`.
-}
decodeCityJson : String -> Result Decode.Error CityGrid
decodeCityJson raw =
    Decode.decodeString decodeCityValue raw


{-| Decode a `Json.Decode.Value` into a `CityGrid`.
-}
decodeCityValue : Decoder CityGrid
decodeCityValue =
    Decode.map5
        (\name cols rows lots version ->
            { cityName =
                if String.isEmpty (String.trim name) then
                    "Palikkakaupunki"

                else
                    name
            , cols = Bounds.clampGridSize cols
            , rows = Bounds.clampGridSize rows
            , lots = lots
            , version = max 0 version
            }
        )
        (optionalField "cityName" Decode.string "Palikkakaupunki")
        (optionalField "cols" Decode.int 4)
        (optionalField "rows" Decode.int 4)
        (optionalField "lots" decodeLotsDict Dict.empty)
        (optionalField "version" Decode.int 1)


decodeLotsDict : Decoder (Dict.Dict ( Int, Int ) LotElement)
decodeLotsDict =
    Decode.keyValuePairs decodeLotElement
        |> Decode.map
            (List.filterMap
                (\( keyStr, el ) ->
                    case String.split "," keyStr of
                        [ rStr, cStr ] ->
                            Maybe.map2 (\r c -> ( ( r, c ), el ))
                                (String.toInt (String.trim rStr))
                                (String.toInt (String.trim cStr))

                        _ ->
                            Nothing
                )
                >> Dict.fromList
            )


{-| Decode a single `LotElement` JSON object.
-}
decodeLotElement : Decoder LotElement
decodeLotElement =
    Decode.map8
        (\id name catStr ldraw templateId rot updatedAt ( updatedBy, partCount, heightPlates ) ->
            { id = id
            , name = name
            , category = categoryFromString catStr
            , ldrawText = ldraw
            , document = Document.parse ldraw
            , contentHash = hashLdrawText ldraw
            , templateId = templateId
            , rotation = modBy 4 rot
            , updatedAt = updatedAt
            , updatedBy = updatedBy
            , partCount = partCount
            , heightPlates = heightPlates
            }
        )
        (optionalField "id" Decode.string "lot")
        (optionalField "name" Decode.string "Lot")
        (optionalField "category" Decode.string "custom")
        (Decode.field "ldrawText" Decode.string)
        (Decode.maybe (Decode.field "templateId" Decode.string))
        (optionalField "rotation" Decode.int 0)
        (optionalField "updatedAt" Decode.int 0)
        (Decode.map3 (\a b c -> ( a, b, c ))
            (optionalField "updatedBy" Decode.string "")
            (optionalField "partCount" Decode.int 0)
            (optionalField "heightPlates" Decode.int 0)
        )


{-| Decode an incoming `cityRemoteLotUpdated` port payload.
-}
decodeRemoteLotUpdate : Decoder RemoteLotUpdate
decodeRemoteLotUpdate =
    Decode.map4 RemoteLotUpdate
        (Decode.field "row" Decode.int)
        (Decode.field "col" Decode.int)
        (Decode.field "element" (Decode.nullable decodeLotElement))
        (optionalField "senderSlot" Decode.int 0)


{-| Decode an incoming `cityRoomStateChanged` port payload.
-}
decodeRoomState : Decode.Value -> RoomUpdate
decodeRoomState raw =
    case Decode.decodeValue roomDecoder raw of
        Ok room ->
            if String.isEmpty (String.trim room.roomId) then
                NoRoom

            else
                InRoom room

        Err _ ->
            Unreadable


roomDecoder : Decoder CityRoom
roomDecoder =
    Decode.map6
        (\roomId roleStr statusStr mySlot myName peers ->
            { roomId = roomId
            , role =
                case roleStr of
                    "host" ->
                        RoomHost

                    "guest" ->
                        RoomGuest

                    _ ->
                        LocalSolo
            , status =
                case statusStr of
                    "connected" ->
                        Connected

                    "connecting" ->
                        Connecting

                    _ ->
                        Disconnected
            , mySlot = mySlot
            , myName = myName
            , peers = peers
            }
        )
        (Decode.field "roomId" Decode.string)
        (optionalField "role" Decode.string "")
        (optionalField "status" Decode.string "disconnected")
        (optionalField "mySlot" Decode.int 1)
        (optionalField "myName" Decode.string "Rakentaja")
        (optionalField "peers" (Decode.list peerDecoder) [])


peerDecoder : Decoder PeerPresence
peerDecoder =
    Decode.map5 PeerPresence
        (Decode.field "slot" Decode.int)
        (optionalField "id" Decode.string "")
        (optionalField "name" Decode.string "Builder")
        (optionalField "color" Decode.string "#0284c7")
        (optionalField "focusedLot" (Decode.nullable coordDecoder) Nothing)


coordDecoder : Decoder LotCoord
coordDecoder =
    Decode.map2 LotCoord
        (Decode.field "row" Decode.int)
        (Decode.field "col" Decode.int)


optionalField : String -> Decoder a -> a -> Decoder a
optionalField field decoder fallback =
    Decode.maybe (Decode.field field decoder)
        |> Decode.map (Maybe.withDefault fallback)
