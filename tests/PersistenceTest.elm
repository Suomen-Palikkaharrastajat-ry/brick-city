module PersistenceTest exposing (suite)

{-| Unit tests for JSON persistence, network sync decoders/encoders, MPD city
export, #b/... element links, and city statistics calculations.
-}

import City.Ingest as Ingest
import City.Persistence as Persistence exposing (RoomUpdate(..))
import City.Stats as Stats
import City.Templates as Templates
import City.Types exposing (ElementCategory(..), LotCoord, RoomConnection(..), RoomRole(..))
import Dict
import Expect
import I18n exposing (Language(..))
import Json.Decode as Decode
import LDraw.Document as Document
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "City Persistence, Ingest, and Stats"
        [ describe "JSON CityGrid round-trip"
            [ test "encodeCityJson and decodeCityJson preserve city name, size, and lots" <|
                \_ ->
                    let
                        city =
                            Templates.defaultStarterCity En

                        jsonText =
                            Persistence.encodeCityJson city
                    in
                    case Persistence.decodeCityJson jsonText of
                        Ok decoded ->
                            Expect.all
                                [ \c -> Expect.equal city.cityName c.cityName
                                , \c -> Expect.equal city.cols c.cols
                                , \c -> Expect.equal city.rows c.rows
                                , \c -> Expect.equal (Dict.size city.lots) (Dict.size c.lots)
                                , \c ->
                                    Expect.equal
                                        (Dict.get ( 1, 1 ) city.lots |> Maybe.map .rotation)
                                        (Dict.get ( 1, 1 ) c.lots |> Maybe.map .rotation)
                                ]
                                decoded

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            ]
        , describe "Remote room sync codecs"
            [ test "encodeLotUpdate and decodeRemoteLotUpdate round-trip built and cleared lots" <|
                \_ ->
                    let
                        el =
                            Templates.elementFromTemplate En 42 "Mayor" 2 Templates.modernSkyscraper

                        builtVal =
                            Persistence.encodeLotUpdate (LotCoord 2 3) (Just el)

                        clearVal =
                            Persistence.encodeLotUpdate (LotCoord 1 0) Nothing
                    in
                    Expect.all
                        [ \_ ->
                            Decode.decodeValue Persistence.decodeRemoteLotUpdate builtVal
                                |> Expect.equal
                                    (Ok
                                        { row = 2
                                        , col = 3
                                        , element = Just el
                                        , senderSlot = 0
                                        }
                                    )
                        , \_ ->
                            Decode.decodeValue Persistence.decodeRemoteLotUpdate clearVal
                                |> Expect.equal
                                    (Ok
                                        { row = 1
                                        , col = 0
                                        , element = Nothing
                                        , senderSlot = 0
                                        }
                                    )
                        ]
                        ()
            , test "decodeRoomState returns NoRoom for empty roomId and InRoom with peers" <|
                \_ ->
                    let
                        emptyJson =
                            """{"roomId":"","role":"","status":"disconnected","mySlot":1,"myName":"Host","peers":[]}"""

                        activeJson =
                            """{"roomId":"K7M9P2","role":"host","status":"connected","mySlot":1,"myName":"Mayor","peers":[{"slot":1,"id":"host","name":"Mayor","color":"#0284c7","focusedLot":{"row":1,"col":2}}]}"""
                    in
                    Expect.all
                        [ \_ ->
                            Decode.decodeString Decode.value emptyJson
                                |> Result.map Persistence.decodeRoomState
                                |> Expect.equal (Ok NoRoom)
                        , \_ ->
                            case Decode.decodeString Decode.value activeJson |> Result.map Persistence.decodeRoomState of
                                Ok (InRoom room) ->
                                    Expect.all
                                        [ \r -> Expect.equal "K7M9P2" r.roomId
                                        , \r -> Expect.equal RoomHost r.role
                                        , \r -> Expect.equal Connected r.status
                                        , \r -> Expect.equal 1 (List.length r.peers)
                                        ]
                                        room

                                other ->
                                    Expect.fail ("Expected InRoom, got " ++ Debug.toString other)
                        ]
                        ()
            ]
        , describe "Element link and MPD city export"
            [ test "encodeElementLink and decodeElementInput round-trip a microscale lot" <|
                \_ ->
                    let
                        doc =
                            Document.parse Templates.centralPark.ldrawText

                        link =
                            Ingest.encodeElementLink doc
                    in
                    case Ingest.decodeElementInput Nothing link of
                        Ok decoded ->
                            Expect.equal
                                (List.length (Document.flattenedPlacements doc))
                                (List.length (Document.flattenedPlacements decoded.document))

                        Err err ->
                            Expect.fail err
            , test "exportCityMpd emits a multi-submodel MPD referencing every built lot" <|
                \_ ->
                    let
                        city =
                            Templates.defaultStarterCity En

                        mpd =
                            Ingest.exportCityMpd city
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (String.contains "0 FILE city.ldr" mpd)
                        , \_ -> Expect.equal True (String.contains "0 FILE lot-0-0.ldr" mpd)
                        , \_ -> Expect.equal True (String.contains "lot-3-0.ldr" mpd)
                        ]
                        ()
            ]
        , describe "City stats calculation"
            [ test "starter city has positive population, buildings, green percentage, road links, and peak <= 32" <|
                \_ ->
                    let
                        stats =
                            Stats.computeStats (Templates.defaultStarterCity En)
                    in
                    Expect.all
                        [ \s -> Expect.greaterThan 0 s.population
                        , \s -> Expect.greaterThan 0 s.buildings
                        , \s -> Expect.greaterThan 0 s.parks
                        , \s -> Expect.greaterThan 0 s.roadConnections
                        , \s -> Expect.atMost 32 s.maxHeightPlates
                        , \s -> Expect.greaterThan 0 s.score
                        ]
                        stats
            , test "adjacent road lots increase roadConnections count" <|
                \_ ->
                    let
                        base =
                            Templates.blankCity "Roads" 4 4

                        elA =
                            Templates.elementFromTemplate En 1 "A" 0 Templates.straightRoad

                        elB =
                            Templates.elementFromTemplate En 2 "B" 0 Templates.intersection

                        oneRoad =
                            Stats.computeStats { base | lots = Dict.fromList [ ( ( 0, 0 ), elA ) ] }

                        twoAdjacent =
                            Stats.computeStats { base | lots = Dict.fromList [ ( ( 0, 0 ), elA ), ( ( 0, 1 ), elB ) ] }
                    in
                    Expect.equal ( oneRoad.roadConnections, twoAdjacent.roadConnections ) ( 0, 1 )
            ]
        ]
