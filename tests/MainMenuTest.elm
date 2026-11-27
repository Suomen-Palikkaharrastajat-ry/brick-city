module MainMenuTest exposing (suite)

{-| Unit tests for Brick City's Main Menu presets, URL hash routing, room
invite URL builder, timestamp formatting, and bilingual localization.
-}

import City.Bounds as Bounds
import City.I18n as CityI18n
import City.Stats as Stats
import City.Templates as Templates exposing (CityPreset(..))
import Dict
import Expect
import I18n exposing (Language(..))
import Main exposing (isCityRouteHash, isMenuHash)
import Pages.MainMenu as MainMenu
import Test exposing (Test, describe, test)
import Time


{-| Test suite for Main Menu presets, routing, room links, and localization.
-}
suite : Test
suite =
    describe "Pages.MainMenu and Starter Layout Presets"
        [ describe "Starter layout presets across grid sizes"
            [ test "all 4 presets generate valid grids for 4x4, 5x5, 6x6, and 8x8" <|
                \_ ->
                    let
                        combos : List ( CityPreset, Int )
                        combos =
                            Templates.allPresets
                                |> List.concatMap (\p -> List.map (\s -> ( p, s )) [ 4, 5, 6, 8 ])

                        checkCombo : ( CityPreset, Int ) -> Maybe String
                        checkCombo ( preset, size ) =
                            let
                                city =
                                    Templates.cityFromPreset Fi preset "Testikaupunki" size

                                badBounds =
                                    Dict.toList city.lots
                                        |> List.filterMap
                                            (\( ( r, c ), el ) ->
                                                if r < 0 || c < 0 || r >= size || c >= size then
                                                    Just ("Out-of-grid lot at " ++ String.fromInt r ++ "," ++ String.fromInt c)

                                                else
                                                    case Bounds.validateDocument el.document of
                                                        Ok _ ->
                                                            Nothing

                                                        Err errs ->
                                                            Just (el.name ++ ": " ++ Debug.toString errs)
                                            )
                            in
                            if city.cols /= size || city.rows /= size then
                                Just ("Wrong grid size for " ++ Templates.presetId preset)

                            else if not (List.isEmpty badBounds) then
                                List.head badBounds

                            else
                                Nothing
                    in
                    combos
                        |> List.filterMap checkCombo
                        |> Expect.equal []
            , test "HarborTown includes roads, park, canal, buildings, and positive population" <|
                \_ ->
                    let
                        city =
                            Templates.cityFromPreset En HarborTown "Harbor" 4

                        stats =
                            Stats.computeStats city
                    in
                    Expect.all
                        [ \s -> Expect.greaterThan 0 s.population
                        , \s -> Expect.greaterThan 0 s.buildings
                        , \s -> Expect.greaterThan 0 s.parks
                        , \s -> Expect.greaterThan 0 s.waterfronts
                        , \s -> Expect.greaterThan 0 s.roadConnections
                        ]
                        stats
            , test "GridMetropolis builds connected perimeter/cross roads and leaves zoned empty lots" <|
                \_ ->
                    let
                        city =
                            Templates.cityFromPreset Fi GridMetropolis "Ruutu" 6

                        stats =
                            Stats.computeStats city
                    in
                    Expect.all
                        [ \s -> Expect.greaterThan 0 s.roadConnections
                        , \s -> Expect.lessThan s.totalLots s.builtLots
                        , \_ -> Expect.equal True (Dict.member ( 0, 0 ) city.lots)
                        , \_ -> Expect.equal True (Dict.member ( 5, 5 ) city.lots)
                        , \_ -> Expect.equal False (Dict.member ( 1, 1 ) city.lots)
                        ]
                        stats
            , test "GreenValley includes central park and waterfront canal with open green lots" <|
                \_ ->
                    let
                        city =
                            Templates.cityFromPreset En GreenValley "Valley" 5

                        stats =
                            Stats.computeStats city
                    in
                    Expect.all
                        [ \s -> Expect.greaterThan 0 s.parks
                        , \s -> Expect.greaterThan 0 s.waterfronts
                        , \s -> Expect.lessThan s.totalLots s.builtLots
                        ]
                        stats
            , test "BlankCanvas starts completely empty for every grid size" <|
                \_ ->
                    [ 4, 5, 6, 8 ]
                        |> List.map (\s -> Dict.size (Templates.cityFromPreset Fi BlankCanvas "Tyhjä" s).lots)
                        |> Expect.equal [ 0, 0, 0, 0 ]
            ]
        , describe "Main Menu URL hash routing"
            [ test "isMenuHash matches root, #/, #menu, and language hashes" <|
                \_ ->
                    [ "", "#", "#/", "#menu", "#/menu", "#lang=en" ]
                        |> List.all isMenuHash
                        |> Expect.equal True
            , test "isCityRouteHash matches solo city routes and rejects menu or room routes" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal True (isCityRouteHash "#/city")
                        , \_ -> Expect.equal True (isCityRouteHash "#city")
                        , \_ -> Expect.equal True (isCityRouteHash "#/play")
                        , \_ -> Expect.equal False (isCityRouteHash "#/city/ABCD")
                        , \_ -> Expect.equal False (isCityRouteHash "#menu")
                        ]
                        ()
            , test "makeCityRoomUrl builds normalized room share links" <|
                \_ ->
                    Expect.all
                        [ \_ ->
                            MainMenu.makeCityRoomUrl "https://kaupunki.palikkaharrastajat.fi/" "ab12cd"
                                |> Expect.equal "https://kaupunki.palikkaharrastajat.fi/#/city/AB12CD"
                        , \_ ->
                            MainMenu.makeCityRoomUrl "http://localhost:5178" "k7m9"
                                |> Expect.equal "http://localhost:5178/#/city/K7M9"
                        , \_ ->
                            MainMenu.makeCityRoomUrl "" "xy99"
                                |> Expect.equal "#/city/XY99"
                        ]
                        ()
            ]
        , describe "Main Menu I18n and timestamp formatting"
            [ test "formatTimestamp formats UTC timestamps cleanly in Finnish and English" <|
                \_ ->
                    let
                        -- 2026-11-26 21:05:00 UTC in milliseconds
                        samplePosix =
                            Time.millisToPosix 1795727100000
                    in
                    Expect.all
                        [ \_ ->
                            CityI18n.formatTimestamp Fi Time.utc samplePosix
                                |> Expect.equal "26.11.2026 klo 21.05"
                        , \_ ->
                            CityI18n.formatTimestamp En Time.utc samplePosix
                                |> Expect.equal "2026-11-26 21:05"
                        ]
                        ()
            , test "all preset titles and menu buttons are localized in Fi and En" <|
                \_ ->
                    Templates.allPresets
                        |> List.all
                            (\p ->
                                not (String.isEmpty (CityI18n.presetTitle Fi p))
                                    && not (String.isEmpty (CityI18n.presetTitle En p))
                                    && (CityI18n.presetTitle Fi p /= CityI18n.presetTitle En p)
                            )
                        |> Expect.equal True
            ]
        ]
