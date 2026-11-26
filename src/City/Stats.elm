module City.Stats exposing (computeStats, inferCategory)

{-| City simulation statistics computed across all 8x8 lots in a `CityGrid`:
estimated microscale population, building count, green space percentage,
connected road links, skyline height, and overall city score.
-}

import City.Types exposing (CityGrid, CityStats, ElementCategory(..), LotElement)
import Dict
import LDraw.Document as Document exposing (Document)
import Set


{-| Compute aggregate statistics for the entire `CityGrid`.
-}
computeStats : CityGrid -> CityStats
computeStats city =
    let
        totalLots =
            city.cols * city.rows

        entries =
            Dict.toList city.lots
                |> List.filter (\( ( r, c ), _ ) -> r >= 0 && c >= 0 && r < city.rows && c < city.cols)

        builtLots =
            List.length entries

        summed =
            List.foldl
                (\( _, el ) acc ->
                    let
                        pop =
                            lotPopulation el

                        isBuilding =
                            case el.category of
                                Residential ->
                                    1

                                Commercial ->
                                    1

                                Civic ->
                                    1

                                Custom ->
                                    if el.heightPlates >= 3 then
                                        1

                                    else
                                        0

                                _ ->
                                    0

                        isPark =
                            if el.category == Park then
                                1

                            else
                                0

                        isRoad =
                            if el.category == Road then
                                1

                            else
                                0

                        isWaterfront =
                            if el.category == Waterfront then
                                1

                            else
                                0

                        isIndustry =
                            if el.category == Industrial then
                                1

                            else
                                0
                    in
                    { population = acc.population + pop
                    , buildings = acc.buildings + isBuilding
                    , parks = acc.parks + isPark
                    , roads = acc.roads + isRoad
                    , waterfronts = acc.waterfronts + isWaterfront
                    , industries = acc.industries + isIndustry
                    , totalParts = acc.totalParts + el.partCount
                    , maxHeight = max acc.maxHeight el.heightPlates
                    }
                )
                { population = 0
                , buildings = 0
                , parks = 0
                , roads = 0
                , waterfronts = 0
                , industries = 0
                , totalParts = 0
                , maxHeight = 0
                }
                entries

        roadCoords =
            entries
                |> List.filter (\( _, el ) -> el.category == Road)
                |> List.map Tuple.first
                |> Set.fromList

        roadLinks =
            roadCoords
                |> Set.foldl
                    (\( r, c ) acc ->
                        let
                            east =
                                if Set.member ( r, c + 1 ) roadCoords then
                                    1

                                else
                                    0

                            south =
                                if Set.member ( r + 1, c ) roadCoords then
                                    1

                                else
                                    0
                        in
                        acc + east + south
                    )
                    0

        greenPct =
            if totalLots <= 0 then
                0

            else
                min 100 (round (toFloat (summed.parks + summed.waterfronts) / toFloat totalLots * 100.0))

        diversityBonus =
            [ summed.buildings > 0
            , summed.parks > 0
            , summed.roads > 0
            , summed.waterfronts > 0
            , summed.industries > 0
            ]
                |> List.filter identity
                |> List.length
                |> (*) 120

        score =
            summed.population
                + (summed.buildings * 45)
                + (summed.parks * 65)
                + (roadLinks * 40)
                + (summed.waterfronts * 50)
                + diversityBonus
                + (summed.maxHeight * 5)
    in
    { population = summed.population
    , buildings = summed.buildings
    , parks = greenPct
    , roads = roadLinks
    , waterfronts = summed.waterfronts
    , industries = summed.industries
    , totalParts = summed.totalParts
    , builtLots = builtLots
    , totalLots = totalLots
    , score = score
    , roadConnections = roadLinks
    , maxHeightPlates = summed.maxHeight
    }


lotPopulation : LotElement -> Int
lotPopulation el =
    case el.category of
        Residential ->
            40 + el.heightPlates * 8 + el.partCount * 2

        Commercial ->
            60 + el.heightPlates * 14 + el.partCount * 3

        Civic ->
            35 + el.heightPlates * 6

        Park ->
            15

        Waterfront ->
            20

        Industrial ->
            10 + el.partCount

        Road ->
            0

        Custom ->
            if el.heightPlates >= 6 then
                30 + el.heightPlates * 10 + el.partCount * 2

            else
                el.partCount


{-| Infer an element category from a custom LDraw document's colours and height.
-}
inferCategory : Document -> Int -> ElementCategory
inferCategory doc heightPlates =
    let
        placements =
            Document.flattenedPlacements doc

        hasColour c =
            List.any (\p -> p.color == c) placements

        greenCount =
            List.length (List.filter (\p -> p.color == 2 || p.color == 288) placements)

        blueCount =
            List.length (List.filter (\p -> p.color == 1 || p.color == 73 || p.color == 43) placements)
    in
    if heightPlates <= 2 && hasColour 0 && hasColour 71 then
        Road

    else if heightPlates <= 8 && blueCount > 2 && not (hasColour 14) then
        Waterfront

    else if heightPlates <= 10 && greenCount > 2 then
        Park

    else if heightPlates >= 14 then
        Commercial

    else if heightPlates >= 4 then
        Residential

    else
        Custom
