module City.Templates exposing
    ( CityPreset(..)
    , Template
    , all
    , allPresets
    , blankCity
    , byId
    , centralPark
    , cityFromPreset
    , cornerRoad
    , defaultStarterCity
    , elementFromTemplate
    , emptyBaseplateDocument
    , highRise
    , industrialPlant
    , intersection
    , microHouse
    , modernSkyscraper
    , presetId
    , straightRoad
    , waterfrontCanal
    )

{-| Built-in 8x8 stud microscale city element templates and starter city layouts.

Every template stands on a single 8x8 baseplate (`41539.dat`) at `(0, 0, 0)`
covering `-80..80` LDU in X and Z, and stays strictly within the max height of
`32` plates (`256` LDU) above the baseplate.

-}

import City.Bounds as Bounds
import City.Types exposing (CityGrid, ElementCategory(..), LotElement, hashLdrawText)
import Dict
import I18n exposing (Language(..))
import LDraw.Document as Document exposing (Document)


{-| A starter 8x8 microscale city building or street block template.
-}
type alias Template =
    { id : String
    , nameEn : String
    , nameFi : String
    , descriptionEn : String
    , descriptionFi : String
    , category : ElementCategory
    , ldrawText : String
    , document : Document
    , contentHash : String
    , partCount : Int
    , heightPlates : Int
    }


{-| All 9 built-in 8x8 microscale templates in palette order.
-}
all : List Template
all =
    [ straightRoad
    , cornerRoad
    , intersection
    , microHouse
    , highRise
    , modernSkyscraper
    , centralPark
    , industrialPlant
    , waterfrontCanal
    ]


{-| Look up a template by its identifier.
-}
byId : String -> Maybe Template
byId id =
    all
        |> List.filter (\t -> t.id == id)
        |> List.head


{-| Build a `LotElement` from a `Template` with the given rotation (`0..3`),
timestamp, and author label.
-}
elementFromTemplate : Language -> Int -> String -> Int -> Template -> LotElement
elementFromTemplate lang updatedAt updatedBy rotation tmpl =
    { id = tmpl.id ++ "-" ++ String.fromInt updatedAt
    , name =
        case lang of
            En ->
                tmpl.nameEn

            Fi ->
                tmpl.nameFi
    , category = tmpl.category
    , ldrawText = tmpl.ldrawText
    , document = tmpl.document
    , contentHash = tmpl.contentHash
    , templateId = Just tmpl.id
    , rotation = modBy 4 rotation
    , updatedAt = updatedAt
    , updatedBy = updatedBy
    , partCount = tmpl.partCount
    , heightPlates = tmpl.heightPlates
    }


{-| Create a clean 8x8 baseplate document in the requested LDraw colour.
-}
emptyBaseplateDocument : Int -> Document
emptyBaseplateDocument colour =
    Document.parse
        (String.join "\n"
            [ "0 FILE lot-element.ldr"
            , "0 Name: Microscale 8x8 Lot"
            , "0 Author: Brick City"
            , "0 BFC CERTIFY CCW"
            , line colour 0 0 0 straightM "41539.dat"
            ]
        )


{-| Create an empty `CityGrid` with the given name and dimensions.
-}
blankCity : String -> Int -> Int -> CityGrid
blankCity name cols rows =
    { cityName = name
    , cols = Bounds.clampGridSize cols
    , rows = Bounds.clampGridSize rows
    , lots = Dict.empty
    , version = 1
    }


{-| Starter city layout presets available on the Main Menu.
-}
type CityPreset
    = HarborTown
    | GridMetropolis
    | GreenValley
    | BlankCanvas


{-| Ordered list of all starter city layout presets.
-}
allPresets : List CityPreset
allPresets =
    [ GridMetropolis
    , GreenValley
    , HarborTown
    , BlankCanvas
    ]


{-| Stable identifier string for a `CityPreset` (used for test IDs and serialization).
-}
presetId : CityPreset -> String
presetId preset =
    case preset of
        GridMetropolis ->
            "grid-metropolis"

        GreenValley ->
            "green-valley"

        HarborTown ->
            "coastal-bay"

        BlankCanvas ->
            "sandbox-plain"


{-| Generate a `CityGrid` from a starter layout preset, city name, and grid
dimension (`4`, `5`, `6`, `8`, clamped to `2..8`).
-}
cityFromPreset : Language -> CityPreset -> String -> Int -> CityGrid
cityFromPreset lang preset rawName rawSize =
    let
        size =
            Bounds.clampGridSize rawSize

        trimmed =
            String.trim rawName

        name =
            if String.isEmpty trimmed then
                case lang of
                    En ->
                        "Brick Bay"

                    Fi ->
                        "Palikkalahti"

            else
                trimmed

        lots =
            case preset of
                HarborTown ->
                    harborTownLots lang size

                GridMetropolis ->
                    gridMetropolisLots lang size

                GreenValley ->
                    greenValleyLots lang size

                BlankCanvas ->
                    Dict.empty
    in
    { cityName = name
    , cols = size
    , rows = size
    , lots = lots
    , version = 1
    }


{-| Pre-built 4x4 microscale starter city showcasing connected roads, a central
park, a canal, houses, towers, a skyscraper, and an industrial plant, while
leaving open green lots ready for the player to build.
-}
defaultStarterCity : Language -> CityGrid
defaultStarterCity lang =
    let
        name =
            case lang of
                En ->
                    "Palikka Harbor"

                Fi ->
                    "Palikkalahti"
    in
    { cityName = name
    , cols = 4
    , rows = 4
    , lots = harborTownLots lang 4
    , version = 1
    }


stampPresetLot : Language -> Int -> Int -> Int -> Template -> Dict.Dict ( Int, Int ) LotElement -> Dict.Dict ( Int, Int ) LotElement
stampPresetLot lang row col rot tmpl dict =
    Dict.insert ( row, col ) (elementFromTemplate lang 1 "Kaupunki" rot tmpl) dict


harborTownLots : Language -> Int -> Dict.Dict ( Int, Int ) LotElement
harborTownLots lang size =
    let
        base4 =
            Dict.empty
                |> stampPresetLot lang 0 0 0 cornerRoad
                |> stampPresetLot lang 0 1 1 straightRoad
                |> stampPresetLot lang 0 2 0 microHouse
                |> stampPresetLot lang 0 3 0 industrialPlant
                |> stampPresetLot lang 1 0 0 straightRoad
                |> stampPresetLot lang 1 1 0 intersection
                |> stampPresetLot lang 1 2 1 straightRoad
                |> stampPresetLot lang 1 3 3 waterfrontCanal
                |> stampPresetLot lang 2 0 1 microHouse
                |> stampPresetLot lang 2 1 0 straightRoad
                |> stampPresetLot lang 2 2 0 centralPark
                |> stampPresetLot lang 2 3 0 highRise
                |> stampPresetLot lang 3 0 0 modernSkyscraper
                |> stampPresetLot lang 3 1 3 cornerRoad
                |> stampPresetLot lang 3 2 1 straightRoad
    in
    if size <= 4 then
        base4
            |> Dict.filter (\( r, c ) _ -> r < size && c < size)

    else
        let
            last =
                size - 1

            eastCanal =
                List.range 1 last
                    |> List.foldl (\r acc -> stampPresetLot lang r last 0 waterfrontCanal acc) Dict.empty

            southAvenue =
                List.range 2 (last - 1)
                    |> List.foldl (\c acc -> stampPresetLot lang 1 c 1 straightRoad acc) eastCanal

            westAvenue =
                List.range 3 (last - 1)
                    |> List.foldl (\r acc -> stampPresetLot lang r 1 0 straightRoad acc) southAvenue
        in
        westAvenue
            |> stampPresetLot lang 0 0 0 cornerRoad
            |> stampPresetLot lang 0 1 1 straightRoad
            |> stampPresetLot lang 0 2 0 microHouse
            |> stampPresetLot lang 0 last 0 industrialPlant
            |> stampPresetLot lang 1 0 0 straightRoad
            |> stampPresetLot lang 1 1 0 intersection
            |> stampPresetLot lang 2 0 1 microHouse
            |> stampPresetLot lang 2 1 0 straightRoad
            |> stampPresetLot lang 2 2 0 centralPark
            |> stampPresetLot lang 2 (last - 1) 0 highRise
            |> stampPresetLot lang 3 0 0 modernSkyscraper
            |> stampPresetLot lang last 1 3 cornerRoad
            |> stampPresetLot lang last 2 1 straightRoad


gridMetropolisLots : Language -> Int -> Dict.Dict ( Int, Int ) LotElement
gridMetropolisLots lang size =
    let
        last =
            size - 1

        mid =
            size // 2

        coords =
            List.range 0 last
                |> List.concatMap (\r -> List.range 0 last |> List.map (\c -> ( r, c )))
    in
    List.foldl
        (\( r, c ) acc ->
            if r == 0 && c == 0 then
                stampPresetLot lang r c 0 cornerRoad acc

            else if r == 0 && c == last then
                stampPresetLot lang r c 1 cornerRoad acc

            else if r == last && c == last then
                stampPresetLot lang r c 2 cornerRoad acc

            else if r == last && c == 0 then
                stampPresetLot lang r c 3 cornerRoad acc

            else if (r == 0 || r == last) && c == mid then
                stampPresetLot lang r c 0 intersection acc

            else if (c == 0 || c == last) && r == mid then
                stampPresetLot lang r c 0 intersection acc

            else if r == 0 || r == last then
                stampPresetLot lang r c 1 straightRoad acc

            else if c == 0 || c == last then
                stampPresetLot lang r c 0 straightRoad acc

            else if r == mid && c == mid then
                stampPresetLot lang r c 0 intersection acc

            else if r == mid then
                stampPresetLot lang r c 1 straightRoad acc

            else if c == mid then
                stampPresetLot lang r c 0 straightRoad acc

            else
                acc
        )
        Dict.empty
        coords


greenValleyLots : Language -> Int -> Dict.Dict ( Int, Int ) LotElement
greenValleyLots lang size =
    let
        last =
            size - 1

        canalCol =
            last

        withCanal =
            List.range 0 last
                |> List.foldl (\r acc -> stampPresetLot lang r canalCol 0 waterfrontCanal acc) Dict.empty

        midRow =
            (size - 1) // 2

        midCol =
            max 0 ((size - 2) // 2)

        parkCoords =
            if size >= 6 then
                [ ( midRow, midCol ), ( midRow + 1, midCol ), ( midRow, midCol + 1 ), ( midRow + 1, midCol + 1 ) ]

            else if size == 5 then
                [ ( midRow, midCol ), ( midRow, midCol + 1 ) ]

            else
                [ ( midRow, midCol ) ]
    in
    List.foldl
        (\( r, c ) acc ->
            if c < canalCol then
                stampPresetLot lang r c 0 centralPark acc

            else
                acc
        )
        withCanal
        parkCoords



-- ── LDraw Helpers ────────────────────────────────────────────────────────────


straightM : String
straightM =
    "1 0 0 0 1 0 0 0 1"


turn90M : String
turn90M =
    "0 0 1 0 1 0 -1 0 0"


turn180M : String
turn180M =
    "-1 0 0 0 1 0 0 0 -1"


turn270M : String
turn270M =
    "0 0 -1 0 1 0 1 0 0"


line : Int -> Float -> Float -> Float -> String -> String -> String
line colour x y z matrix file =
    String.join " "
        [ "1"
        , String.fromInt colour
        , String.fromFloat x
        , String.fromFloat y
        , String.fromFloat z
        , matrix
        , file
        ]


buildTemplate :
    { id : String
    , nameEn : String
    , nameFi : String
    , descriptionEn : String
    , descriptionFi : String
    , category : ElementCategory
    , lines : List String
    }
    -> Template
buildTemplate cfg =
    let
        text =
            String.join "\n"
                ([ "0 FILE " ++ cfg.id ++ ".ldr"
                 , "0 Name: " ++ cfg.nameEn
                 , "0 Author: Brick City"
                 , "0 BFC CERTIFY CCW"
                 ]
                    ++ cfg.lines
                )

        doc =
            Document.parse text

        measured =
            Bounds.documentBounds doc
    in
    { id = cfg.id
    , nameEn = cfg.nameEn
    , nameFi = cfg.nameFi
    , descriptionEn = cfg.descriptionEn
    , descriptionFi = cfg.descriptionFi
    , category = cfg.category
    , ldrawText = text
    , document = doc
    , contentHash = hashLdrawText text
    , partCount = measured.partCount
    , heightPlates = measured.heightPlates
    }



-- ── Colour Constants ─────────────────────────────────────────────────────────


colBlack : Int
colBlack =
    0


colBlue : Int
colBlue =
    1


colGreen : Int
colGreen =
    2


colRed : Int
colRed =
    4


colYellow : Int
colYellow =
    14


colWhite : Int
colWhite =
    15


colTan : Int
colTan =
    19


colDarkTan : Int
colDarkTan =
    28


colTransLightBlue : Int
colTransLightBlue =
    43


colTransYellow : Int
colTransYellow =
    46


colReddishBrown : Int
colReddishBrown =
    70


colLightBluishGray : Int
colLightBluishGray =
    71


colDarkBluishGray : Int
colDarkBluishGray =
    72


colMediumBlue : Int
colMediumBlue =
    73


colDarkGreen : Int
colDarkGreen =
    288


colDarkRed : Int
colDarkRed =
    320



-- ── 9 Starter Templates (all 8x8 stud = -80..80 X/Z, height <= 32 plates) ────


{-| Straight 4-stud micro road running north-south with sidewalks & streetlamps.
-}
straightRoad : Template
straightRoad =
    buildTemplate
        { id = "straight-road"
        , nameEn = "Straight Road"
        , nameFi = "Suora tie"
        , descriptionEn = "Four-stud asphalt avenue with sidewalks, dashed lane line and micro streetlamps."
        , descriptionFi = "Neljä nuppaa leveä katu jalkakäytävillä, keskiviivalla ja katulampuilla."
        , category = Road
        , lines =
            [ line colDarkBluishGray 0 0 0 straightM "41539.dat"
            , -- Asphalt outer lanes along Z (X = -30 and X = 30)
              line colBlack -30 -8 0 turn90M "4162.dat"
            , line colBlack 30 -8 0 turn90M "4162.dat"
            , -- Asphalt center strip with jumper plates for lane line
              line colBlack 0 -8 -70 straightM "3069b.dat"
            , line colBlack 0 -8 -50 straightM "15573.dat"
            , line colWhite 0 -16 -50 straightM "3070b.dat"
            , line colBlack 0 -8 -20 straightM "3068b.dat"
            , line colBlack 0 -8 10 straightM "15573.dat"
            , line colWhite 0 -16 10 straightM "3070b.dat"
            , line colBlack 0 -8 30 straightM "3069b.dat"
            , line colBlack 0 -8 50 straightM "15573.dat"
            , line colWhite 0 -16 50 straightM "3070b.dat"
            , line colBlack 0 -8 70 straightM "3069b.dat"
            , -- Left sidewalk with jumper for streetlamp
              line colLightBluishGray -60 -8 -70 straightM "3069b.dat"
            , line colLightBluishGray -60 -8 -50 straightM "15573.dat"
            , line colLightBluishGray -60 -8 0 turn90M "87079.dat"
            , line colLightBluishGray -60 -8 60 straightM "3068b.dat"
            , -- Right sidewalk with jumper for streetlamp
              line colLightBluishGray 60 -8 -60 straightM "3068b.dat"
            , line colLightBluishGray 60 -8 0 turn90M "87079.dat"
            , line colLightBluishGray 60 -8 50 straightM "15573.dat"
            , line colLightBluishGray 60 -8 70 straightM "3069b.dat"
            , -- Streetlamps mounted on sidewalk jumpers
              line colDarkBluishGray -60 -32 -50 straightM "3062b.dat"
            , line colTransYellow -60 -40 -50 straightM "6141.dat"
            , line colDarkBluishGray 60 -32 50 straightM "3062b.dat"
            , line colTransYellow 60 -40 50 straightM "6141.dat"
            ]
        }


{-| 90-degree corner road connecting North (-Z) and East (+X) edges.
-}
cornerRoad : Template
cornerRoad =
    buildTemplate
        { id = "corner-road"
        , nameEn = "Corner Road"
        , nameFi = "Kulmatie"
        , descriptionEn = "90-degree street bend linking north and east with a corner kiosk."
        , descriptionFi = "90 asteen mutkakatu, joka yhdistää pohjoisen ja idän sekä kulmapuistikon."
        , category = Road
        , lines =
            [ line colDarkBluishGray 0 0 0 straightM "41539.dat"
            , -- Road surface from -Z (0..-80) turning to +X (0..80)
              line colBlack 0 -8 -60 straightM "87079.dat"
            , line colBlack 0 -8 -20 straightM "87079.dat"
            , line colBlack 0 -8 20 straightM "87079.dat"
            , line colBlack 60 -8 0 turn90M "87079.dat"
            , -- Outer sidewalk (west and south)
              line colLightBluishGray -60 -8 -60 straightM "3068b.dat"
            , line colLightBluishGray -60 -8 -20 straightM "3068b.dat"
            , line colLightBluishGray -60 -8 20 straightM "3068b.dat"
            , line colLightBluishGray -60 -8 50 straightM "15573.dat"
            , line colLightBluishGray -60 -8 70 straightM "3069b.dat"
            , line colLightBluishGray -20 -8 60 straightM "3068b.dat"
            , line colLightBluishGray 20 -8 60 straightM "3068b.dat"
            , line colLightBluishGray 60 -8 60 straightM "3068b.dat"
            , -- Inner corner sidewalk
              line colLightBluishGray 60 -8 -60 straightM "3068b.dat"
            , -- Outer sidewalk streetlamp mounted on jumper plate
              line colDarkBluishGray -60 -32 50 straightM "3062b.dat"
            , line colTransYellow -60 -40 50 straightM "6141.dat"
            ]
        }


{-| 4-way intersection with crosswalks and traffic signal posts.
-}
intersection : Template
intersection =
    buildTemplate
        { id = "intersection"
        , nameEn = "Intersection"
        , nameFi = "Risteys"
        , descriptionEn = "Four-way micro crossroads with zebra crosswalks and traffic lights."
        , descriptionFi = "Nelisuuntainen risteys suojateillä ja liikennevaloilla."
        , category = Road
        , lines =
            [ line colDarkBluishGray 0 0 0 straightM "41539.dat"
            , -- Central asphalt square
              line colBlack 0 -8 0 straightM "3068b.dat"
            , -- Four branch approaches
              line colBlack 0 -8 -60 straightM "87079.dat"
            , line colBlack 0 -8 60 straightM "87079.dat"
            , line colBlack -60 -8 0 turn90M "87079.dat"
            , line colBlack 60 -8 0 turn90M "87079.dat"
            , -- Corner sidewalks
              line colLightBluishGray -60 -8 -60 straightM "3068b.dat"
            , line colLightBluishGray 60 -8 -60 straightM "3068b.dat"
            , line colLightBluishGray -60 -8 60 straightM "3068b.dat"
            , line colLightBluishGray 60 -8 60 straightM "3068b.dat"
            , -- Inlaid asphalt transition corners
              line colBlack -30 -8 -30 straightM "3070b.dat"
            , line colBlack 30 -8 -30 straightM "3070b.dat"
            , line colBlack -30 -8 30 straightM "3070b.dat"
            , line colBlack 30 -8 30 straightM "3070b.dat"
            , -- Inlaid white crosswalk stripes
              line colWhite -10 -8 -30 straightM "3070b.dat"
            , line colWhite 10 -8 -30 straightM "3070b.dat"
            , line colWhite -10 -8 30 straightM "3070b.dat"
            , line colWhite 10 -8 30 straightM "3070b.dat"
            , line colWhite -30 -8 -10 straightM "3070b.dat"
            , line colWhite -30 -8 10 straightM "3070b.dat"
            , line colWhite 30 -8 -10 straightM "3070b.dat"
            , line colWhite 30 -8 10 straightM "3070b.dat"
            ]
        }


{-| Cozy residential micro houses with pitched roofs, windows, and garden.
-}
microHouse : Template
microHouse =
    buildTemplate
        { id = "micro-house"
        , nameEn = "Micro House"
        , nameFi = "Pientalo"
        , descriptionEn = "Two pitched-roof townhomes with glowing windows, chimney and garden hedge."
        , descriptionFi = "Kaksi harjakattoista pientaloa, ikkunaruudut, savupiippu ja pihapensas."
        , category = Residential
        , lines =
            [ line colGreen 0 0 0 straightM "41539.dat"
            , -- Front garden patio / walkway
              line colTan -20 -8 60 straightM "3068b.dat"
            , line colTan 20 -8 60 straightM "3068b.dat"
            , -- House 1 (west): 2x2 cottage with pitched roof
              line colTan -40 -24 -20 straightM "3003.dat"
            , line colTan -40 -48 -20 straightM "3003.dat"
            , line colDarkRed -40 -72 -10 straightM "3039.dat"
            , -- House 2 (east): 2x2 cottage with pitched roof
              line colWhite 40 -24 -20 straightM "3003.dat"
            , line colWhite 40 -48 -20 straightM "3003.dat"
            , line colBlue 40 -72 -10 straightM "3039.dat"
            , -- Garden trees and flowers
              line colReddishBrown 50 -24 30 straightM "3062b.dat"
            , line colDarkGreen 50 -48 30 straightM "4589.dat"
            , line colReddishBrown -50 -24 -50 straightM "3062b.dat"
            , line colGreen -50 -48 -50 straightM "4589.dat"
            , line colRed -30 -8 30 straightM "24866.dat"
            , line colYellow -10 -8 30 straightM "24866.dat"
            ]
        }


{-| Mid-rise 19-plate brick office/apartment block with curtain-wall bands.
-}
highRise : Template
highRise =
    buildTemplate
        { id = "high-rise"
        , nameEn = "High-Rise Tower"
        , nameFi = "Keskikorkea torni"
        , descriptionEn = "19-plate mid-rise apartment and office block with glass bays and rooftop HVAC."
        , descriptionFi = "19 tason kerrostalo lasikaistaleilla ja kattotekniikalla."
        , category = Commercial
        , lines =
            [ line colLightBluishGray 0 0 0 straightM "41539.dat"
            , -- Ground podium (4x4 studs = 80x80 LDU)
              line colDarkBluishGray 0 -24 -20 straightM "3001.dat"
            , line colDarkBluishGray 0 -24 20 straightM "3001.dat"
            , -- Floor 2 (masonry band)
              line colTan 0 -48 -20 straightM "3001.dat"
            , line colTan 0 -48 20 straightM "3001.dat"
            , -- Plate transition band
              line colDarkBluishGray 0 -56 0 straightM "3031.dat"
            , -- Tower core (2x2 shaft, height up to Y -144 = 18 plates)
              line colWhite 0 -80 0 straightM "3003.dat"
            , line colTransLightBlue 0 -88 0 straightM "3022.dat"
            , line colWhite 0 -112 0 straightM "3003.dat"
            , line colTransLightBlue 0 -120 0 straightM "3022.dat"
            , line colDarkBluishGray 0 -144 0 straightM "3003.dat"
            , -- Rooftop crown (Y -152 = 19 plates)
              line colLightBluishGray 0 -152 0 straightM "3068b.dat"
            , -- Plaza landscaping
              line colGreen -50 -24 -50 straightM "3062b.dat"
            , line colDarkGreen -50 -48 -50 straightM "4589.dat"
            ]
        }


{-| Supertall 29-plate modern glass & steel skyscraper right under the 32-plate limit.
-}
modernSkyscraper : Template
modernSkyscraper =
    buildTemplate
        { id = "skyscraper"
        , nameEn = "Modern Skyscraper"
        , nameFi = "Moderni pilvenpiirtäjä"
        , descriptionEn = "Supertall setback glass tower with colonnade plaza and spire reaching 29 plates."
        , descriptionFi = "Huikea 29 tason lasipilvenpiirtäjä pylväiköllä ja huippuantennilla."
        , category = Commercial
        , lines =
            [ line colDarkBluishGray 0 0 0 straightM "41539.dat"
            , -- Plaza sidewalks
              line colLightBluishGray -60 -8 0 turn90M "87079.dat"
            , line colLightBluishGray 60 -8 0 turn90M "87079.dat"
            , -- Lower 4x4 tower tier
              line colDarkBluishGray 0 -24 -20 straightM "3001.dat"
            , line colDarkBluishGray 0 -24 20 straightM "3001.dat"
            , line colTransLightBlue 0 -32 0 straightM "3031.dat"
            , line colDarkBluishGray 0 -56 -20 straightM "3001.dat"
            , line colDarkBluishGray 0 -56 20 straightM "3001.dat"
            , line colTransLightBlue 0 -64 0 straightM "3031.dat"
            , -- Setback 2x2 tower core
              line colWhite 0 -88 0 straightM "3003.dat"
            , line colTransLightBlue 0 -96 0 straightM "3022.dat"
            , line colWhite 0 -104 0 straightM "3022.dat"
            , line colTransLightBlue 0 -112 0 straightM "3022.dat"
            , line colDarkBluishGray 0 -120 0 straightM "3022.dat"
            , -- Crown and spire
              line colTransLightBlue 0 -128 0 straightM "3022.dat"
            , line colWhite 0 -136 0 straightM "4740.dat"
            , line colLightBluishGray 0 -144 0 straightM "3957a.dat"
            ]
        }


{-| Central Park with tiered water fountain, walking paths, and micro trees.
-}
centralPark : Template
centralPark =
    buildTemplate
        { id = "central-park"
        , nameEn = "Central Park"
        , nameFi = "Keskuspuisto"
        , descriptionEn = "Green city park with gravel paths, a tiered fountain, flowers and pine trees."
        , descriptionFi = "Vehreä kaupunkipuisto suihkulähteellä, poluilla, kukilla ja puilla."
        , category = Park
        , lines =
            [ line colGreen 0 0 0 straightM "41539.dat"
            , -- Tan gravel cross paths
              line colTan 0 -8 -60 straightM "3068b.dat"
            , line colTan 0 -8 60 straightM "3068b.dat"
            , line colTan -60 -8 0 straightM "3068b.dat"
            , line colTan 60 -8 0 straightM "3068b.dat"
            , -- Central tiered fountain on 2x2 plate
              line colLightBluishGray 0 -8 0 straightM "3022.dat"
            , line colTransLightBlue -10 -16 -10 straightM "6141.dat"
            , line colTransLightBlue 10 -16 -10 straightM "6141.dat"
            , line colTransLightBlue -10 -16 10 straightM "6141.dat"
            , line colTransLightBlue 10 -16 10 straightM "6141.dat"
            , line colWhite 0 -24 0 straightM "4740.dat"
            , -- Corner pine trees
              line colReddishBrown -50 -24 -50 straightM "3062b.dat"
            , line colDarkGreen -50 -48 -50 straightM "4589.dat"
            , line colGreen -50 -72 -50 straightM "4589.dat"
            , line colReddishBrown 50 -24 -50 straightM "3062b.dat"
            , line colDarkGreen 50 -48 -50 straightM "4589.dat"
            , line colGreen 50 -72 -50 straightM "4589.dat"
            , line colReddishBrown 50 -24 50 straightM "3062b.dat"
            , line colDarkGreen 50 -48 50 straightM "4589.dat"
            , line colGreen 50 -72 50 straightM "4589.dat"
            , -- Flowerbeds
              line colRed -50 -8 50 straightM "24866.dat"
            , line colYellow -30 -8 50 straightM "24866.dat"
            , line colRed -50 -8 30 straightM "24866.dat"
            ]
        }


{-| Industrial factory with sawtooth roof, storage silos, and twin smokestacks.
-}
industrialPlant : Template
industrialPlant =
    buildTemplate
        { id = "industrial-plant"
        , nameEn = "Industrial Plant"
        , nameFi = "Tehdaslaitos"
        , descriptionEn = "Micro factory with sawtooth skylights, twin smokestacks and storage tanks."
        , descriptionFi = "Pienoistehdas saha-katolla, savupiipuilla ja varastosäiliöillä."
        , category = Industrial
        , lines =
            [ line colDarkBluishGray 0 0 0 straightM "41539.dat"
            , -- Factory hall (4x4 footprint)
              line colDarkTan -40 -24 20 straightM "3001.dat"
            , line colDarkTan -40 -24 60 straightM "3001.dat"
            , line colDarkTan -40 -48 20 straightM "3001.dat"
            , line colDarkTan -40 -48 60 straightM "3001.dat"
            , line colLightBluishGray -40 -56 40 straightM "3031.dat"
            , -- Storage silo (2x2 round brick + dome)
              line colLightBluishGray 40 -24 40 straightM "3941.dat"
            , line colLightBluishGray 40 -48 40 straightM "3941.dat"
            , line colDarkBluishGray 40 -56 40 straightM "4740.dat"
            , -- Twin smokestacks with steam plumes
              line colDarkRed -50 -24 -50 straightM "3062b.dat"
            , line colDarkRed -50 -48 -50 straightM "3062b.dat"
            , line colBlack -50 -72 -50 straightM "3062b.dat"
            , line colWhite -50 -80 -50 straightM "6141.dat"
            , line colDarkRed 30 -24 -50 straightM "3062b.dat"
            , line colDarkRed 30 -48 -50 straightM "3062b.dat"
            , line colBlack 30 -72 -50 straightM "3062b.dat"
            , line colWhite 30 -80 -50 straightM "6141.dat"
            ]
        }


{-| Waterfront canal with stone quay, arched footbridge, and tiny sailboat.
-}
waterfrontCanal : Template
waterfrontCanal =
    buildTemplate
        { id = "waterfront-canal"
        , nameEn = "Waterfront Canal"
        , nameFi = "Kanaaliranta"
        , descriptionEn = "Blue waterway running north-south with stone embankment, footbridge and sailboat."
        , descriptionFi = "Sininen kaupunkikanaali kivilaitureilla, kaarisillalla ja purjeveneellä."
        , category = Waterfront
        , lines =
            [ line colMediumBlue 0 0 0 straightM "41539.dat"
            , -- Trans-light-blue canal tiles along center (4x8 studs)
              line colTransLightBlue 0 -8 -60 straightM "87079.dat"
            , line colTransLightBlue 0 -8 -20 straightM "87079.dat"
            , line colTransLightBlue 0 -8 20 straightM "87079.dat"
            , line colTransLightBlue 0 -8 60 straightM "87079.dat"
            , -- Stone quays on west bank (X = -60)
              line colLightBluishGray -60 -24 -60 straightM "3003.dat"
            , line colLightBluishGray -60 -24 -20 straightM "3003.dat"
            , line colLightBluishGray -60 -24 20 straightM "3003.dat"
            , line colLightBluishGray -60 -24 60 straightM "3003.dat"
            , -- Stone quays on east bank (X = 60)
              line colLightBluishGray 60 -24 -60 straightM "3003.dat"
            , line colLightBluishGray 60 -24 -20 straightM "3003.dat"
            , line colLightBluishGray 60 -24 20 straightM "3003.dat"
            , line colLightBluishGray 60 -24 60 straightM "3003.dat"
            , -- Footbridge spanning the canal (6 studs = 120 LDU wide from quay to quay)
              line colDarkTan 0 -32 0 straightM "3795.dat"
            , line colReddishBrown 0 -40 -10 straightM "6636.dat"
            , line colReddishBrown 0 -40 10 straightM "6636.dat"
            , -- Quay trees
              line colReddishBrown -70 -48 -50 straightM "3062b.dat"
            , line colGreen -70 -72 -50 straightM "4589.dat"
            , line colReddishBrown 70 -48 50 straightM "3062b.dat"
            , line colGreen 70 -72 50 straightM "4589.dat"
            ]
        }
