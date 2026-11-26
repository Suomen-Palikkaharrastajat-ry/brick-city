module City.Templates exposing
    ( Template
    , all
    , blankCity
    , byId
    , centralPark
    , cornerRoad
    , defaultStarterCity
    , elementFromTemplate
    , emptyBaseplateDocument
    , highRise
    , industrialPlant
    , intersection
    , microHouse
    , modernSkyscraper
    , straightRoad
    , waterfrontCanal
    )

{-| Built-in 8x8 stud microscale city element templates and starter city layouts.

Every template stands on a single 8x8 baseplate (`41539.dat`) at `(0, 0, 0)`
covering `-80..80` LDU in X and Z, and stays strictly within the max height of
`32` plates (`256` LDU) above the baseplate.

-}

import City.Bounds as Bounds
import City.Types exposing (CityGrid, ElementCategory(..), LotElement)
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


{-| Pre-built 4x4 microscale starter city showcasing connected roads, a central
park, a canal, houses, towers, a skyscraper, and an industrial plant, while
leaving open green lots ready for the player to build.
-}
defaultStarterCity : Language -> CityGrid
defaultStarterCity lang =
    let
        stamp row col rot tmpl dict =
            Dict.insert ( row, col ) (elementFromTemplate lang 1 "Kaupunki" rot tmpl) dict

        name =
            case lang of
                En ->
                    "Palikka Harbor"

                Fi ->
                    "Palikkalahti"

        lots =
            Dict.empty
                |> stamp 0 0 0 cornerRoad
                |> stamp 0 1 1 straightRoad
                |> stamp 0 2 0 microHouse
                |> stamp 0 3 0 industrialPlant
                |> stamp 1 0 0 straightRoad
                |> stamp 1 1 0 intersection
                |> stamp 1 2 1 straightRoad
                |> stamp 1 3 3 waterfrontCanal
                |> stamp 2 0 1 microHouse
                |> stamp 2 1 0 straightRoad
                |> stamp 2 2 0 centralPark
                |> stamp 2 3 0 highRise
                |> stamp 3 0 0 modernSkyscraper
                |> stamp 3 1 3 cornerRoad
                |> stamp 3 2 1 straightRoad
    in
    { cityName = name
    , cols = 4
    , rows = 4
    , lots = lots
    , version = 1
    }



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
            , -- Asphalt lanes along Z (X -40..40)
              line colBlack -20 -8 -40 turn90M "87079.dat"
            , line colBlack 20 -8 -40 turn90M "87079.dat"
            , line colBlack -20 -8 40 turn90M "87079.dat"
            , line colBlack 20 -8 40 turn90M "87079.dat"
            , -- White dashed lane markers
              line colWhite 0 -16 -50 straightM "3070b.dat"
            , line colWhite 0 -16 10 straightM "3070b.dat"
            , line colWhite 0 -16 50 straightM "3070b.dat"
            , -- Left and right sidewalks
              line colLightBluishGray -60 -8 0 turn90M "87079.dat"
            , line colLightBluishGray -60 -8 -60 straightM "3068b.dat"
            , line colLightBluishGray -60 -8 60 straightM "3068b.dat"
            , line colLightBluishGray 60 -8 0 turn90M "87079.dat"
            , line colLightBluishGray 60 -8 -60 straightM "3068b.dat"
            , line colLightBluishGray 60 -8 60 straightM "3068b.dat"
            , -- Streetlamps
              line colDarkBluishGray -70 -32 -50 straightM "3062b.dat"
            , line colTransYellow -70 -40 -50 straightM "6141.dat"
            , line colDarkBluishGray 70 -32 50 straightM "3062b.dat"
            , line colTransYellow 70 -40 50 straightM "6141.dat"
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
              line colBlack 0 -8 -40 turn90M "87079.dat"
            , line colBlack 40 -8 0 straightM "87079.dat"
            , line colBlack 0 -8 20 straightM "3068b.dat"
            , -- Outer sidewalk (west and south)
              line colLightBluishGray -60 -8 0 turn90M "87079.dat"
            , line colLightBluishGray -60 -8 -60 straightM "3068b.dat"
            , line colLightBluishGray -60 -8 60 straightM "3068b.dat"
            , line colLightBluishGray 20 -8 60 straightM "87079.dat"
            , line colLightBluishGray 70 -8 60 turn90M "3069b.dat"
            , -- Inner corner plaza & micro tree
              line colLightBluishGray 60 -8 -60 straightM "3068b.dat"
            , line colReddishBrown 70 -32 -70 straightM "3062b.dat"
            , line colGreen 70 -56 -70 straightM "4589.dat"
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
            , -- Central asphalt square + four approaches
              line colBlack 0 -8 0 straightM "3068b.dat"
            , line colBlack 0 -8 -50 straightM "3068b.dat"
            , line colBlack 0 -8 50 straightM "3068b.dat"
            , line colBlack -50 -8 0 straightM "3068b.dat"
            , line colBlack 50 -8 0 straightM "3068b.dat"
            , -- White crosswalk stripes
              line colWhite -10 -16 -30 straightM "3070b.dat"
            , line colWhite 10 -16 -30 straightM "3070b.dat"
            , line colWhite -10 -16 30 straightM "3070b.dat"
            , line colWhite 10 -16 30 straightM "3070b.dat"
            , line colWhite -30 -16 -10 straightM "3070b.dat"
            , line colWhite -30 -16 10 straightM "3070b.dat"
            , line colWhite 30 -16 -10 straightM "3070b.dat"
            , line colWhite 30 -16 10 straightM "3070b.dat"
            , -- Corner sidewalks
              line colLightBluishGray -60 -8 -60 straightM "3068b.dat"
            , line colLightBluishGray 60 -8 -60 straightM "3068b.dat"
            , line colLightBluishGray -60 -8 60 straightM "3068b.dat"
            , line colLightBluishGray 60 -8 60 straightM "3068b.dat"
            , -- Traffic light posts
              line colDarkBluishGray -50 -32 -50 straightM "3062b.dat"
            , line colYellow -50 -40 -50 straightM "6141.dat"
            , line colDarkBluishGray 50 -32 50 straightM "3062b.dat"
            , line colGreen 50 -40 50 straightM "6141.dat"
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
            , -- Front footpath & patio tiles
              line colLightBluishGray 0 -8 60 straightM "2431.dat"
            , line colTan 0 -8 20 straightM "87079.dat"
            , -- House 1 (west): tan & trans-light-blue walls + dark red gable roof
              line colTan -40 -24 -20 turn90M "3001.dat"
            , line colTransLightBlue -30 -32 -10 straightM "3024.dat"
            , line colTransLightBlue -30 -32 10 straightM "3024.dat"
            , line colTan -50 -32 0 turn90M "3023b.dat"
            , line colTan -30 -40 0 turn90M "3023b.dat"
            , line colTan -50 -40 0 turn90M "3023b.dat"
            , line colDarkRed -40 -64 -20 straightM "3040b.dat"
            , line colDarkRed -40 -64 20 turn180M "3040b.dat"
            , line colReddishBrown -50 -80 -10 straightM "3024.dat"
            , -- House 2 (east): white cottage + dark tan roof
              line colWhite 30 -24 -10 turn90M "3003.dat"
            , line colTransYellow 30 -32 10 turn90M "3023b.dat"
            , line colWhite 30 -32 -10 turn90M "3023b.dat"
            , line colRed 30 -32 -10 straightM "85984.dat"
            , line colRed 30 -32 10 turn180M "85984.dat"
            , -- Garden trees
              line colReddishBrown 50 -24 50 straightM "3062b.dat"
            , line colDarkGreen 50 -48 50 straightM "4589.dat"
            , line colRed 10 -8 50 straightM "24866.dat"
            ]
        }


{-| Mid-rise 18-plate brick office/apartment block with curtain-wall bands.
-}
highRise : Template
highRise =
    buildTemplate
        { id = "high-rise"
        , nameEn = "High-Rise Tower"
        , nameFi = "Keskikorkea torni"
        , descriptionEn = "18-plate mid-rise apartment and office block with glass bays and rooftop HVAC."
        , descriptionFi = "18 tason kerrostalo lasikaistaleilla ja kattotekniikalla."
        , category = Commercial
        , lines =
            [ line colLightBluishGray 0 0 0 straightM "41539.dat"
            , -- Lobby podium (3 plates = 24 LDU)
              line colDarkBluishGray 0 -24 0 straightM "3001.dat"
            , line colDarkBluishGray 0 -24 -30 straightM "3010.dat"
            , line colTransLightBlue 0 -24 30 straightM "3010.dat"
            , -- Floor 2: masonry + glass band
              line colTan 0 -48 0 straightM "3001.dat"
            , line colTransLightBlue -30 -32 30 straightM "3023b.dat"
            , line colTransLightBlue 30 -32 30 straightM "3023b.dat"
            , line colTan 0 -32 30 straightM "3023b.dat"
            , line colTan 0 -56 0 straightM "3020.dat"
            , -- Upper tower core (4x2 -> height up to Y -136 = 17 plates)
              line colWhite 0 -80 0 straightM "3001.dat"
            , line colTransLightBlue 0 -88 0 straightM "3020.dat"
            , line colWhite 0 -112 0 straightM "3001.dat"
            , line colTransLightBlue 0 -120 0 straightM "3020.dat"
            , line colDarkBluishGray 0 -128 0 straightM "3020.dat"
            , -- Rooftop penthouse & grilles (Y -144 = 18 plates)
              line colLightBluishGray -10 -136 0 straightM "2412b.dat"
            , line colLightBluishGray 20 -136 0 turn90M "2412b.dat"
            , line colDarkBluishGray 0 -144 0 straightM "3068b.dat"
            , -- Street-level shrub & plaza tile
              line colGreen -60 -24 -60 straightM "3062b.dat"
            , line colDarkGreen -60 -48 -60 straightM "4589.dat"
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
            , -- Street plaza tiles & colonnade arches
              line colLightBluishGray -60 -8 0 turn90M "87079.dat"
            , line colLightBluishGray 60 -8 0 turn90M "87079.dat"
            , line colWhite 0 -24 50 straightM "3659.dat"
            , line colWhite 0 -24 -50 straightM "3659.dat"
            , -- Lower 4x4 tower tier (levels 0..9 = 72 LDU)
              line colDarkBluishGray 0 -24 0 straightM "3001.dat"
            , line colTransLightBlue 0 -32 0 straightM "3020.dat"
            , line colDarkBluishGray 0 -56 0 straightM "3001.dat"
            , line colTransLightBlue 0 -64 0 straightM "3020.dat"
            , line colLightBluishGray 0 -72 0 straightM "3031.dat"
            , -- Mid 2x2 glass shaft (levels 9..18 = 144 LDU)
              line colTransLightBlue 0 -96 0 straightM "3003.dat"
            , line colDarkBluishGray 0 -104 0 straightM "3022.dat"
            , line colTransLightBlue 0 -128 0 straightM "3003.dat"
            , line colDarkBluishGray 0 -136 0 straightM "3022.dat"
            , -- Tapered crown & radar spire (18..29 plates = 232 LDU)
              line colTransLightBlue 0 -144 0 straightM "3022.dat"
            , line colWhite 0 -152 0 straightM "4740.dat"
            , line colLightBluishGray 0 -152 0 straightM "3957a.dat"
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
              line colTan 0 -8 -50 turn90M "3069b.dat"
            , line colTan 0 -8 50 turn90M "3069b.dat"
            , line colTan -50 -8 0 straightM "3069b.dat"
            , line colTan 50 -8 0 straightM "3069b.dat"
            , -- Central tiered fountain
              line colLightBluishGray 0 -8 0 straightM "4032b.dat"
            , line colTransLightBlue 0 -16 0 straightM "6141.dat"
            , line colTransLightBlue 0 -24 0 straightM "85861.dat"
            , line colWhite 0 -32 0 straightM "6141.dat"
            , -- Four corner pine trees & flowerbeds
              line colReddishBrown -50 -24 -50 straightM "3062b.dat"
            , line colDarkGreen -50 -48 -50 straightM "4589.dat"
            , line colGreen -50 -72 -50 straightM "4589.dat"
            , line colReddishBrown 50 -24 50 straightM "3062b.dat"
            , line colDarkGreen 50 -48 50 straightM "4589.dat"
            , line colGreen 50 -72 50 straightM "4589.dat"
            , line colReddishBrown 50 -24 -50 straightM "3062b.dat"
            , line colGreen 50 -48 -50 straightM "4589.dat"
            , line colRed -50 -8 50 straightM "24866.dat"
            , line colYellow -30 -8 50 straightM "24866.dat"
            , line colReddishBrown 30 -8 30 straightM "2412b.dat"
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
            , -- Factory hall & sawtooth roofs
              line colDarkTan -20 -24 20 straightM "3001.dat"
            , line colDarkTan -20 -48 20 straightM "3001.dat"
            , line colLightBluishGray -40 -48 20 straightM "85984.dat"
            , line colLightBluishGray 0 -48 20 straightM "85984.dat"
            , -- Loading bay grilles
              line colYellow -30 -24 50 straightM "2877.dat"
            , line colLightBluishGray 10 -8 50 straightM "2412b.dat"
            , -- Storage silo (2x2 round + dome)
              line colLightBluishGray 40 -24 40 straightM "3941.dat"
            , line colLightBluishGray 40 -48 40 straightM "3941.dat"
            , line colDarkBluishGray 40 -56 40 straightM "4740.dat"
            , -- Twin smokestacks with white steam plumes
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
            , -- Trans-light-blue canal tiles along center
              line colTransLightBlue 0 -8 -40 turn90M "87079.dat"
            , line colTransLightBlue 0 -8 40 turn90M "87079.dat"
            , -- Stone quays on west & east banks
              line colLightBluishGray -60 -24 0 turn90M "3001.dat"
            , line colTan -60 -24 -60 straightM "98283.dat"
            , line colTan -60 -24 60 straightM "98283.dat"
            , line colLightBluishGray 60 -24 0 turn90M "3001.dat"
            , line colTan 60 -24 -60 straightM "98283.dat"
            , line colTan 60 -24 60 straightM "98283.dat"
            , -- Arched footbridge spanning the canal
              line colDarkTan 0 -48 0 straightM "3659.dat"
            , line colReddishBrown 0 -56 0 straightM "2431.dat"
            , -- Tiny micro sailboat floating on south canal
              line colReddishBrown 0 -16 50 straightM "3069b.dat"
            , line colWhite -10 -16 50 straightM "54200.dat"
            , -- Quay trees
              line colReddishBrown -60 -48 -50 straightM "3062b.dat"
            , line colGreen -60 -72 -50 straightM "4589.dat"
            ]
        }
