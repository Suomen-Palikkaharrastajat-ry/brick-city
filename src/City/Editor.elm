module City.Editor exposing
    ( PartCategory
    , PartEntry
    , Swatch
    , baseplateIntact
    , defaultColour
    , dropMatrix
    , isBaseplatePlacement
    , palette
    , partCategories
    , parts
    , recolorBaseplate
    )

{-| Curated microscale brick palette, architectural colours, baseplate protection,
and bounded stud-grid placement rules for the Brick City Element Editor.
-}

import City.Bounds as Bounds
import Dict
import LDraw.Connectivity as Connectivity exposing (BoundingBox3D, ConnectionType(..))
import LDraw.Document as Document exposing (Document, Placement)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Set exposing (Set)


{-| One LDraw colour swatch in the Element Editor palette.
-}
type alias Swatch =
    { code : Int
    , nameEn : String
    , nameFi : String
    }


{-| 16 microscale city architectural colours including trans-blue glass and
warm glowing window colours.
-}
palette : List Swatch
palette =
    [ { code = 15, nameEn = "White", nameFi = "Valkoinen" }
    , { code = 71, nameEn = "Light Bluish Gray", nameFi = "Vaaleanharmaa" }
    , { code = 72, nameEn = "Dark Bluish Gray", nameFi = "Tummanharmaa" }
    , { code = 0, nameEn = "Black", nameFi = "Musta" }
    , { code = 19, nameEn = "Tan", nameFi = "Hiekka" }
    , { code = 28, nameEn = "Dark Tan", nameFi = "Tumma hiekka" }
    , { code = 70, nameEn = "Reddish Brown", nameFi = "Punaruskea" }
    , { code = 4, nameEn = "Red", nameFi = "Punainen" }
    , { code = 320, nameEn = "Dark Red", nameFi = "Tummanpunainen" }
    , { code = 14, nameEn = "Yellow", nameFi = "Keltainen" }
    , { code = 2, nameEn = "Green", nameFi = "Vihreä" }
    , { code = 288, nameEn = "Dark Green", nameFi = "Tummanvihreä" }
    , { code = 1, nameEn = "Blue", nameFi = "Sininen" }
    , { code = 73, nameEn = "Medium Blue", nameFi = "Keskisininen" }
    , { code = 43, nameEn = "Trans-Light Blue (Glass)", nameFi = "Läpinäkyvä sininen (lasi)" }
    , { code = 46, nameEn = "Trans-Yellow (Light)", nameFi = "Läpinäkyvä keltainen (valo)" }
    ]


{-| Default brick colour when the editor opens.
-}
defaultColour : Int
defaultColour =
    71


{-| One microscale part entry in the editor dock.
-}
type alias PartEntry =
    { file : String
    , nameEn : String
    , nameFi : String
    }


{-| A group of related microscale parts.
-}
type alias PartCategory =
    { titleEn : String
    , titleFi : String
    , parts : List PartEntry
    }


{-| Curated microscale brick catalog for 8x8 city elements.
Every part exists in `brick-catalog/data/curated-parts.json` and has explicit
`LDraw.Connectivity.knownCatalogPart` geometry for stud snapping.
-}
partCategories : List PartCategory
partCategories =
    [ { titleEn = "Micro Bricks"
      , titleFi = "Peruspalikat"
      , parts =
            [ part "3005.dat" "Brick 1 × 1" "Palikka 1 × 1"
            , part "3004.dat" "Brick 1 × 2" "Palikka 1 × 2"
            , part "3622.dat" "Brick 1 × 3" "Palikka 1 × 3"
            , part "3010.dat" "Brick 1 × 4" "Palikka 1 × 4"
            , part "3003.dat" "Brick 2 × 2" "Palikka 2 × 2"
            , part "3002.dat" "Brick 2 × 3" "Palikka 2 × 3"
            , part "3001.dat" "Brick 2 × 4" "Palikka 2 × 4"
            , part "3062b.dat" "Round Brick 1 × 1" "Pyöreä palikka 1 × 1"
            , part "3941.dat" "Round Brick 2 × 2" "Pyöreä palikka 2 × 2"
            , part "3245c.dat" "Pillar 1 × 2 × 2" "Pylväs 1 × 2 × 2"
            , part "43888.dat" "Column 1 × 1 × 6" "Pilari 1 × 1 × 6"
            ]
      }
    , { titleEn = "Facade & Windows"
      , titleFi = "Julkisivu ja ikkunat"
      , parts =
            [ part "98283.dat" "Masonry Brick 1 × 2" "Tiilipalikka 1 × 2"
            , part "2877.dat" "Grille Brick 1 × 2" "Säleikköpalikka 1 × 2"
            , part "30136.dat" "Log Brick 1 × 2" "Hirsipalikka 1 × 2"
            , part "60592.dat" "Window Frame 1 × 2 × 2" "Ikkunakehys 1 × 2 × 2"
            , part "60593.dat" "Tall Window 1 × 2 × 3" "Korkea ikkuna 1 × 2 × 3"
            , part "87552.dat" "Wall Panel 1 × 2 × 2" "Seinäpaneeli 1 × 2 × 2"
            , part "4865b.dat" "Wall Panel 1 × 2 × 1" "Seinäpaneeli 1 × 2 × 1"
            , part "3633.dat" "Fence 1 × 4 × 1" "Aita 1 × 4 × 1"
            ]
      }
    , { titleEn = "Plates & Foundations"
      , titleFi = "Levyt ja perustat"
      , parts =
            [ part "3024.dat" "Plate 1 × 1" "Levy 1 × 1"
            , part "3023b.dat" "Plate 1 × 2" "Levy 1 × 2"
            , part "3623.dat" "Plate 1 × 3" "Levy 1 × 3"
            , part "3710.dat" "Plate 1 × 4" "Levy 1 × 4"
            , part "3022.dat" "Plate 2 × 2" "Levy 2 × 2"
            , part "3020.dat" "Plate 2 × 4" "Levy 2 × 4"
            , part "3031.dat" "Plate 4 × 4" "Levy 4 × 4"
            , part "3958.dat" "Plate 6 × 6" "Levy 6 × 6"
            , part "6141.dat" "Round Plate 1 × 1" "Pyöreä levy 1 × 1"
            , part "85861.dat" "Open Stud 1 × 1" "Avoreikälevy 1 × 1"
            , part "4032b.dat" "Round Plate 2 × 2" "Pyöreä levy 2 × 2"
            , part "15573.dat" "Jumper Plate 1 × 2" "Keskityslevy 1 × 2"
            , part "87580.dat" "Jumper Plate 2 × 2" "Keskityslevy 2 × 2"
            ]
      }
    , { titleEn = "Roads & Smooth Tiles"
      , titleFi = "Tiet ja sileät laatat"
      , parts =
            [ part "3070b.dat" "Tile 1 × 1" "Laatta 1 × 1"
            , part "3069b.dat" "Tile 1 × 2" "Laatta 1 × 2"
            , part "63864.dat" "Tile 1 × 3" "Laatta 1 × 3"
            , part "2431.dat" "Tile 1 × 4" "Laatta 1 × 4"
            , part "3068b.dat" "Tile 2 × 2" "Laatta 2 × 2"
            , part "26603.dat" "Tile 2 × 3" "Laatta 2 × 3"
            , part "87079.dat" "Tile 2 × 4" "Laatta 2 × 4"
            , part "4162.dat" "Tile 1 × 8" "Laatta 1 × 8"
            , part "98138.dat" "Round Tile 1 × 1" "Pyöreä laatta 1 × 1"
            , part "14769.dat" "Round Tile 2 × 2" "Pyöreä laatta 2 × 2"
            , part "2412b.dat" "Grille Tile 1 × 2" "Ritilälaatta 1 × 2"
            ]
      }
    , { titleEn = "Roofs, Slopes & Arches"
      , titleFi = "Katot, viisteet ja kaaret"
      , parts =
            [ part "54200.dat" "Cheese Slope 1 × 1" "Juustoviiste 1 × 1"
            , part "35464.dat" "Double Slope 1 × 1" "Kaksoisviiste 1 × 1"
            , part "85984.dat" "Cheese Slope 1 × 2" "Juustoviiste 1 × 2"
            , part "3040b.dat" "Slope 45° 2 × 1" "Viiste 45° 2 × 1"
            , part "3039.dat" "Slope 45° 2 × 2" "Viiste 45° 2 × 2"
            , part "60481a.dat" "Spire Slope 2 × 1 × 2" "Torniviiste 2 × 1 × 2"
            , part "4460b.dat" "Spire Slope 2 × 1 × 3" "Torniviiste 2 × 1 × 3"
            , part "11477.dat" "Curved Slope 2 × 1" "Kaareva viiste 2 × 1"
            , part "15068.dat" "Curved Slope 2 × 2" "Kaareva viiste 2 × 2"
            , part "4490.dat" "Arch 1 × 3" "Kaari 1 × 3"
            , part "3659.dat" "Arch 1 × 4" "Kaari 1 × 4"
            , part "6005.dat" "Curved Arch 1 × 3 × 2" "Kaareva holvi 1 × 3 × 2"
            ]
      }
    , { titleEn = "Trees, Spires & Detail"
      , titleFi = "Puut, tornit ja yksityiskohdat"
      , parts =
            [ part "4589.dat" "Cone 1 × 1" "Kartio 1 × 1"
            , part "3942c.dat" "Cone 2 × 2 × 2" "Tornikartio 2 × 2 × 2"
            , part "4740.dat" "Dish 2 × 2" "Kupoli 2 × 2"
            , part "3957a.dat" "Antenna Spire 4H" "Antenni 4H"
            , part "32828.dat" "Micro Lamp / Hydrant" "Katulamppu / posti"
            , part "32607.dat" "3-Leaf Plant 1 × 1" "Kasvi 3 lehteä"
            , part "24866.dat" "Flower 1 × 1" "Kukka 1 × 1"
            , part "87087.dat" "SNOT Brick 1 × 1" "Sivunastapalikka 1 × 1"
            , part "4070.dat" "Headlight Brick 1 × 1" "Lyhtypalikka 1 × 1"
            , part "4733.dat" "4-Stud Brick 1 × 1" "Nelinastapalikka 1 × 1"
            ]
      }
    ]


{-| Flattened list of all microscale editor parts in dock order.
-}
parts : List PartEntry
parts =
    List.concatMap .parts partCategories


part : String -> String -> String -> PartEntry
part file nameEn nameFi =
    { file = file, nameEn = nameEn, nameFi = nameFi }


{-| True if `placement` is the anchored 8x8 baseplate (`41539.dat`) at the lot
center `(0, 0, 0)`.
-}
isBaseplatePlacement : Placement -> Bool
isBaseplatePlacement placement =
    let
        pos =
            Mat4.transform placement.matrix (vec3 0 0 0) |> Vec3.toRecord
    in
    String.toLower (String.trim placement.part)
        == "41539.dat"
        && abs pos.x
        < 0.5
        && abs pos.y
        < 0.5
        && abs pos.z
        < 0.5


{-| True if the document retains at least one anchored 8x8 baseplate at `(0, 0, 0)`.
-}
baseplateIntact : Document -> Bool
baseplateIntact doc =
    Document.mainPlacements doc
        |> List.any isBaseplatePlacement


{-| Recolor the anchored 8x8 baseplate in place.
-}
recolorBaseplate : Int -> Document -> Document
recolorBaseplate colour doc =
    Document.mainPlacements doc
        |> List.filter isBaseplatePlacement
        |> List.foldl
            (\p acc -> Document.updatePlacement p.id (\pl -> { pl | color = colour }) acc)
            doc



-- ── Bounded Drop Matrix ──────────────────────────────────────────────────────


type alias WorldPart =
    { part : String
    , matrix : Mat4
    }


{-| Choose a free stud spot inside the 8x8 lot (`-80..80` X/Z, height `<= 32`
plates) near the selected placement or lot center.
-}
dropMatrix : Document -> Set Int -> String -> Mat4
dropMatrix doc selection partFile =
    let
        everything =
            Document.mainPlacements doc |> List.concatMap (worldParts doc)

        selected =
            Document.mainPlacements doc
                |> List.filter (\p -> Set.member p.id selection && not (isBaseplatePlacement p))
                |> List.concatMap (worldParts doc)

        targetBox =
            unionBoxes
                (List.map worldBox
                    (if List.isEmpty selected then
                        everything

                     else
                        selected
                    )
                )

        target =
            if List.isEmpty selected then
                vec3 0 0 0

            else
                boxCentre targetBox

        newPart =
            localInfo partFile
    in
    if List.isEmpty newPart.antiStuds then
        restOn
            (if List.isEmpty selected then
                everything

             else
                selected
            )
            newPart
            target

    else
        case studSpot everything newPart target of
            Just translation ->
                Mat4.makeTranslate translation

            Nothing ->
                restOn everything newPart target


restOn : List WorldPart -> LocalInfo -> Vec3 -> Mat4
restOn below info target =
    let
        rawX =
            snap20 (Vec3.getX target)

        rawZ =
            snap20 (Vec3.getZ target)

        x =
            clamp (-Bounds.lotHalfLdu - Vec3.getX info.box.min) (Bounds.lotHalfLdu - Vec3.getX info.box.max) rawX
                |> snap20

        z =
            clamp (-Bounds.lotHalfLdu - Vec3.getZ info.box.min) (Bounds.lotHalfLdu - Vec3.getZ info.box.max) rawZ
                |> snap20

        footprint =
            shiftBox (vec3 x 0 z) info.box

        belowBoxes =
            List.filterMap worldBox below

        under =
            List.filter (\b -> overlapsAcross footprint b) belowBoxes

        topY =
            (if List.isEmpty under then
                belowBoxes

             else
                under
            )
                |> List.map (.min >> Vec3.getY)
                |> List.minimum
                |> Maybe.withDefault 0

        candidateY =
            topY + studHeight - Vec3.getY info.box.max
    in
    Mat4.makeTranslate3 x (max (-Bounds.maxHeightLdu - Vec3.getY info.box.min) candidateY) z


snap20 : Float -> Float
snap20 v =
    toFloat (round (v / 20)) * 20


studHeight : Float
studHeight =
    4


studSpot : List WorldPart -> LocalInfo -> Vec3 -> Maybe Vec3
studSpot everything info target =
    let
        covered =
            everything
                |> List.concatMap (worldPoints AntiStud)
                |> List.map (.position >> key)
                |> Set.fromList

        free =
            everything
                |> List.concatMap (worldPoints Stud)
                |> List.filter (\s -> Vec3.getY s.normal < -0.7 && not (Set.member (key s.position) covered))

        freeKeys =
            free |> List.map (.position >> key) |> Set.fromList

        boxes =
            List.map worldBox everything |> List.filterMap identity

        candidates =
            free
                |> List.concatMap (\s -> List.map (\a -> Vec3.sub s.position a) info.antiStuds)
                |> List.map (\t -> ( key t, t ))
                |> Dict.fromList
                |> Dict.values

        withinLot translation =
            let
                b =
                    shiftBox translation info.box
            in
            Vec3.getX b.min
                >= -Bounds.lotHalfLdu
                - 0.5
                && Vec3.getX b.max
                <= Bounds.lotHalfLdu
                + 0.5
                && Vec3.getZ b.min
                >= -Bounds.lotHalfLdu
                - 0.5
                && Vec3.getZ b.max
                <= Bounds.lotHalfLdu
                + 0.5
                && Vec3.getY b.min
                >= -Bounds.maxHeightLdu
                - 4.5
                && Vec3.getY b.max
                <= Bounds.baseplateBottomY
                + 0.5

        matched translation =
            List.length (List.filter (\a -> Set.member (key (Vec3.add a translation)) freeKeys) info.antiStuds)

        clear translation =
            withinLot translation && not (List.any (overlaps (shiftBox translation info.box)) boxes)
    in
    candidates
        |> List.filter clear
        |> List.map (\t -> ( ( -(matched t), round (horizontalDistance t target), Vec3.getY t ), t ))
        |> List.sortBy Tuple.first
        |> List.head
        |> Maybe.map Tuple.second


horizontalDistance : Vec3 -> Vec3 -> Float
horizontalDistance a b =
    sqrt ((Vec3.getX a - Vec3.getX b) ^ 2 + (Vec3.getZ a - Vec3.getZ b) ^ 2)


key : Vec3 -> ( Int, Int, Int )
key v =
    ( round (Vec3.getX v), round (Vec3.getY v), round (Vec3.getZ v) )


worldParts : Document -> Placement -> List WorldPart
worldParts doc placement =
    worldPartsHelp doc 0 Mat4.identity placement


worldPartsHelp : Document -> Int -> Mat4 -> Placement -> List WorldPart
worldPartsHelp doc depth parent placement =
    let
        matrix =
            Mat4.mul parent placement.matrix
    in
    if depth < 8 && Document.isSubmodelReference placement doc then
        Document.placementsOf placement.part doc
            |> List.concatMap (worldPartsHelp doc (depth + 1) matrix)

    else
        [ { part = placement.part, matrix = matrix } ]


type alias LocalInfo =
    { antiStuds : List Vec3
    , box : BoundingBox3D
    }


localInfo : String -> LocalInfo
localInfo partFile =
    case Connectivity.knownCatalogPart partFile of
        Just conn ->
            { antiStuds =
                conn.connections
                    |> List.filter (\c -> c.connType == AntiStud && Vec3.getY c.normal > 0.7)
                    |> List.map .localPosition
            , box =
                unionBoxes (List.map Just conn.boundingBoxes)
                    |> Maybe.withDefault defaultBox
            }

        Nothing ->
            { antiStuds = [], box = defaultBox }


defaultBox : BoundingBox3D
defaultBox =
    { min = vec3 -10 -4 -10, max = vec3 10 24 10 }


worldPoints : ConnectionType -> WorldPart -> List { position : Vec3, normal : Vec3 }
worldPoints kind wp =
    case Connectivity.knownCatalogPart wp.part of
        Just conn ->
            conn.connections
                |> List.filter (\c -> c.connType == kind)
                |> List.map
                    (\c ->
                        let
                            position =
                                Mat4.transform wp.matrix c.localPosition
                        in
                        { position = position
                        , normal = Vec3.sub (Mat4.transform wp.matrix (Vec3.add c.localPosition c.normal)) position
                        }
                    )

        Nothing ->
            []


worldBox : WorldPart -> Maybe BoundingBox3D
worldBox wp =
    let
        box =
            (localInfo wp.part).box

        corners =
            [ vec3 (Vec3.getX box.min) (Vec3.getY box.min) (Vec3.getZ box.min)
            , vec3 (Vec3.getX box.max) (Vec3.getY box.min) (Vec3.getZ box.min)
            , vec3 (Vec3.getX box.min) (Vec3.getY box.max) (Vec3.getZ box.min)
            , vec3 (Vec3.getX box.min) (Vec3.getY box.min) (Vec3.getZ box.max)
            , vec3 (Vec3.getX box.max) (Vec3.getY box.max) (Vec3.getZ box.min)
            , vec3 (Vec3.getX box.max) (Vec3.getY box.min) (Vec3.getZ box.max)
            , vec3 (Vec3.getX box.min) (Vec3.getY box.max) (Vec3.getZ box.max)
            , vec3 (Vec3.getX box.max) (Vec3.getY box.max) (Vec3.getZ box.max)
            ]
                |> List.map (Mat4.transform wp.matrix)
    in
    boundsOf corners


boundsOf : List Vec3 -> Maybe BoundingBox3D
boundsOf points =
    case points of
        [] ->
            Nothing

        first :: rest ->
            Just
                (List.foldl
                    (\p acc ->
                        { min = vec3 (min (Vec3.getX p) (Vec3.getX acc.min)) (min (Vec3.getY p) (Vec3.getY acc.min)) (min (Vec3.getZ p) (Vec3.getZ acc.min))
                        , max = vec3 (max (Vec3.getX p) (Vec3.getX acc.max)) (max (Vec3.getY p) (Vec3.getY acc.max)) (max (Vec3.getZ p) (Vec3.getZ acc.max))
                        }
                    )
                    { min = first, max = first }
                    rest
                )


unionBoxes : List (Maybe BoundingBox3D) -> Maybe BoundingBox3D
unionBoxes boxes =
    boxes
        |> List.filterMap identity
        |> List.concatMap (\b -> [ b.min, b.max ])
        |> boundsOf


boxCentre : Maybe BoundingBox3D -> Vec3
boxCentre box =
    case box of
        Just b ->
            Vec3.scale 0.5 (Vec3.add b.min b.max)

        Nothing ->
            vec3 0 0 0


shiftBox : Vec3 -> BoundingBox3D -> BoundingBox3D
shiftBox t b =
    { min = Vec3.add b.min t, max = Vec3.add b.max t }


overlapsAcross : BoundingBox3D -> BoundingBox3D -> Bool
overlapsAcross a b =
    let
        apart getter =
            getter a.max - 2 <= getter b.min || getter b.max - 2 <= getter a.min
    in
    not (apart Vec3.getX || apart Vec3.getZ)


overlaps : BoundingBox3D -> BoundingBox3D -> Bool
overlaps a b =
    let
        apart getter slack =
            getter a.max - slack <= getter b.min || getter b.max - slack <= getter a.min
    in
    not (apart Vec3.getX 2 || apart Vec3.getY (studHeight + 1) || apart Vec3.getZ 2)
