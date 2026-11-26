module City.I18n exposing
    ( Lang
    , actionBuildLot
    , actionClearLot
    , actionCopyLot
    , actionExportJson
    , actionExportMpd
    , actionImportJson
    , actionNewCity
    , actionPasteLot
    , actionRotateLot
    , actionStarterTown
    , boundsIssueText
    , buildersHeading
    , cancelButton
    , categoryLabel
    , cityNameLabel
    , clearConfirmHint
    , connectedLabel
    , connectingLabel
    , copyInviteLink
    , disconnectedLabel
    , editorBoundsOk
    , editorHeightLabel
    , editorKeepBaseText
    , editorLimitsTab
    , editorOpenTab
    , editorPartsLabel
    , editorTemplatesTab
    , emptyLotHint
    , emptyLotTitle
    , fitViewLabel
    , fromFlag
    , gridSizeLabel
    , heightUnit
    , hostRoomButton
    , joinRoomButton
    , joinRoomPlaceholder
    , leaveRoomButton
    , lotLabel
    , modeOrbitLabel
    , modeWalkLabel
    , muteLabel
    , pasteBuildHint
    , pasteButtonLabel
    , pasteEmptyError
    , quickTemplatesLabel
    , roomCopiedToast
    , roomHeading
    , saveLotButton
    , searchPartsPlaceholder
    , statBuildings
    , statGreen
    , statPeak
    , statPopulation
    , statRoads
    , statScore
    , templateLabel
    , title
    , tr
    , uploadLdrawLabel
    , walkHint
    , youTag
    )

{-| Finnish and English localization for Brick City (`brick-city`).
Finnish is the default unless the `language` flag begins with `"en"`, matching
all other Brick Family applications.
-}

import City.Types exposing (BoundsIssue(..), ElementCategory(..))
import I18n exposing (Language(..))


{-| Alias for the shared `I18n.Language`.
-}
type alias Lang =
    Language


{-| Resolve the language flag passed at startup.
-}
fromFlag : String -> Language
fromFlag raw =
    if String.startsWith "en" (String.toLower (String.trim raw)) then
        En

    else
        Fi


{-| Pick English or Finnish string according to `lang`.
-}
tr : Language -> String -> String -> String
tr lang en fi =
    case lang of
        En ->
            en

        Fi ->
            fi


{-| Application title.
-}
title : Language -> String
title lang =
    tr lang "Brick City" "Palikkakaupunki"


{-| Aria/title label for the mute toggle.
-}
muteLabel : Language -> Bool -> String
muteLabel lang muted =
    if muted then
        tr lang "Unmute audio" "Äänet päälle"

    else
        tr lang "Mute audio" "Mykistä äänet"


{-| City name input label.
-}
cityNameLabel : Language -> String
cityNameLabel lang =
    tr lang "City name" "Kaupungin nimi"


{-| Grid size control label.
-}
gridSizeLabel : Language -> String
gridSizeLabel lang =
    tr lang "Grid" "Ruudukko"


{-| Stat label: Population.
-}
statPopulation : Language -> String
statPopulation lang =
    tr lang "Pop." "Asukkaita"


{-| Stat label: Buildings.
-}
statBuildings : Language -> String
statBuildings lang =
    tr lang "Buildings" "Rakennuksia"


{-| Stat label: Green space percentage.
-}
statGreen : Language -> String
statGreen lang =
    tr lang "Green" "Vihreää"


{-| Stat label: Road links.
-}
statRoads : Language -> String
statRoads lang =
    tr lang "Road links" "Tieyhteydet"


{-| Stat label: Skyline peak.
-}
statPeak : Language -> String
statPeak lang =
    tr lang "Peak" "Huippu"


{-| Stat label: City score.
-}
statScore : Language -> String
statScore lang =
    tr lang "Score" "Pisteet"


{-| Camera mode label: Orbit.
-}
modeOrbitLabel : Language -> String
modeOrbitLabel lang =
    tr lang "Orbit" "Kierto"


{-| Camera mode label: Street Walk.
-}
modeWalkLabel : Language -> String
modeWalkLabel lang =
    tr lang "Street Walk" "Katukävely"


{-| Hint shown during Street Walk mode.
-}
walkHint : Language -> String
walkHint lang =
    tr lang "WASD / Arrows to walk street level • Drag to look around" "WASD / Nuolinäppäimet kävelyyn • Vedä katsellaksesi ympärille"


{-| Fit view tooltip.
-}
fitViewLabel : Language -> String
fitViewLabel lang =
    tr lang "Reset camera view" "Keskitä kamera"


{-| Lot coordinate heading.
-}
lotLabel : Language -> Int -> Int -> String
lotLabel lang col row =
    tr lang
        ("Lot (" ++ String.fromInt (col + 1) ++ ", " ++ String.fromInt (row + 1) ++ ")")
        ("Tontti (" ++ String.fromInt (col + 1) ++ ", " ++ String.fromInt (row + 1) ++ ")")


{-| Empty lot title.
-}
emptyLotTitle : Language -> String
emptyLotTitle lang =
    tr lang "Open 8×8 Lot" "Vapaa 8×8-tontti"


{-| Empty lot instruction.
-}
emptyLotHint : Language -> String
emptyLotHint lang =
    tr lang "Pick a starter block below or open the 3D builder." "Valitse aloituspala alta tai avaa 3D-rakennustyökalu."


{-| Button label: Edit lot in 3D builder.
-}
actionBuildLot : Language -> String
actionBuildLot lang =
    tr lang "Build in 3D" "Rakenna 3D:ssä"


{-| Button label: Rotate 90 degrees.
-}
actionRotateLot : Language -> String
actionRotateLot lang =
    tr lang "Rotate 90°" "Käännä 90°"


{-| Button label: Copy lot.
-}
actionCopyLot : Language -> String
actionCopyLot lang =
    tr lang "Copy" "Kopioi"


{-| Button label: Paste lot.
-}
actionPasteLot : Language -> String
actionPasteLot lang =
    tr lang "Paste" "Liitä"


{-| Button label: Clear lot.
-}
actionClearLot : Language -> String
actionClearLot lang =
    tr lang "Clear" "Tyhjennä"


{-| Label for quick starter templates list.
-}
quickTemplatesLabel : Language -> String
quickTemplatesLabel lang =
    tr lang "Starter 8×8 Blocks" "8×8-aloituspalat"


{-| Menu action: New blank city.
-}
actionNewCity : Language -> String
actionNewCity lang =
    tr lang "Empty City" "Tyhjä kaupunki"


{-| Menu action: Load starter town.
-}
actionStarterTown : Language -> String
actionStarterTown lang =
    tr lang "Starter Town" "Aloituskaupunki"


{-| Menu action: Export MPD.
-}
actionExportMpd : Language -> String
actionExportMpd lang =
    tr lang "Export .mpd" "Vie .mpd"


{-| Menu action: Export JSON.
-}
actionExportJson : Language -> String
actionExportJson lang =
    tr lang "Export JSON" "Vie JSON"


{-| Menu action: Import JSON.
-}
actionImportJson : Language -> String
actionImportJson lang =
    tr lang "Import JSON" "Tuo JSON"


{-| Multiplayer room section heading.
-}
roomHeading : Language -> String
roomHeading lang =
    tr lang "Collab City" "Yhteiskaupunki"


{-| Host online room button.
-}
hostRoomButton : Language -> String
hostRoomButton lang =
    tr lang "Host Room" "Luo huone"


{-| Join room button.
-}
joinRoomButton : Language -> String
joinRoomButton lang =
    tr lang "Join" "Liity"


{-| Join room code input placeholder.
-}
joinRoomPlaceholder : Language -> String
joinRoomPlaceholder lang =
    tr lang "Room code" "Huonekoodi"


{-| Leave room button.
-}
leaveRoomButton : Language -> String
leaveRoomButton lang =
    tr lang "Leave" "Poistu"


{-| Copy room invite link button.
-}
copyInviteLink : Language -> String
copyInviteLink lang =
    tr lang "Copy link" "Kopioi linkki"


{-| Toast when room invite link is copied.
-}
roomCopiedToast : Language -> String -> String
roomCopiedToast lang roomId =
    tr lang ("Copied invite link for room " ++ roomId) ("Huoneen " ++ roomId ++ " kutsulinkki kopioitu")


{-| Connected builders heading.
-}
buildersHeading : Language -> String
buildersHeading lang =
    tr lang "Builders" "Rakentajat"


{-| Room status: Connected.
-}
connectedLabel : Language -> String
connectedLabel lang =
    tr lang "Live" "Yhdistetty"


{-| Room status: Connecting.
-}
connectingLabel : Language -> String
connectingLabel lang =
    tr lang "Connecting…" "Yhdistetään…"


{-| Room status: Disconnected.
-}
disconnectedLabel : Language -> String
disconnectedLabel lang =
    tr lang "Solo" "Paikallinen"


{-| Self suffix on peer list.
-}
youTag : Language -> String
youTag lang =
    tr lang "you" "sinä"


{-| Element category human-readable label.
-}
categoryLabel : Language -> ElementCategory -> String
categoryLabel lang category =
    case category of
        Residential ->
            tr lang "Residential" "Asuminen"

        Commercial ->
            tr lang "Commercial" "Liike"

        Civic ->
            tr lang "Civic" "Julkinen"

        Park ->
            tr lang "Park" "Puisto"

        Road ->
            tr lang "Road" "Tie"

        Industrial ->
            tr lang "Industrial" "Teollisuus"

        Waterfront ->
            tr lang "Waterfront" "Ranta"

        Custom ->
            tr lang "Custom" "Oma malli"


{-| Height unit label (plates).
-}
heightUnit : Language -> Int -> String
heightUnit lang h =
    tr lang (String.fromInt h ++ "/32 pl") (String.fromInt h ++ "/32 tasoa")


{-| Template label prefix.
-}
templateLabel : Language -> String
templateLabel lang =
    tr lang "Template" "Pohja"


{-| Editor save & return button tooltip.
-}
saveLotButton : Language -> String
saveLotButton lang =
    tr lang "Save lot to city" "Tallenna tontti kaupunkiin"


{-| Editor cancel button tooltip.
-}
cancelButton : Language -> String
cancelButton lang =
    tr lang "Back without saving" "Palaa tallentamatta"


{-| Editor tab: Templates.
-}
editorTemplatesTab : Language -> String
editorTemplatesTab lang =
    tr lang "8×8 Templates" "8×8-pohjat"


{-| Editor tab: Import/Export build link or LDraw.
-}
editorOpenTab : Language -> String
editorOpenTab lang =
    tr lang "Open / Share" "Avaa / Jaa"


{-| Editor tab: Lot limits & health.
-}
editorLimitsTab : Language -> String
editorLimitsTab lang =
    tr lang "Lot Limits" "Tontin rajat"


{-| Editor search input placeholder.
-}
searchPartsPlaceholder : Language -> String
searchPartsPlaceholder lang =
    tr lang "Search microscale parts…" "Hae pienoispaloja…"


{-| Editor baseplate lock message.
-}
editorKeepBaseText : Language -> String
editorKeepBaseText lang =
    tr lang "The 8×8 baseplate stays anchored at the lot center." "8×8-pohjalevy pysyy lukittuna tontin keskellä."


{-| Editor live height label.
-}
editorHeightLabel : Language -> Int -> String
editorHeightLabel lang h =
    tr lang ("Height: " ++ String.fromInt h ++ " / 32 plates") ("Korkeus: " ++ String.fromInt h ++ " / 32 levyä")


{-| Editor part count label.
-}
editorPartsLabel : Language -> Int -> String
editorPartsLabel lang count =
    tr lang ("Parts: " ++ String.fromInt count ++ " / 400") ("Paloja: " ++ String.fromInt count ++ " / 400")


{-| Editor validation OK text.
-}
editorBoundsOk : Language -> String
editorBoundsOk lang =
    tr lang "Inside 8×8 lot and height 32 limit." "Mahtuu 8×8-tontille ja alle 32 tason rajaan."


{-| Error message for a bounds validation failure.
-}
boundsIssueText : Language -> BoundsIssue -> String
boundsIssueText lang issue =
    case issue of
        EmptyElement ->
            tr lang "Lot has no bricks yet." "Tontilla ei ole vielä palikoita."

        TooManyParts count ->
            tr lang ("Too many parts (" ++ String.fromInt count ++ ", max 400).") ("Liian monta palaa (" ++ String.fromInt count ++ ", enintään 400).")

        OutOfBoundsX _ _ ->
            tr lang "Exceeds 8×8 lot width (-80..80 LDU in X)." "Ylittää 8×8-tontin leveyden (X-suunta)."

        OutOfBoundsZ _ _ ->
            tr lang "Exceeds 8×8 lot depth (-80..80 LDU in Z)." "Ylittää 8×8-tontin syvyyden (Z-suunta)."

        ExceedsMaxHeight h ->
            tr lang ("Exceeds max height 32 (" ++ String.fromInt h ++ " plates).") ("Ylittää 32 tason maksimikorkeuden (" ++ String.fromInt h ++ " tasoa).")

        BelowBaseplate _ ->
            tr lang "Part extends below the baseplate." "Pala ulottuu pohjalevyn alapuolelle."


{-| Paste build link prompt.
-}
pasteBuildHint : Language -> String
pasteBuildHint lang =
    tr lang "Paste a #b/... build link or .ldr text:" "Liitä #b/...-rakennuslinkki tai .ldr-teksti:"


{-| Paste empty validation message.
-}
pasteEmptyError : Language -> String
pasteEmptyError lang =
    tr lang "Paste a build link or LDraw text first." "Liitä rakennuslinkki tai LDraw-teksti ensin."


{-| Paste submit button label.
-}
pasteButtonLabel : Language -> String
pasteButtonLabel lang =
    tr lang "Load into Lot" "Lataa tontille"


{-| Upload LDraw file button label.
-}
uploadLdrawLabel : Language -> String
uploadLdrawLabel lang =
    tr lang "Open .ldr / .mpd / .io file" "Avaa .ldr / .mpd / .io -tiedosto"


{-| Status message when a lot is cleared.
-}
clearConfirmHint : Language -> String
clearConfirmHint lang =
    tr lang "Cleared lot." "Tontti tyhjennetty."
