module City.I18n exposing
    ( Lang
    , actionBuildElement
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
    , boundsCeilingShort
    , boundsExceededShort
    , boundsIssueText
    , boundsSafeShort
    , builderNameLabel
    , buildersHeading
    , cancelButton
    , categoryLabel
    , cityFilesTab
    , cityNameLabel
    , cityNameSuggestions
    , claimHostButton
    , clearConfirmHint
    , collabMenuSubtitle
    , connectedLabel
    , connectingLabel
    , continueGameButton
    , copyInviteLink
    , defaultCityName
    , deselectLotLabel
    , disconnectedLabel
    , dragOrbitHint
    , editorBoundsOk
    , editorHeightLabel
    , editorKeepBaseText
    , editorLimitsTab
    , editorOpenTab
    , editorPartsLabel
    , editorTemplatesTab
    , emptyLotHint
    , emptyLotTitle
    , enterRoomCityButton
    , fitViewLabel
    , formatTimestamp
    , fromFlag
    , gridDimensionsHint
    , gridSizeLabel
    , heightMeterLabel
    , heightUnit
    , hostCollabRoomButton
    , hostOfflineBadge
    , hostOfflineDescription
    , hostOfflineHeading
    , hostRoomButton
    , joinRoomButton
    , joinRoomInputPlaceholder
    , joinRoomPlaceholder
    , leaveRoomButton
    , loadingBricksLabel
    , lotLabel
    , lotTemplatesToggle
    , menuButton
    , modeOrbitLabel
    , modeTopDownLabel
    , modeTopDownShort
    , modeWalkLabel
    , modeWalkShort
    , muteLabel
    , newCityHeading
    , partCountShort
    , pasteBuildHint
    , pasteButtonLabel
    , pasteEmptyError
    , presetDescription
    , presetTitle
    , previewNewLabel
    , previewSavedLabel
    , qrInviteHint
    , quickTemplatesLabel
    , roomCopiedToast
    , roomHeading
    , roomSharingTab
    , saveAndReturnButton
    , saveAndReturnShort
    , saveLotButton
    , savedCityBadge
    , savedLocallyLabel
    , searchPartsPlaceholder
    , selectLotHint
    , startBuildingButton
    , starterLayoutLabel
    , statBuildings
    , statGreen
    , statLots
    , statPeak
    , statPieces
    , statPopulation
    , statRoads
    , statScore
    , tabBricksLabel
    , tabPlatesTilesLabel
    , tabSnotSlopesLabel
    , tabTemplatesLabel
    , templateLabel
    , tidyUpBoundsError
    , tidyUpCleanLabel
    , tidyUpDoneNotice
    , tidyUpLabel
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

import City.Templates exposing (CityPreset(..))
import City.Types exposing (BoundsIssue(..), ElementCategory(..))
import I18n exposing (Language(..))
import Time exposing (Month(..))


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


{-| Stat label: Lots.
-}
statLots : Language -> String
statLots lang =
    tr lang "Lots" "Tontit"


{-| Stat label: Pieces.
-}
statPieces : Language -> String
statPieces lang =
    tr lang "Pieces" "Palat"


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


{-| Room status: Host offline / connection lost badge.
-}
hostOfflineBadge : Language -> String
hostOfflineBadge lang =
    tr lang "Host Offline" "Ei yhteyttä"


{-| Heading when collaborative room host is offline.
-}
hostOfflineHeading : Language -> String
hostOfflineHeading lang =
    tr lang "Room Host is Offline" "Huoneen isäntä ei ole linjoilla"


{-| Detailed explanation when room host is offline.
-}
hostOfflineDescription : Language -> String
hostOfflineDescription lang =
    tr lang
        "Connection to the host timed out or the creator left. You can become the new host to keep building, or return to your solo city."
        "Yhteys huoneen isäntään katkesi tai huoneen luoja poistui. Voit ryhtyä huoneen uudeksi isännäksi ja jatkaa rakentamista, tai poistua omaan kaupunkiisi."


{-| CTA button to claim host role for the room.
-}
claimHostButton : Language -> String
claimHostButton lang =
    tr lang "Become Room Host" "Ryhdy huoneen isännäksi"


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


{-| Short status label: 8x8 bounds safe.
-}
boundsSafeShort : Language -> String
boundsSafeShort lang =
    tr lang "8×8 Safe" "8×8 Kunnossa"


{-| Short status label: Near 32-plate height ceiling.
-}
boundsCeilingShort : Language -> String
boundsCeilingShort lang =
    tr lang "Near ceiling" "Katto lähellä"


{-| Short status label: Bounds exceeded.
-}
boundsExceededShort : Language -> String
boundsExceededShort lang =
    tr lang "Over limits" "Rajat ylitetty"


{-| Action label to deselect current lot.
-}
deselectLotLabel : Language -> String
deselectLotLabel lang =
    tr lang "Deselect" "Poista valinta"


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


{-| Primary CTA button inside the floating Lot Inspector card.
-}
actionBuildElement : Language -> Bool -> String
actionBuildElement lang isBuilt =
    if isBuilt then
        tr lang "Edit Element" "Rakenna / Muokkaa"

    else
        tr lang "Build Element" "Rakenna elementti"


{-| Toggle label for the starter blocks grid inside the Lot Inspector.
-}
lotTemplatesToggle : Language -> String
lotTemplatesToggle lang =
    tr lang "Starter Blocks" "Aloitusmallit"


{-| Camera mode label: Top-down overhead plan view.
-}
modeTopDownLabel : Language -> String
modeTopDownLabel lang =
    tr lang "Top-down" "Ylhäältä"


{-| Short label for Top-down toggle in the bottom bar.
-}
modeTopDownShort : Language -> String
modeTopDownShort lang =
    tr lang "Top" "Ylhäältä"


{-| Short label for Walk toggle in the bottom bar.
-}
modeWalkShort : Language -> String
modeWalkShort lang =
    tr lang "Walk" "Kävely"


{-| Modal tab label for City Layout & Export.
-}
cityFilesTab : Language -> String
cityFilesTab lang =
    tr lang "City & Files" "Kaupunki & tiedostot"


{-| Modal tab label for Room Sharing.
-}
roomSharingTab : Language -> String
roomSharingTab lang =
    tr lang "Room Sharing" "Yhteishuone"


{-| Subtle hint shown in bottom bar when no lot is selected.
-}
selectLotHint : Language -> String
selectLotHint lang =
    tr lang "Tap a lot to build" "Valitse tontti"


{-| Short part count summary for a lot.
-}
partCountShort : Language -> Int -> String
partCountShort lang count =
    tr lang (String.fromInt count ++ " pcs") (String.fromInt count ++ " palaa")


{-| Full label for the Save & Return button in `Pages.Build`.
-}
saveAndReturnButton : Language -> String
saveAndReturnButton lang =
    tr lang "Save & Return" "Tallenna ja palaa kaupunkiin"


{-| Compact mobile label for Save & Return in `Pages.Build`.
-}
saveAndReturnShort : Language -> String
saveAndReturnShort lang =
    tr lang "Save" "Tallenna"


{-| Visual height meter readout (`9 / 32 plates` or `9 / 32 kerrosta`).
-}
heightMeterLabel : Language -> Int -> String
heightMeterLabel lang h =
    tr lang (String.fromInt h ++ " / 32 plates") (String.fromInt h ++ " / 32 kerrosta")


{-| Dock tab label: Microscale Bricks.
-}
tabBricksLabel : Language -> String
tabBricksLabel lang =
    tr lang "Bricks" "Palikat"


{-| Dock tab label: Plates & Tiles.
-}
tabPlatesTilesLabel : Language -> String
tabPlatesTilesLabel lang =
    tr lang "Plates & Tiles" "Levyt & Laatat"


{-| Dock tab label: SNOT & Slopes.
-}
tabSnotSlopesLabel : Language -> String
tabSnotSlopesLabel lang =
    tr lang "SNOT & Slopes" "Viisteet & SNOT"


{-| Dock tab label: Starter Templates.
-}
tabTemplatesLabel : Language -> String
tabTemplatesLabel lang =
    tr lang "Templates" "Pohjat"


{-| Tidy Up action label.
-}
tidyUpLabel : Language -> String
tidyUpLabel lang =
    tr lang "Tidy Up" "Siisti"


{-| Notice shown after Tidy Up snaps loose or overlapping bricks.
-}
tidyUpDoneNotice : Language -> Int -> String
tidyUpDoneNotice lang count =
    tr lang
        ("Snapped " ++ String.fromInt count ++ " bricks neatly onto studs.")
        ("Siistitty " ++ String.fromInt count ++ " palaa paikoilleen.")


{-| Tooltip when the build has no loose or overlapping parts.
-}
tidyUpCleanLabel : Language -> String
tidyUpCleanLabel lang =
    tr lang "All bricks aligned" "Kaikki palat kohdallaan"


{-| Error notice if Tidy Up cannot keep all moves within 8x8 lot limits.
-}
tidyUpBoundsError : Language -> String
tidyUpBoundsError lang =
    tr lang "Could not move overlapping bricks inside the 8×8 lot." "Päällekkäisiä paloja ei voitu siirtää 8×8-tontin sisällä."


{-| Navbar button label to return to the Main Menu.
-}
menuButton : Language -> String
menuButton lang =
    tr lang "Menu" "Valikko"


{-| Default city name for new cities.
-}
defaultCityName : Language -> String
defaultCityName lang =
    tr lang "Brick Bay" "Palikkalahti"


{-| Suggested city names shown as quick chips on the Main Menu.
-}
cityNameSuggestions : Language -> List String
cityNameSuggestions lang =
    case lang of
        Fi ->
            [ "Palikkalahti", "Uusi kaupunki", "Palikkamäki" ]

        En ->
            [ "Brick Bay", "New City", "Palikka Harbor" ]


{-| Primary CTA button label to continue a saved city.
-}
continueGameButton : Language -> String
continueGameButton lang =
    tr lang "Continue City" "Jatka peliä"


{-| Overline badge on the saved-city card.
-}
savedCityBadge : Language -> String
savedCityBadge lang =
    tr lang "Saved City" "Tallennettu kaupunki"


{-| Fallback label when no wall-clock timestamp was saved yet.
-}
savedLocallyLabel : Language -> String
savedLocallyLabel lang =
    tr lang "Saved in browser" "Tallennettu selaimeen"


{-| Format a POSIX timestamp in the given time zone for FI/EN display.
-}
formatTimestamp : Language -> Time.Zone -> Time.Posix -> String
formatTimestamp lang zone posix =
    let
        year =
            String.fromInt (Time.toYear zone posix)

        month =
            pad2 (monthNumber (Time.toMonth zone posix))

        day =
            pad2 (Time.toDay zone posix)

        hour =
            pad2 (Time.toHour zone posix)

        minute =
            pad2 (Time.toMinute zone posix)
    in
    case lang of
        Fi ->
            day ++ "." ++ month ++ "." ++ year ++ " klo " ++ hour ++ "." ++ minute

        En ->
            year ++ "-" ++ month ++ "-" ++ day ++ " " ++ hour ++ ":" ++ minute


monthNumber : Month -> Int
monthNumber m =
    case m of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


pad2 : Int -> String
pad2 n =
    if n < 10 then
        "0" ++ String.fromInt n

    else
        String.fromInt n


{-| Heading for the New City setup panel.
-}
newCityHeading : Language -> String
newCityHeading lang =
    tr lang "New City" "Uusi kaupunki"


{-| Label for the starter layout preset selector.
-}
starterLayoutLabel : Language -> String
starterLayoutLabel lang =
    tr lang "Starter Layout" "Aloituspohja"


{-| Summary of lot count and stud dimensions for a given grid size.
-}
gridDimensionsHint : Language -> Int -> String
gridDimensionsHint lang size =
    let
        lots =
            size * size

        studs =
            size * 8
    in
    tr lang
        (String.fromInt lots ++ " lots • " ++ String.fromInt studs ++ "×" ++ String.fromInt studs ++ " studs")
        (String.fromInt lots ++ " tonttia • " ++ String.fromInt studs ++ "×" ++ String.fromInt studs ++ " nuppaa")


{-| Primary CTA button label to start building a new city.
-}
startBuildingButton : Language -> String
startBuildingButton lang =
    tr lang "Start Building" "Aloita rakentaminen"


{-| Display title for a starter city layout preset.
-}
presetTitle : Language -> CityPreset -> String
presetTitle lang preset =
    case preset of
        GridMetropolis ->
            tr lang "Grid Metropolis" "Ruutukaava"

        GreenValley ->
            tr lang "Green Valley" "Vihreä laakso"

        HarborTown ->
            tr lang "Coastal Bay" "Rantakaupunki"

        BlankCanvas ->
            tr lang "Sandbox Plain" "Tyhjä tonttimaa"


{-| Short description for a starter city layout preset.
-}
presetDescription : Language -> CityPreset -> String
presetDescription lang preset =
    case preset of
        GridMetropolis ->
            tr lang
                "Perimeter and cross roads with zoned lots"
                "Kehätie, risteykset ja kaavoitetut tontit"

        GreenValley ->
            tr lang
                "Central park and canal with green lots"
                "Keskuspuisto, kanaali ja vehreät tontit"

        HarborTown ->
            tr lang
                "Canal, roads, park, houses, and skyscraper"
                "Kanaali, tiet, puisto, talot ja pilvenpiirtäjä"

        BlankCanvas ->
            tr lang
                "Completely empty 8x8 building lots"
                "Täysin tyhjät 8x8-rakennustontit"


{-| Label for previewing the saved city on the 3D turntable.
-}
previewSavedLabel : Language -> String
previewSavedLabel lang =
    tr lang "Saved" "Tallennettu"


{-| Label for previewing the new city setup on the 3D turntable.
-}
previewNewLabel : Language -> String
previewNewLabel lang =
    tr lang "New Setup" "Uusi pohja"


{-| Subtle hint on the 3D turntable preview card.
-}
dragOrbitHint : Language -> String
dragOrbitHint lang =
    tr lang "Drag to orbit" "Käännä vetämällä"


{-| Part loading progress badge on the 3D preview.
-}
loadingBricksLabel : Language -> Int -> Int -> String
loadingBricksLabel lang remaining total =
    tr lang
        ("Loading bricks (" ++ String.fromInt (max 0 (total - remaining)) ++ "/" ++ String.fromInt total ++ ")…")
        ("Ladataan palikoita (" ++ String.fromInt (max 0 (total - remaining)) ++ "/" ++ String.fromInt total ++ ")…")


{-| Host button label on the Main Menu collaborative panel.
-}
hostCollabRoomButton : Language -> String
hostCollabRoomButton lang =
    tr lang "Host Collaborative Room" "Luo yhteishuone"


{-| Room join input placeholder accepting 4-letter code or full invite link.
-}
joinRoomInputPlaceholder : Language -> String
joinRoomInputPlaceholder lang =
    tr lang "4-letter code or paste link" "4+ merkin koodi tai linkki"


{-| Button to enter the 3D city view after creating a collaborative room.
-}
enterRoomCityButton : Language -> String
enterRoomCityButton lang =
    tr lang "Open Shared City" "Siirry kaupunkiin"


{-| Builder nickname input label.
-}
builderNameLabel : Language -> String
builderNameLabel lang =
    tr lang "Builder name" "Rakentajan nimi"


{-| Subtitle describing collaborative city building on the Main Menu.
-}
collabMenuSubtitle : Language -> String
collabMenuSubtitle lang =
    tr lang
        "Build the same 8×8 city grid together in real time with up to 8 builders."
        "Rakentakaa samaa 8×8-kaupunkia yhdessä reaaliajassa jopa 8 rakentajan kesken."


{-| QR code instruction inside collaborative lobby.
-}
qrInviteHint : Language -> String
qrInviteHint lang =
    tr lang "Scan QR code or share invite link" "Skannaa QR-koodi tai jaa kutsulinkki"
