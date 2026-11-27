module Main exposing (Flags, Model, Msg, isCityRouteHash, isMenuHash, main)

{-| Top-level Elm Architecture entry point for Brick City (`brick-city`).
Deployed to <https://kaupunki.palikkaharrastajat.fi/>.

Orchestrates:

  - Three top-level pages: Main Menu / New Game (`Pages.MainMenu`), the
    interactive 3D City View (`Pages.CityView`), and the 3D 8x8 Element Editor
    (`Pages.Build` built on `Studio.Core`).
  - Automatic `localStorage` persistence of the player's city (with saved
    timestamp), JSON export/import, and full multi-submodel `.mpd` export of the
    combined city.
  - Real-time collaborative city building over Cloudflare signaling rooms
    (`#/city/:roomId`) via `city-sync.js`.
  - LDraw part fetching & caching (`Game.PartLoading`) plus GPU mesh compilation
    (`City.Mesh`).

-}

import Browser
import Browser.Events
import City.Bounds as Bounds
import City.I18n as CityI18n
import City.Ingest as Ingest
import City.Mesh as Mesh exposing (MeshCache)
import City.Persistence as Persistence exposing (RoomUpdate(..))
import City.Render as Render
import City.Stats as Stats
import City.Templates as Templates exposing (CityPreset(..), Template)
import City.Types exposing (CityGrid, CityRoom, LotCoord, LotElement, RoomConnection(..), RoomRole(..), ViewMode(..), hashLdrawText)
import Dict
import EmbeddedParts
import Game.PartLoading as PartLoading
import Game.Ports as GamePorts
import Html exposing (Html)
import Http
import I18n exposing (Language(..))
import Json.Decode as Decode
import Json.Encode as Encode
import LDraw.Document as Document exposing (Document)
import LDraw.Parser as Parser
import LDraw.Resolve as Resolve exposing (PartCache, ResolverConfig)
import LDraw.Writer as Writer
import Pages.Build as Build
import Pages.CityView as CityView exposing (CityMenuModal(..))
import Pages.MainMenu as MainMenu exposing (PreviewSource(..), SavedCitySummary)
import Ports
import Render.Camera as Camera exposing (Camera, Navigation(..))
import Task
import Time
import UI.CameraGlobe as CameraGlobe


{-| Startup flags passed from `main.js`.
-}
type alias Flags =
    { savedAudioMuted : Bool
    , savedCityJson : String
    , savedCitySavedAt : Float
    , initialRoomId : String
    , ldrawBase : String
    , ldrawFallbackBase : String
    , initialHash : String
    , predecodedInitialBytes : Maybe (List Int)
    , language : String
    , supportsPointerEvents : Bool
    , baseUrl : String
    }


type CanvasDrag
    = NoCanvasDrag
    | Orbiting { startX : Float, startY : Float, travel : Float, pan : Bool }


{-| Top-level application state.
-}
type alias Model =
    { resolver : ResolverConfig
    , partCache : PartCache
    , meshes : MeshCache
    , city : CityGrid
    , savedCity : Maybe SavedCitySummary
    , camera : Camera
    , selectedLot : Maybe LotCoord
    , hoveredLot : Maybe LotCoord
    , clipboard : Maybe LotElement
    , showLotTemplates : Bool
    , canvasDrag : CanvasDrag
    , globeDrag : Maybe CameraGlobe.Drag
    , heldKeys : { w : Bool, a : Bool, s : Bool, d : Bool }
    , viewMode : ViewMode
    , builder : Maybe Build.Model
    , menuCityName : String
    , menuCityNameEdited : Bool
    , menuGridSize : Int
    , menuPreset : CityPreset
    , menuPreviewSource : PreviewSource
    , menuPreviewCity : CityGrid
    , menuCamera : Camera
    , menuDragging : Bool
    , timeZone : Time.Zone
    , shareCopiedTimer : Float
    , room : CityRoom
    , roomCodeInput : String
    , openModal : CityMenuModal
    , toast : Maybe ( String, Float )
    , audioMuted : Bool
    , language : Language
    , time : Float
    , timestampCounter : Int
    , viewportWidth : Int
    , viewportHeight : Int
    , ldrawBase : String
    , ldrawFallbackBase : String
    , supportsPointerEvents : Bool
    , baseUrl : String
    }


{-| Top-level application messages.
-}
type Msg
    = PartFetched String (Result Http.Error String)
    | ViewportResized { width : Int, height : Int }
    | AnimationTick Float
    | GotTimeZone Time.Zone
    | GotSavedTimestamp Time.Posix
    | HashChanged String
    | KeyDown String
    | KeyUp String
    | MenuCityNameChanged String
    | MenuPickGridSize Int
    | MenuPickPreset CityPreset
    | MenuSetPreviewSource PreviewSource
    | MenuCanvasMouseDown Float Float
    | MenuCanvasMouseMove Float Float
    | MenuCanvasMouseUp
    | MenuCanvasWheel Float
    | ContinueSavedCity
    | StartNewCity
    | ReturnToMainMenu
    | MenuHostRoom
    | EnterRoomCity
    | BuilderNameChanged String
    | SetLanguage Language
    | CityNameChanged String
    | ResizeGrid Int
    | CanvasPointerDown Float Float Int Bool
    | CanvasPointerMove Float Float
    | CanvasPointerUp Float Float
    | CanvasDoubleClick Float Float
    | CanvasWheel Float
    | GlobePress ( Float, Float )
    | GlobeMove ( Float, Float )
    | GlobeRelease
    | GlobeZoom Float
    | GlobeZoomDone
    | GlobeReset
    | SetCameraMode CityView.CameraMode
    | EditSelectedLot
    | RotateSelectedLot
    | CopySelectedLot
    | PasteSelectedLot
    | ClearSelectedLot
    | StampTemplate Template
    | ToggleLotTemplates
    | DeselectLot
    | CityViewNoOp
    | ToggleModal CityMenuModal
    | HostRoom
    | JoinRoom
    | LeaveRoom
    | RoomCodeInput String
    | CopyRoomLink
    | RoomStateChanged Decode.Value
    | RemoteSnapshotReceived Decode.Value
    | RemoteLotUpdated Decode.Value
    | RoomError String
    | LoadStarterCity
    | NewEmptyCity
    | ExportMpd
    | ExportJson
    | RequestImportJson
    | JsonImported String
    | ToggleLanguage
    | ToggleMute
    | BuildMsg Build.Msg


{-| Elm program entry point.
-}
main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{-| Return `True` when a URL hash explicitly targets the Main Menu (`""`, `"#"`,
`"#/"`, `"#menu"`, `"#/menu"`, or language-only hashes).
-}
isMenuHash : String -> Bool
isMenuHash rawHash =
    let
        clean =
            String.toLower (String.trim rawHash)
    in
    List.member clean [ "", "#", "#/", "#menu", "#/menu" ]
        || String.startsWith "#lang=" clean
        || String.startsWith "#/lang=" clean


{-| Return `True` when a URL hash targets the solo City View (`"#/city"`, `"#city"`,
`"#/play"`, or `"#play"` without a collaborative room suffix).
-}
isCityRouteHash : String -> Bool
isCityRouteHash rawHash =
    let
        clean =
            String.toLower (String.trim rawHash)
    in
    List.member clean [ "#city", "#/city", "#play", "#/play" ]


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        lang =
            CityI18n.fromFlag flags.language

        decodedSaved =
            if not (String.isEmpty (String.trim flags.savedCityJson)) then
                case Persistence.decodeCityJson flags.savedCityJson of
                    Ok loaded ->
                        let
                            savedPosix =
                                if flags.savedCitySavedAt > 0 then
                                    Just (Time.millisToPosix (round flags.savedCitySavedAt))

                                else
                                    Nothing
                        in
                        Just { city = loaded, savedAt = savedPosix }

                    Err _ ->
                        Nothing

            else
                Nothing

        defaultMenuName =
            CityI18n.defaultCityName lang

        defaultPreviewCity =
            Templates.cityFromPreset lang GridMetropolis defaultMenuName 4

        activeCity =
            case decodedSaved of
                Just saved ->
                    saved.city

                Nothing ->
                    Templates.defaultStarterCity lang

        hasRoomLink =
            not (String.isEmpty (String.trim flags.initialRoomId))

        initialViewMode =
            if hasRoomLink then
                CityPage

            else if isCityRouteHash flags.initialHash && decodedSaved /= Nothing then
                CityPage

            else
                MainMenuPage

        initialPreviewSource =
            case decodedSaved of
                Just _ ->
                    PreviewSaved

                Nothing ->
                    PreviewNew

        initialPreview =
            case initialPreviewSource of
                PreviewSaved ->
                    activeCity

                PreviewNew ->
                    defaultPreviewCity

        initialRoom =
            { roomId = flags.initialRoomId
            , role =
                if String.isEmpty flags.initialRoomId then
                    LocalSolo

                else
                    RoomGuest
            , status =
                if String.isEmpty flags.initialRoomId then
                    Disconnected

                else
                    Connecting
            , mySlot = 1
            , myName = "Rakentaja"
            , peers = []
            }

        baseModel =
            { resolver = PartLoading.resolverFromFlags flags
            , partCache = EmbeddedParts.cache
            , meshes = Mesh.empty
            , city = activeCity
            , savedCity = decodedSaved
            , camera = Render.resetCameraForCity activeCity 768
            , selectedLot = Just (LotCoord 1 1)
            , hoveredLot = Nothing
            , clipboard = Nothing
            , showLotTemplates = False
            , canvasDrag = NoCanvasDrag
            , globeDrag = Nothing
            , heldKeys = { w = False, a = False, s = False, d = False }
            , viewMode = initialViewMode
            , builder = Nothing
            , menuCityName = defaultMenuName
            , menuCityNameEdited = False
            , menuGridSize = 4
            , menuPreset = GridMetropolis
            , menuPreviewSource = initialPreviewSource
            , menuPreviewCity = defaultPreviewCity
            , menuCamera = Render.resetCameraForCity initialPreview 768
            , menuDragging = False
            , timeZone = Time.utc
            , shareCopiedTimer = 0
            , room = initialRoom
            , roomCodeInput = flags.initialRoomId
            , openModal = NoModal
            , toast = Nothing
            , audioMuted = flags.savedAudioMuted
            , language = lang
            , time = 0
            , timestampCounter = 100
            , viewportWidth = 1024
            , viewportHeight = 768
            , ldrawBase = flags.ldrawBase
            , ldrawFallbackBase = flags.ldrawFallbackBase
            , supportsPointerEvents = flags.supportsPointerEvents
            , baseUrl = flags.baseUrl
            }

        ( refreshed, fetchCmd ) =
            refreshCityMeshes baseModel
    in
    ( refreshed
    , Cmd.batch [ fetchCmd, Task.perform GotTimeZone Time.here ]
    )


displayedMenuCity : Model -> CityGrid
displayedMenuCity model =
    case ( model.menuPreviewSource, model.savedCity ) of
        ( PreviewSaved, Just saved ) ->
            saved.city

        _ ->
            model.menuPreviewCity


activeSceneCity : Model -> CityGrid
activeSceneCity model =
    case model.viewMode of
        MainMenuPage ->
            displayedMenuCity model

        _ ->
            model.city


refreshCityMeshes : Model -> ( Model, Cmd Msg )
refreshCityMeshes model =
    let
        res =
            Mesh.refreshMeshes (activeSceneCity model) model.partCache model.meshes
    in
    ( { model | meshes = res.meshes, partCache = res.cache }
    , PartLoading.fetchParts model.resolver PartFetched res.fetch
    )


persistLocalCity : Model -> ( Model, Cmd Msg )
persistLocalCity model =
    let
        existingSavedAt =
            model.savedCity |> Maybe.andThen .savedAt

        updated =
            { model | savedCity = Just { city = model.city, savedAt = existingSavedAt } }
    in
    ( updated
    , Cmd.batch
        [ Ports.saveCity (Persistence.encodeCityJson model.city)
        , Task.perform GotSavedTimestamp Time.now
        ]
    )


sfxCmd : String -> Cmd Msg
sfxCmd kind =
    Ports.playSfx (Encode.object [ ( "type", Encode.string kind ) ])


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PartFetched name result ->
            refreshCityMeshes
                { model
                    | partCache = Resolve.updateCache name result Parser.parseFile model.partCache
                }

        ViewportResized dims ->
            let
                cam =
                    Camera.setViewportHeight (toFloat dims.height) model.camera

                mCam =
                    Camera.setViewportHeight (toFloat dims.height) model.menuCamera

                sized =
                    { model
                        | viewportWidth = dims.width
                        , viewportHeight = dims.height
                        , camera = cam
                        , menuCamera = mCam
                    }
            in
            case model.builder of
                Just build ->
                    let
                        ( nextBuild, buildCmd ) =
                            Build.resize dims.width dims.height build
                    in
                    ( { sized | builder = Just nextBuild }, Cmd.map BuildMsg buildCmd )

                Nothing ->
                    ( sized, Cmd.none )

        AnimationTick dtMs ->
            let
                dt =
                    min 0.05 (dtMs / 1000.0)

                nextToast =
                    case model.toast of
                        Just ( txt, rem ) ->
                            if rem - dt <= 0 then
                                Nothing

                            else
                                Just ( txt, rem - dt )

                        Nothing ->
                            Nothing

                nextCopiedTimer =
                    max 0 (model.shareCopiedTimer - dt)

                nextCam =
                    if model.viewMode == CityPage && model.camera.navigation == Walk then
                        let
                            speed =
                                Camera.walkSpeed model.camera * dt

                            fwd =
                                (if model.heldKeys.w then
                                    1

                                 else
                                    0
                                )
                                    - (if model.heldKeys.s then
                                        1

                                       else
                                        0
                                      )

                            strafe =
                                (if model.heldKeys.d then
                                    1

                                 else
                                    0
                                )
                                    - (if model.heldKeys.a then
                                        1

                                       else
                                        0
                                      )
                        in
                        if fwd /= 0 || strafe /= 0 then
                            Camera.moveBy { forward = toFloat fwd * speed, right = toFloat strafe * speed, up = 0 } model.camera

                        else
                            model.camera

                    else
                        model.camera

                nextMenuCam =
                    if model.viewMode == MainMenuPage && not model.menuDragging then
                        let
                            mc =
                                model.menuCamera
                        in
                        { mc | azimuth = mc.azimuth + dt * 0.16 }

                    else
                        model.menuCamera
            in
            ( { model
                | time = model.time + dt
                , toast = nextToast
                , shareCopiedTimer = nextCopiedTimer
                , camera = nextCam
                , menuCamera = nextMenuCam
              }
            , Cmd.none
            )

        GotTimeZone tz ->
            ( { model | timeZone = tz }, Cmd.none )

        GotSavedTimestamp posix ->
            let
                nextSaved =
                    Maybe.map (\s -> { s | savedAt = Just posix }) model.savedCity
            in
            ( { model | savedCity = nextSaved }, Cmd.none )

        HashChanged rawHash ->
            if isMenuHash rawHash then
                let
                    nextPreviewSrc =
                        if model.savedCity /= Nothing then
                            PreviewSaved

                        else
                            PreviewNew

                    updated =
                        { model
                            | viewMode = MainMenuPage
                            , openModal = NoModal
                            , builder = Nothing
                            , menuPreviewSource = nextPreviewSrc
                        }

                    menuReset =
                        { updated | menuCamera = Render.resetCameraForCity (displayedMenuCity updated) updated.viewportHeight }
                in
                refreshCityMeshes menuReset

            else if isCityRouteHash rawHash then
                case model.savedCity of
                    Just saved ->
                        let
                            updated =
                                { model
                                    | viewMode = CityPage
                                    , city = saved.city
                                    , camera = Render.resetCameraForCity saved.city model.viewportHeight
                                    , selectedLot = defaultLotSelection saved.city
                                    , openModal = NoModal
                                }
                        in
                        refreshCityMeshes updated

                    Nothing ->
                        ( { model | viewMode = MainMenuPage }, Cmd.none )

            else
                ( model, Cmd.none )

        KeyDown key ->
            updateHeldKey True key model

        KeyUp key ->
            updateHeldKey False key model

        MenuCityNameChanged nextName ->
            let
                nextPreview =
                    Templates.cityFromPreset model.language model.menuPreset nextName model.menuGridSize
            in
            ( { model
                | menuCityName = nextName
                , menuCityNameEdited = True
                , menuPreviewSource = PreviewNew
                , menuPreviewCity = nextPreview
              }
            , Cmd.none
            )

        MenuPickGridSize size ->
            let
                clamped =
                    Bounds.clampGridSize size

                nextPreview =
                    Templates.cityFromPreset model.language model.menuPreset model.menuCityName clamped

                updated =
                    { model
                        | menuGridSize = clamped
                        , menuPreviewSource = PreviewNew
                        , menuPreviewCity = nextPreview
                        , menuCamera = Render.resetCameraForCity nextPreview model.viewportHeight
                    }

                ( refreshed, fetchCmd ) =
                    refreshCityMeshes updated
            in
            ( refreshed, Cmd.batch [ fetchCmd, sfxCmd "rotate" ] )

        MenuPickPreset preset ->
            let
                nextPreview =
                    Templates.cityFromPreset model.language preset model.menuCityName model.menuGridSize

                updated =
                    { model
                        | menuPreset = preset
                        , menuPreviewSource = PreviewNew
                        , menuPreviewCity = nextPreview
                        , menuCamera = Render.resetCameraForCity nextPreview model.viewportHeight
                    }

                ( refreshed, fetchCmd ) =
                    refreshCityMeshes updated
            in
            ( refreshed, Cmd.batch [ fetchCmd, sfxCmd "template" ] )

        MenuSetPreviewSource src ->
            let
                updated =
                    { model | menuPreviewSource = src }

                withCam =
                    { updated | menuCamera = Render.resetCameraForCity (displayedMenuCity updated) updated.viewportHeight }
            in
            refreshCityMeshes withCam

        MenuCanvasMouseDown x y ->
            ( { model
                | menuCamera = Camera.onMouseDown x y model.menuCamera
                , menuDragging = True
              }
            , Cmd.none
            )

        MenuCanvasMouseMove x y ->
            if model.menuDragging then
                ( { model | menuCamera = Camera.onMouseMove x y model.menuCamera }, Cmd.none )

            else
                ( model, Cmd.none )

        MenuCanvasMouseUp ->
            ( { model
                | menuCamera = Camera.onMouseUp model.menuCamera
                , menuDragging = False
              }
            , Cmd.none
            )

        MenuCanvasWheel dy ->
            ( { model | menuCamera = Camera.onWheel dy model.menuCamera }, Cmd.none )

        ContinueSavedCity ->
            case model.savedCity of
                Just saved ->
                    let
                        updated =
                            { model
                                | city = saved.city
                                , camera = Render.resetCameraForCity saved.city model.viewportHeight
                                , selectedLot = defaultLotSelection saved.city
                                , viewMode = CityPage
                                , openModal = NoModal
                            }

                        ( refreshed, fetchCmd ) =
                            refreshCityMeshes updated

                        hashCmd =
                            if refreshed.room.role == LocalSolo then
                                GamePorts.setUrlHash "#/city"

                            else
                                Cmd.none
                    in
                    ( refreshed, Cmd.batch [ fetchCmd, hashCmd, sfxCmd "join" ] )

                Nothing ->
                    update StartNewCity model

        StartNewCity ->
            let
                newCity =
                    Templates.cityFromPreset model.language model.menuPreset model.menuCityName model.menuGridSize

                leaveCmd =
                    if model.room.role /= LocalSolo then
                        Ports.leaveCityRoom ()

                    else
                        Cmd.none

                resetRoom =
                    { roomId = ""
                    , role = LocalSolo
                    , status = Disconnected
                    , mySlot = 1
                    , myName = model.room.myName
                    , peers = []
                    }

                updated =
                    { model
                        | city = newCity
                        , camera = Render.resetCameraForCity newCity model.viewportHeight
                        , selectedLot = defaultLotSelection newCity
                        , viewMode = CityPage
                        , openModal = NoModal
                        , room = resetRoom
                    }

                ( persisted, saveCmd ) =
                    persistLocalCity updated

                ( refreshed, fetchCmd ) =
                    refreshCityMeshes persisted
            in
            ( refreshed
            , Cmd.batch [ fetchCmd, saveCmd, leaveCmd, GamePorts.setUrlHash "#/city", sfxCmd "template" ]
            )

        ReturnToMainMenu ->
            let
                nextPreviewSrc =
                    if model.savedCity /= Nothing then
                        PreviewSaved

                    else
                        PreviewNew

                updated =
                    { model
                        | viewMode = MainMenuPage
                        , openModal = NoModal
                        , builder = Nothing
                        , menuPreviewSource = nextPreviewSrc
                    }

                withCam =
                    { updated | menuCamera = Render.resetCameraForCity (displayedMenuCity updated) updated.viewportHeight }

                ( refreshed, fetchCmd ) =
                    refreshCityMeshes withCam

                hashCmd =
                    if refreshed.room.role == LocalSolo then
                        GamePorts.setUrlHash "#menu"

                    else
                        Cmd.none
            in
            ( refreshed, Cmd.batch [ fetchCmd, hashCmd ] )

        MenuHostRoom ->
            let
                hostCity =
                    displayedMenuCity model

                cleanName =
                    cleanBuilderName model.room.myName

                oldRoom =
                    model.room

                updated =
                    { model
                        | city = hostCity
                        , camera = Render.resetCameraForCity hostCity model.viewportHeight
                        , selectedLot = defaultLotSelection hostCity
                        , openModal = NoModal
                        , room = { oldRoom | myName = cleanName }
                    }

                ( refreshed, fetchCmd ) =
                    refreshCityMeshes updated

                targetRoomId =
                    if model.room.role /= LocalSolo && not (String.isEmpty (String.trim model.room.roomId)) then
                        model.room.roomId

                    else
                        ""
            in
            ( refreshed
            , Cmd.batch
                [ fetchCmd
                , Ports.createCityRoom
                    { roomId = targetRoomId
                    , name = cleanName
                    , city = Persistence.encodeCityValue hostCity
                    }
                ]
            )

        EnterRoomCity ->
            let
                updated =
                    { model
                        | viewMode = CityPage
                        , camera = Render.resetCameraForCity model.city model.viewportHeight
                        , selectedLot = defaultLotSelection model.city
                        , openModal = NoModal
                    }
            in
            refreshCityMeshes updated

        BuilderNameChanged rawName ->
            let
                oldRoom =
                    model.room
            in
            ( { model | room = { oldRoom | myName = rawName } }, Cmd.none )

        SetLanguage nextLang ->
            applyLanguageChange nextLang model

        CityNameChanged nextName ->
            let
                oldCity =
                    model.city

                nextCity =
                    { oldCity | cityName = nextName, version = oldCity.version + 1 }

                ( persisted, saveCmd ) =
                    persistLocalCity { model | city = nextCity }
            in
            ( persisted
            , Cmd.batch [ saveCmd, syncFullIfInRoom persisted ]
            )

        ResizeGrid size ->
            let
                clamped =
                    Bounds.clampGridSize size

                oldCity =
                    model.city

                filteredLots =
                    Dict.filter (\( r, c ) _ -> r < clamped && c < clamped) oldCity.lots

                nextCity =
                    { oldCity | cols = clamped, rows = clamped, lots = filteredLots, version = oldCity.version + 1 }

                updated =
                    { model
                        | city = nextCity
                        , camera = Render.resetCameraForCity nextCity model.viewportHeight
                        , selectedLot = Nothing
                    }

                ( persisted, saveCmd ) =
                    persistLocalCity updated
            in
            ( persisted
            , Cmd.batch [ saveCmd, syncFullIfInRoom persisted, sfxCmd "rotate" ]
            )

        CanvasPointerDown x y btn shift ->
            let
                pan =
                    btn /= 0 || shift

                cam =
                    Camera.onMouseDown x y model.camera
            in
            ( { model
                | camera = cam
                , canvasDrag = Orbiting { startX = x, startY = y, travel = 0, pan = pan }
              }
            , Cmd.none
            )

        CanvasPointerMove x y ->
            case model.canvasDrag of
                Orbiting drag ->
                    let
                        ( lx, ly ) =
                            Maybe.withDefault ( x, y ) model.camera.lastMousePos

                        dx =
                            x - lx

                        dy =
                            y - ly

                        cam =
                            model.camera

                        nextCam =
                            if drag.pan then
                                Camera.onPan dx dy { cam | lastMousePos = Just ( x, y ) }

                            else
                                Camera.onMouseMove x y cam

                        nextTravel =
                            drag.travel + sqrt (dx * dx + dy * dy)
                    in
                    ( { model
                        | camera = nextCam
                        , canvasDrag = Orbiting { drag | travel = nextTravel }
                      }
                    , Cmd.none
                    )

                NoCanvasDrag ->
                    let
                        hovered =
                            Render.pickLotAt model.camera model.viewportWidth model.viewportHeight x y model.city
                    in
                    ( { model | hoveredLot = hovered }, Cmd.none )

        CanvasPointerUp x y ->
            let
                clicked =
                    case model.canvasDrag of
                        Orbiting drag ->
                            drag.travel < 6.0

                        NoCanvasDrag ->
                            False

                picked =
                    if clicked then
                        Render.pickLotAt model.camera model.viewportWidth model.viewportHeight x y model.city

                    else
                        model.selectedLot

                focusCmd =
                    if clicked then
                        Ports.sendLotFocus (Persistence.encodeLotFocus picked)

                    else
                        Cmd.none
            in
            ( { model
                | camera = Camera.onMouseUp model.camera
                , canvasDrag = NoCanvasDrag
                , selectedLot = picked
                , showLotTemplates =
                    if clicked && picked /= model.selectedLot then
                        False

                    else
                        model.showLotTemplates
              }
            , focusCmd
            )

        CanvasDoubleClick x y ->
            case Render.pickLotAt model.camera model.viewportWidth model.viewportHeight x y model.city of
                Just coord ->
                    openEditorForLot coord { model | selectedLot = Just coord }

                Nothing ->
                    ( model, Cmd.none )

        CanvasWheel dy ->
            let
                nextCam =
                    if model.camera.navigation == Walk then
                        Camera.dollyBy (-dy * 0.4) model.camera

                    else
                        Camera.onWheel dy model.camera
            in
            ( { model | camera = nextCam }, Cmd.none )

        GlobePress pt ->
            ( { model | globeDrag = Just (CameraGlobe.startDrag model.camera pt) }, Cmd.none )

        GlobeMove pt ->
            case model.globeDrag of
                Just drag ->
                    let
                        ( ( dAz, dEl ), nextDrag ) =
                            CameraGlobe.moveDrag drag pt
                    in
                    ( { model | camera = Camera.orbitBy dAz dEl model.camera, globeDrag = Just nextDrag }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        GlobeRelease ->
            case model.globeDrag of
                Just drag ->
                    let
                        nextCam =
                            case CameraGlobe.releaseDrag model.camera drag of
                                Just ( dAz, dEl ) ->
                                    Camera.orbitBy dAz dEl model.camera

                                Nothing ->
                                    model.camera
                    in
                    ( { model | camera = nextCam, globeDrag = Nothing }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        GlobeZoom level ->
            let
                cam =
                    model.camera

                dist =
                    CameraGlobe.wheelDeltaForZoom cam level
            in
            ( { model | camera = { cam | distance = dist } }, Cmd.none )

        GlobeZoomDone ->
            ( model, Cmd.none )

        GlobeReset ->
            ( { model | camera = Render.resetCameraForCity model.city model.viewportHeight }, Cmd.none )

        SetCameraMode mode ->
            let
                nextCam =
                    case mode of
                        CityView.OrbitMode ->
                            Render.resetCameraForCity model.city model.viewportHeight

                        CityView.WalkMode ->
                            if model.camera.navigation == Walk then
                                model.camera

                            else
                                Camera.enterWalk 0.0 model.camera

                        CityView.TopDownMode ->
                            Render.topDownCameraForCity model.city model.viewportHeight
            in
            ( { model | camera = nextCam }, Cmd.none )

        EditSelectedLot ->
            case model.selectedLot of
                Just coord ->
                    openEditorForLot coord model

                Nothing ->
                    ( model, Cmd.none )

        RotateSelectedLot ->
            case model.selectedLot of
                Just coord ->
                    case Dict.get ( coord.row, coord.col ) model.city.lots of
                        Just el ->
                            let
                                ts =
                                    model.timestampCounter + 1

                                nextEl =
                                    { el | rotation = modBy 4 (el.rotation + 1), updatedAt = ts, updatedBy = model.room.myName }
                            in
                            applyLocalLotChange coord (Just nextEl) { model | timestampCounter = ts } "rotate"

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CopySelectedLot ->
            case model.selectedLot of
                Just coord ->
                    case Dict.get ( coord.row, coord.col ) model.city.lots of
                        Just el ->
                            ( { model | clipboard = Just el, toast = Just ( CityI18n.tr model.language "Copied lot!" "Tontti kopioitu!", 2.5 ) }
                            , sfxCmd "snap"
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        PasteSelectedLot ->
            case ( model.selectedLot, model.clipboard ) of
                ( Just coord, Just clip ) ->
                    let
                        ts =
                            model.timestampCounter + 1

                        nextEl =
                            { clip
                                | id = clip.id ++ "-p" ++ String.fromInt ts
                                , updatedAt = ts
                                , updatedBy = model.room.myName
                            }
                    in
                    applyLocalLotChange coord (Just nextEl) { model | timestampCounter = ts } "snap"

                _ ->
                    ( model, Cmd.none )

        ClearSelectedLot ->
            case model.selectedLot of
                Just coord ->
                    applyLocalLotChange coord Nothing model "delete"

                Nothing ->
                    ( model, Cmd.none )

        StampTemplate tmpl ->
            case model.selectedLot of
                Just coord ->
                    let
                        ts =
                            model.timestampCounter + 1

                        el =
                            Templates.elementFromTemplate model.language ts model.room.myName 0 tmpl
                    in
                    applyLocalLotChange coord (Just el) { model | timestampCounter = ts } "template"

                Nothing ->
                    ( model, Cmd.none )

        ToggleLotTemplates ->
            ( { model | showLotTemplates = not model.showLotTemplates }, Cmd.none )

        DeselectLot ->
            ( { model | selectedLot = Nothing, showLotTemplates = False }, Ports.sendLotFocus Encode.null )

        CityViewNoOp ->
            ( model, Cmd.none )

        ToggleModal nextModal ->
            let
                toggled =
                    if model.openModal == nextModal then
                        NoModal

                    else
                        nextModal
            in
            ( { model | openModal = toggled }, Cmd.none )

        HostRoom ->
            let
                targetRoomId =
                    if model.room.role /= LocalSolo && not (String.isEmpty (String.trim model.room.roomId)) then
                        model.room.roomId

                    else
                        ""
            in
            ( { model | openModal = NoModal }
            , Ports.createCityRoom
                { roomId = targetRoomId
                , name = cleanBuilderName model.room.myName
                , city = Persistence.encodeCityValue model.city
                }
            )

        JoinRoom ->
            let
                code =
                    String.trim model.roomCodeInput
            in
            if String.isEmpty code then
                ( model, Cmd.none )

            else
                ( { model | openModal = NoModal }
                , Ports.joinCityRoom { roomId = code, name = cleanBuilderName model.room.myName }
                )

        LeaveRoom ->
            let
                hashCmd =
                    case model.viewMode of
                        MainMenuPage ->
                            GamePorts.setUrlHash "#menu"

                        _ ->
                            GamePorts.setUrlHash "#/city"
            in
            ( { model
                | room = { roomId = "", role = LocalSolo, status = Disconnected, mySlot = 1, myName = model.room.myName, peers = [] }
                , openModal = NoModal
              }
            , Cmd.batch [ Ports.leaveCityRoom (), hashCmd ]
            )

        RoomCodeInput raw ->
            ( { model | roomCodeInput = raw }, Cmd.none )

        CopyRoomLink ->
            let
                url =
                    MainMenu.makeCityRoomUrl model.baseUrl model.room.roomId
            in
            ( { model
                | toast = Just ( CityI18n.roomCopiedToast model.language model.room.roomId, 3.0 )
                , shareCopiedTimer = 2.5
              }
            , GamePorts.copyShareUrl url
            )

        RoomStateChanged raw ->
            case Persistence.decodeRoomState raw of
                InRoom room ->
                    let
                        justJoined =
                            model.room.status /= Connected && room.status == Connected

                        hashCmd =
                            if not (String.isEmpty (String.trim room.roomId)) && room.roomId /= model.room.roomId then
                                GamePorts.setUrlHash ("#/city/" ++ room.roomId)

                            else
                                Cmd.none
                    in
                    ( { model | room = room }
                    , Cmd.batch
                        [ hashCmd
                        , if justJoined then
                            sfxCmd "join"

                          else
                            Cmd.none
                        ]
                    )

                NoRoom ->
                    ( { model | room = { roomId = "", role = LocalSolo, status = Disconnected, mySlot = 1, myName = model.room.myName, peers = [] } }
                    , Cmd.none
                    )

                Unreadable ->
                    ( model, Cmd.none )

        RemoteSnapshotReceived raw ->
            case Decode.decodeValue Persistence.decodeCityValue raw of
                Ok nextCity ->
                    let
                        nextViewMode =
                            if model.viewMode == MainMenuPage && model.room.role == RoomGuest then
                                CityPage

                            else
                                model.viewMode

                        updated =
                            { model
                                | city = nextCity
                                , camera = Render.resetCameraForCity nextCity model.viewportHeight
                                , selectedLot = defaultLotSelection nextCity
                                , viewMode = nextViewMode
                            }

                        ( persisted, saveCmd ) =
                            persistLocalCity updated

                        ( refreshed, fetchCmd ) =
                            refreshCityMeshes persisted
                    in
                    ( refreshed, Cmd.batch [ fetchCmd, saveCmd ] )

                Err _ ->
                    ( model, Cmd.none )

        RemoteLotUpdated raw ->
            case Decode.decodeValue Persistence.decodeRemoteLotUpdate raw of
                Ok upd ->
                    let
                        validLot =
                            case upd.element of
                                Just el ->
                                    Bounds.isDocumentValid (Document.parse el.ldrawText)

                                Nothing ->
                                    True
                    in
                    if validLot then
                        let
                            oldCity =
                                model.city

                            nextLots =
                                case upd.element of
                                    Just el ->
                                        Dict.insert ( upd.row, upd.col ) el oldCity.lots

                                    Nothing ->
                                        Dict.remove ( upd.row, upd.col ) oldCity.lots

                            nextCity =
                                { oldCity | lots = nextLots, version = oldCity.version + 1 }

                            ( persisted, saveCmd ) =
                                persistLocalCity { model | city = nextCity }

                            ( refreshed, fetchCmd ) =
                                refreshCityMeshes persisted
                        in
                        ( refreshed, Cmd.batch [ fetchCmd, saveCmd ] )

                    else
                        ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        RoomError err ->
            ( { model | toast = Just ( err, 4.0 ) }, sfxCmd "error" )

        LoadStarterCity ->
            let
                nextCity =
                    Templates.defaultStarterCity model.language

                updated =
                    { model
                        | city = nextCity
                        , camera = Render.resetCameraForCity nextCity model.viewportHeight
                        , openModal = NoModal
                        , selectedLot = Just (LotCoord 1 1)
                    }

                ( persisted, saveCmd ) =
                    persistLocalCity updated

                ( refreshed, fetchCmd ) =
                    refreshCityMeshes persisted
            in
            ( refreshed, Cmd.batch [ fetchCmd, saveCmd, syncFullIfInRoom refreshed, sfxCmd "template" ] )

        NewEmptyCity ->
            let
                nextCity =
                    Templates.blankCity (CityI18n.title model.language) model.city.cols model.city.rows

                updated =
                    { model
                        | city = nextCity
                        , camera = Render.resetCameraForCity nextCity model.viewportHeight
                        , openModal = NoModal
                        , selectedLot = Just (LotCoord 0 0)
                    }

                ( persisted, saveCmd ) =
                    persistLocalCity updated

                ( refreshed, fetchCmd ) =
                    refreshCityMeshes persisted
            in
            ( refreshed, Cmd.batch [ fetchCmd, saveCmd, syncFullIfInRoom refreshed, sfxCmd "clear" ] )

        ExportMpd ->
            ( { model | openModal = NoModal, toast = Just ( CityI18n.tr model.language "Exported city .mpd" "Kaupunki viety .mpd-tiedostona", 3.0 ) }
            , Ports.downloadCityFile
                { filename = slugify model.city.cityName ++ ".mpd"
                , content = Ingest.exportCityMpd model.city
                , mimeType = "text/plain;charset=utf-8"
                }
            )

        ExportJson ->
            ( { model | openModal = NoModal, toast = Just ( CityI18n.tr model.language "Exported city JSON" "Kaupunki viety JSON-tiedostona", 3.0 ) }
            , Ports.downloadCityFile
                { filename = slugify model.city.cityName ++ ".json"
                , content = Persistence.encodeCityJson model.city
                , mimeType = "application/json;charset=utf-8"
                }
            )

        RequestImportJson ->
            ( { model | openModal = NoModal }, Ports.requestCityJsonImport () )

        JsonImported raw ->
            case Persistence.decodeCityJson raw of
                Ok imported ->
                    let
                        wasMenu =
                            model.viewMode == MainMenuPage

                        updated =
                            { model
                                | city = imported
                                , camera = Render.resetCameraForCity imported model.viewportHeight
                                , selectedLot = defaultLotSelection imported
                                , viewMode =
                                    if wasMenu then
                                        CityPage

                                    else
                                        model.viewMode
                                , toast = Just ( CityI18n.tr model.language "City imported!" "Kaupunki tuotu!", 3.0 )
                            }

                        ( persisted, saveCmd ) =
                            persistLocalCity updated

                        ( refreshed, fetchCmd ) =
                            refreshCityMeshes persisted

                        hashCmd =
                            if wasMenu && refreshed.room.role == LocalSolo then
                                GamePorts.setUrlHash "#/city"

                            else
                                Cmd.none
                    in
                    ( refreshed, Cmd.batch [ fetchCmd, saveCmd, syncFullIfInRoom refreshed, hashCmd, sfxCmd "save" ] )

                Err _ ->
                    ( { model | toast = Just ( CityI18n.tr model.language "Invalid city JSON file." "Virheellinen kaupunki-JSON.", 4.0 ) }
                    , sfxCmd "error"
                    )

        ToggleLanguage ->
            let
                nextLang =
                    case model.language of
                        Fi ->
                            En

                        En ->
                            Fi
            in
            applyLanguageChange nextLang model

        ToggleMute ->
            let
                nextMuted =
                    not model.audioMuted
            in
            ( { model | audioMuted = nextMuted }, GamePorts.setAudioMuted nextMuted )

        BuildMsg bMsg ->
            case model.builder of
                Just build ->
                    let
                        ( nextBuild, buildCmd, outcome ) =
                            Build.update bMsg build
                    in
                    case outcome of
                        Build.Stay ->
                            ( { model | builder = Just nextBuild }, Cmd.map BuildMsg buildCmd )

                        Build.Cancelled ->
                            ( { model | viewMode = CityPage, builder = Nothing }, Cmd.none )

                        Build.Saved saved ->
                            let
                                ts =
                                    model.timestampCounter + 1

                                ldrawStr =
                                    Writer.writeDocument saved.document

                                nextEl : LotElement
                                nextEl =
                                    { id = "lot-" ++ String.fromInt saved.coord.row ++ "-" ++ String.fromInt saved.coord.col ++ "-" ++ String.fromInt ts
                                    , name = saved.name
                                    , category = Stats.inferCategory saved.document saved.heightPlates
                                    , ldrawText = ldrawStr
                                    , document = saved.document
                                    , contentHash = hashLdrawText ldrawStr
                                    , templateId = saved.templateId
                                    , rotation = 0
                                    , updatedAt = ts
                                    , updatedBy = model.room.myName
                                    , partCount = saved.partCount
                                    , heightPlates = saved.heightPlates
                                    }

                                ( afterLot, lotCmd ) =
                                    applyLocalLotChange saved.coord (Just nextEl) { model | viewMode = CityPage, builder = Nothing, timestampCounter = ts } "save"
                            in
                            ( afterLot, Cmd.batch [ Cmd.map BuildMsg buildCmd, lotCmd ] )

                Nothing ->
                    ( model, Cmd.none )


applyLanguageChange : Language -> Model -> ( Model, Cmd Msg )
applyLanguageChange nextLang model =
    let
        langCode =
            case nextLang of
                Fi ->
                    "fi"

                En ->
                    "en"

        nextMenuCityName =
            if model.menuCityNameEdited then
                model.menuCityName

            else
                CityI18n.defaultCityName nextLang

        nextPreview =
            Templates.cityFromPreset nextLang model.menuPreset nextMenuCityName model.menuGridSize

        nextBuilder =
            Maybe.map (Build.setLanguage nextLang) model.builder
    in
    ( { model
        | language = nextLang
        , builder = nextBuilder
        , menuCityName = nextMenuCityName
        , menuPreviewCity = nextPreview
      }
    , GamePorts.storeLanguage langCode
    )


cleanBuilderName : String -> String
cleanBuilderName raw =
    let
        trimmed =
            String.trim raw
    in
    if String.isEmpty trimmed then
        "Rakentaja"

    else
        String.left 24 trimmed


defaultLotSelection : CityGrid -> Maybe LotCoord
defaultLotSelection city =
    if city.cols > 1 && city.rows > 1 && not (Dict.isEmpty city.lots) then
        Just (LotCoord 1 1)

    else
        Just (LotCoord 0 0)


openEditorForLot : LotCoord -> Model -> ( Model, Cmd Msg )
openEditorForLot coord model =
    let
        initial =
            case Dict.get ( coord.row, coord.col ) model.city.lots of
                Just el ->
                    { name = el.name
                    , category = el.category
                    , templateId = el.templateId
                    , document = Document.parse el.ldrawText
                    }

                Nothing ->
                    { name = CityI18n.lotLabel model.language coord.col coord.row
                    , category = City.Types.Custom
                    , templateId = Nothing
                    , document = Templates.emptyBaseplateDocument 71
                    }

        ( buildModel, buildCmd ) =
            Build.init
                { ldrawBase = model.ldrawBase
                , ldrawFallbackBase = model.ldrawFallbackBase
                , supportsPointerEvents = model.supportsPointerEvents
                , language = model.language
                , width = model.viewportWidth
                , height = model.viewportHeight
                }
                coord
                initial
    in
    ( { model | viewMode = LotEditorPage, builder = Just buildModel }
    , Cmd.batch [ Cmd.map BuildMsg buildCmd, Ports.sendLotFocus (Persistence.encodeLotFocus (Just coord)) ]
    )


applyLocalLotChange : LotCoord -> Maybe LotElement -> Model -> String -> ( Model, Cmd Msg )
applyLocalLotChange coord maybeEl model sfxName =
    let
        oldCity =
            model.city

        nextLots =
            case maybeEl of
                Just el ->
                    Dict.insert ( coord.row, coord.col ) el oldCity.lots

                Nothing ->
                    Dict.remove ( coord.row, coord.col ) oldCity.lots

        nextCity =
            { oldCity | lots = nextLots, version = oldCity.version + 1 }

        ( persisted, saveCmd ) =
            persistLocalCity { model | city = nextCity }

        ( refreshed, fetchCmd ) =
            refreshCityMeshes persisted

        syncCmd =
            if refreshed.room.role /= LocalSolo then
                Ports.sendLotUpdate (Persistence.encodeLotUpdate coord maybeEl)

            else
                Cmd.none
    in
    ( refreshed
    , Cmd.batch [ fetchCmd, saveCmd, syncCmd, sfxCmd sfxName ]
    )


syncFullIfInRoom : Model -> Cmd Msg
syncFullIfInRoom model =
    if model.room.role /= LocalSolo then
        Ports.sendCitySnapshot (Persistence.encodeCityValue model.city)

    else
        Cmd.none


updateHeldKey : Bool -> String -> Model -> ( Model, Cmd Msg )
updateHeldKey down rawKey model =
    let
        hk =
            model.heldKeys

        next =
            case String.toLower rawKey of
                "w" ->
                    { hk | w = down }

                "arrowup" ->
                    { hk | w = down }

                "s" ->
                    { hk | s = down }

                "arrowdown" ->
                    { hk | s = down }

                "a" ->
                    { hk | a = down }

                "arrowleft" ->
                    { hk | a = down }

                "d" ->
                    { hk | d = down }

                "arrowright" ->
                    { hk | d = down }

                _ ->
                    hk
    in
    ( { model | heldKeys = next }, Cmd.none )


slugify : String -> String
slugify raw =
    let
        clean =
            raw
                |> String.trim
                |> String.toLower
                |> String.map
                    (\ch ->
                        if Char.isAlphaNum ch then
                            ch

                        else
                            '-'
                    )
    in
    if String.isEmpty clean then
        "palikkakaupunki"

    else
        clean


view : Model -> Html Msg
view model =
    case ( model.viewMode, model.builder ) of
        ( MainMenuPage, _ ) ->
            MainMenu.view
                { language = model.language
                , audioMuted = model.audioMuted
                , savedCity = model.savedCity
                , previewSource = model.menuPreviewSource
                , previewCity = displayedMenuCity model
                , previewStats = Stats.computeStats (displayedMenuCity model)
                , meshes = model.meshes
                , camera = model.menuCamera
                , time = model.time
                , timeZone = model.timeZone
                , viewportWidth = model.viewportWidth
                , viewportHeight = model.viewportHeight
                , newCityName = model.menuCityName
                , gridSize = model.menuGridSize
                , preset = model.menuPreset
                , room = model.room
                , roomCodeInput = model.roomCodeInput
                , builderName = model.room.myName
                , baseUrl = model.baseUrl
                , shareCopiedNotice = model.shareCopiedTimer > 0
                , toast = Maybe.map Tuple.first model.toast
                }
                { onSetLanguage = SetLanguage
                , onToggleMute = ToggleMute
                , onContinueSaved = ContinueSavedCity
                , onCityNameChanged = MenuCityNameChanged
                , onSelectGridSize = MenuPickGridSize
                , onSelectPreset = MenuPickPreset
                , onSetPreviewSource = MenuSetPreviewSource
                , onStartNewCity = StartNewCity
                , onImportJson = RequestImportJson
                , onBuilderNameChanged = BuilderNameChanged
                , onHostRoom = MenuHostRoom
                , onRoomCodeChanged = RoomCodeInput
                , onJoinRoom = JoinRoom
                , onCopyRoomLink = CopyRoomLink
                , onEnterRoomCity = EnterRoomCity
                , onLeaveRoom = LeaveRoom
                , onMouseDown = MenuCanvasMouseDown
                , onMouseMove = MenuCanvasMouseMove
                , onMouseUp = MenuCanvasMouseUp
                , onWheel = MenuCanvasWheel
                }

        ( LotEditorPage, Just build ) ->
            Html.map BuildMsg (Build.view build)

        _ ->
            CityView.viewCity
                { city = model.city
                , stats = Stats.computeStats model.city
                , meshes = model.meshes
                , camera = model.camera
                , selectedLot = model.selectedLot
                , hoveredLot = model.hoveredLot
                , clipboard = model.clipboard
                , showLotTemplates = model.showLotTemplates
                , room = model.room
                , roomCodeInput = model.roomCodeInput
                , openModal = model.openModal
                , toast = Maybe.map Tuple.first model.toast
                , audioMuted = model.audioMuted
                , language = model.language
                , baseUrl = model.baseUrl
                , time = model.time
                , width = model.viewportWidth
                , height = model.viewportHeight
                , actions =
                    { onOpenMainMenu = ReturnToMainMenu
                    , onCityNameChanged = CityNameChanged
                    , onResizeGrid = ResizeGrid
                    , onPointerDown = CanvasPointerDown
                    , onPointerMove = CanvasPointerMove
                    , onPointerUp = CanvasPointerUp
                    , onDoubleClick = CanvasDoubleClick
                    , onWheel = CanvasWheel
                    , onGlobePress = GlobePress
                    , onGlobeMove = GlobeMove
                    , onGlobeRelease = GlobeRelease
                    , onGlobeZoom = GlobeZoom
                    , onGlobeZoomDone = GlobeZoomDone
                    , onGlobeReset = GlobeReset
                    , onSetCameraMode = SetCameraMode
                    , onEditSelectedLot = EditSelectedLot
                    , onRotateSelectedLot = RotateSelectedLot
                    , onCopySelectedLot = CopySelectedLot
                    , onPasteSelectedLot = PasteSelectedLot
                    , onClearSelectedLot = ClearSelectedLot
                    , onStampTemplate = StampTemplate
                    , onToggleLotTemplates = ToggleLotTemplates
                    , onDeselectLot = DeselectLot
                    , onToggleModal = ToggleModal
                    , onHostRoom = HostRoom
                    , onJoinRoom = JoinRoom
                    , onLeaveRoom = LeaveRoom
                    , onRoomCodeInput = RoomCodeInput
                    , onCopyRoomLink = CopyRoomLink
                    , onLoadStarterCity = LoadStarterCity
                    , onNewEmptyCity = NewEmptyCity
                    , onExportMpd = ExportMpd
                    , onExportJson = ExportJson
                    , onImportJson = RequestImportJson
                    , onToggleLanguage = ToggleLanguage
                    , onToggleMute = ToggleMute
                    , onNoOp = CityViewNoOp
                    }
                }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ GamePorts.viewportResized ViewportResized
        , GamePorts.hashChanged HashChanged
        , Browser.Events.onAnimationFrameDelta AnimationTick
        , Ports.cityRoomStateChanged RoomStateChanged
        , Ports.cityRemoteSnapshot RemoteSnapshotReceived
        , Ports.cityRemoteLotUpdated RemoteLotUpdated
        , Ports.cityRoomError RoomError
        , Ports.cityJsonImported JsonImported
        , case ( model.viewMode, model.builder ) of
            ( LotEditorPage, Just build ) ->
                Sub.map BuildMsg (Build.subscriptions build)

            ( CityPage, _ ) ->
                Sub.batch
                    [ Browser.Events.onKeyDown nonInputKeyDownDecoder
                    , Browser.Events.onKeyUp (Decode.field "key" Decode.string |> Decode.map KeyUp)
                    ]

            ( MainMenuPage, _ ) ->
                if model.menuDragging then
                    Browser.Events.onMouseUp (Decode.succeed MenuCanvasMouseUp)

                else
                    Sub.none

            _ ->
                Sub.none
        ]


nonInputKeyDownDecoder : Decode.Decoder Msg
nonInputKeyDownDecoder =
    Decode.oneOf [ Decode.at [ "target", "tagName" ] Decode.string, Decode.succeed "" ]
        |> Decode.andThen
            (\tag ->
                if List.member (String.toUpper tag) [ "INPUT", "TEXTAREA", "SELECT" ] then
                    Decode.fail "Ignoring key inside form input"

                else
                    Decode.field "key" Decode.string |> Decode.map KeyDown
            )
