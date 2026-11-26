module Pages.Build exposing
    ( BuilderTab(..)
    , Model
    , Msg(..)
    , Outcome(..)
    , document
    , init
    , resize
    , setLanguage
    , subscriptions
    , update
    , view
    )

{-| 3D Microscale 8x8 Element Editor (`Pages.Build`) for Brick City.

Wraps `Studio.Core` with:

  - An 8x8 stud boundary drafting table and max-height `32` plates (`256` LDU)
    visual guide (`City.LotGuide`), turning amber near ceiling and red on
    violations.
  - Strict enforcement of `-80..80` LDU X/Z bounds and `<= 32` plates height
    both interactively on edits and on save (`City.Bounds`).
  - Anchored 8x8 baseplate (`41539.dat`) that can be recolored freely while
    locked in place (with an optional unlock toggle).
  - Curated microscale brick palette (6 categories, search filter, 16 architectural
    & trans-glass colours, rendered part thumbnails via `Ports.requestThumbnails`).
  - Drawers for the 9 starter 8x8 templates (`TemplatesTab`), link/LDraw
    import & `#b/...` share (`ImportBuildTab`), and live bounds/health report
    (`LimitsTab`).

-}

import City.Bounds as Bounds
import City.Editor as Editor exposing (PartCategory, PartEntry, Swatch)
import City.I18n as T
import City.Ingest as Ingest
import City.LotGuide as LotGuide
import City.Templates as Templates exposing (Template)
import City.Types exposing (ElementCategory, LotCoord)
import DesignTokens.Colors as Brand
import Dict exposing (Dict)
import FeatherIcons
import Game.Ports as GamePorts
import Html exposing (Html, button, div, h3, input, p, span, text, textarea)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import I18n exposing (Language(..))
import Json.Decode as Decode
import Json.Encode as Encode
import LDraw.Colors as Colors
import LDraw.Document as Document exposing (Document)
import Ports
import Set
import Studio.Core as Core
import Studio.Health as Health
import Studio.Shortcuts exposing (TurnDirection(..))
import UI.PartCard as PartCard
import Viewport


{-| Drawer tabs in the 8x8 Element Editor.
-}
type BuilderTab
    = BuildBricksTab
    | AllBricksTab
    | TemplatesTab
    | ImportBuildTab
    | LimitsTab


{-| State of the 8x8 Element Editor.
-}
type alias Model =
    { studio : Core.Model
    , coord : LotCoord
    , elementName : String
    , elementCategory : ElementCategory
    , templateId : Maybe String
    , baseLocked : Bool
    , colour : Int
    , language : Language
    , width : Int
    , height : Int
    , thumbnails : Dict String String
    , notice : Maybe String
    , activeTab : BuilderTab
    , catalogFilter : String
    , pasteText : String
    , pasteError : Maybe String
    , activePart : String
    , recentParts : List String
    }


{-| Messages handled by the Element Editor.
-}
type Msg
    = StudioMsg Core.Msg
    | PickColour Int
    | AddPart String
    | Turn TurnDirection
    | Raise
    | Lower
    | DeleteSelected
    | CopySelected
    | PasteSelected
    | Undo
    | Redo
    | FitView
    | Finish
    | Cancel
    | ThumbnailLoaded { file : String, color : Int, url : String }
    | SelectTab BuilderTab
    | ToggleCatalog
    | ApplyTemplate Template
    | ToggleBaseLock
    | NameChanged String
    | PasteTextChanged String
    | SubmitPaste
    | UploadLdrawFile
    | CopyBuildLink
    | CatalogFilterChanged String
    | CanvasTapped
    | FileContentReceived String
    | FileLoadError String


{-| Outcome returned to `Main` after an editor update.
-}
type Outcome
    = Stay
    | Cancelled
    | Saved
        { coord : LotCoord
        , name : String
        , category : ElementCategory
        , templateId : Maybe String
        , document : Document
        , heightPlates : Int
        , partCount : Int
        }


{-| Initialize the Element Editor for a specific lot coordinate and LDraw
document.
-}
init :
    { ldrawBase : String
    , ldrawFallbackBase : String
    , supportsPointerEvents : Bool
    , language : Language
    , width : Int
    , height : Int
    }
    -> LotCoord
    -> { name : String, category : ElementCategory, templateId : Maybe String, document : Document }
    -> ( Model, Cmd Msg )
init config coord initial =
    let
        ( studio, studioCmd ) =
            Core.init
                { ldrawBase = config.ldrawBase
                , ldrawFallbackBase = config.ldrawFallbackBase
                , defaultModelUrl = ""
                , initialHash = ""
                , supportsPointerEvents = config.supportsPointerEvents
                , document = initial.document
                , gridStep = Just 20
                , rotateStep = Just 90
                , snapDistance = Just 24.0
                , collisionMode = Nothing
                }
                |> Tuple.mapFirst
                    (Core.setGizmoStyle Core.NoGizmo
                        >> Core.setDragMode (Core.DragPartsOrPan Core.OneFingerPans)
                        >> Core.setTwoFingerOrbits True
                        >> Core.setGlueEditable False
                        >> Core.setNudgeSteps { horizontal = 20, vertical = 8 }
                    )

        ( fittedStudio, fitCmd ) =
            Core.fitView studio

        model =
            { studio = fittedStudio
            , coord = coord
            , elementName = initial.name
            , elementCategory = initial.category
            , templateId = initial.templateId
            , baseLocked = Editor.baseplateIntact initial.document
            , colour = Editor.defaultColour
            , language = config.language
            , width = config.width
            , height = config.height
            , thumbnails = Dict.empty
            , notice = Nothing
            , activeTab = BuildBricksTab
            , catalogFilter = ""
            , pasteText = ""
            , pasteError = Nothing
            , activePart = "3005.dat"
            , recentParts = initialRecentParts
            }

        ( sized, sizeCmd ) =
            resize config.width config.height model
    in
    ( sized
    , Cmd.batch [ Cmd.map StudioMsg studioCmd, Cmd.map StudioMsg fitCmd, sizeCmd, requestThumbnails sized ]
    )


initialRecentParts : List String
initialRecentParts =
    [ "3005.dat", "3004.dat", "3024.dat", "3070b.dat", "54200.dat", "85984.dat", "4589.dat" ]


paletteSize : Int
paletteSize =
    7


{-| Current edited LDraw document.
-}
document : Model -> Document
document model =
    Core.getDocument model.studio


{-| Resize the 3D studio viewport on window size changes.
-}
resize : Int -> Int -> Model -> ( Model, Cmd Msg )
resize width height model =
    let
        sized =
            { model | width = width, height = height }
    in
    runCoreUnchecked (Core.update (Core.ViewportMsg (Viewport.WindowResize width (canvasHeight sized)))) sized


{-| Update editor UI language.
-}
setLanguage : Language -> Model -> Model
setLanguage language model =
    { model | language = language }


commitRecent : Model -> Model
commitRecent model =
    { model
        | recentParts =
            (model.activePart :: List.filter (\f -> f /= model.activePart) model.recentParts)
                |> List.take paletteSize
    }


setTab : BuilderTab -> Model -> Model
setTab tab model =
    { model
        | activeTab = tab
        , notice = Nothing
    }


toggleTab : BuilderTab -> Model -> Model
toggleTab tab model =
    if model.activeTab == tab then
        setTab BuildBricksTab model

    else
        setTab tab model


{-| Update editor state.
-}
update : Msg -> Model -> ( Model, Cmd Msg, Outcome )
update msg model =
    let
        shouldCommit =
            case msg of
                AddPart file ->
                    file /= model.activePart

                PickColour _ ->
                    True

                Turn _ ->
                    True

                Raise ->
                    True

                Lower ->
                    True

                DeleteSelected ->
                    True

                SelectTab _ ->
                    True

                _ ->
                    False

        prepared =
            if shouldCommit then
                commitRecent model

            else
                model

        ( next, cmd, outcome ) =
            updateModel msg prepared
    in
    if next.activeTab /= model.activeTab then
        let
            ( resized, resizeCmd ) =
                runCoreUnchecked (Core.update (Core.ViewportMsg (Viewport.WindowResize next.width (canvasHeight next)))) next
        in
        ( resized, Cmd.batch [ cmd, resizeCmd ], outcome )

    else
        ( next, cmd, outcome )


updateModel : Msg -> Model -> ( Model, Cmd Msg, Outcome )
updateModel msg model =
    let
        stay ( m, c ) =
            ( m, c, Stay )

        sfx kind =
            Ports.playSfx (Encode.object [ ( "type", Encode.string kind ) ])
    in
    case msg of
        StudioMsg studioMsg ->
            let
                ( next, cmd ) =
                    runCore (Core.update studioMsg) model
            in
            ( next, cmd, Stay )

        PickColour code ->
            let
                picked =
                    { model | colour = code, notice = Nothing }
            in
            if Core.hasSelection model.studio && baseSelected model then
                let
                    nextDoc =
                        Editor.recolorBaseplate code (Core.getDocument model.studio)

                    ( nextStudio, cmd, events ) =
                        Core.resetDocument nextDoc picked.studio
                in
                ( { picked | studio = nextStudio }
                , Cmd.batch (Cmd.map StudioMsg cmd :: requestThumbnails picked :: sfx "snap" :: List.map eventCmd events)
                , Stay
                )

            else if Core.hasSelection model.studio then
                let
                    ( next, cmd ) =
                        runCore (Core.recolorSelection code) picked
                in
                ( next, Cmd.batch [ cmd, requestThumbnails next, sfx "snap" ], Stay )

            else
                ( picked, requestThumbnails picked, Stay )

        AddPart file ->
            let
                doc =
                    Core.getDocument model.studio

                matrix =
                    Editor.dropMatrix doc model.studio.selection file

                closed =
                    setTab BuildBricksTab { model | activePart = file, notice = Nothing }

                ( next, cmd ) =
                    runCore (Core.addPartAt file model.colour matrix) closed
            in
            ( next, Cmd.batch [ cmd, sfx "snap" ], Stay )

        Turn direction ->
            let
                ( next, cmd ) =
                    runCore (Core.turnSelected direction) model
            in
            ( next, Cmd.batch [ cmd, sfx "rotate" ], Stay )

        Raise ->
            stay (runCore Core.raiseSelected model)

        Lower ->
            stay (runCore Core.lowerSelected model)

        CopySelected ->
            stay (runCoreUnchecked (\s -> ( Core.copySelection s, Cmd.none, [] )) model)

        PasteSelected ->
            stay (runCore Core.pasteClipboard model)

        DeleteSelected ->
            if baseSelected model then
                ( { model | notice = Just (T.editorKeepBaseText model.language) }, sfx "error", Stay )

            else
                let
                    ( next, cmd ) =
                        runCore Core.deleteSelection model
                in
                ( next, Cmd.batch [ cmd, sfx "delete" ], Stay )

        Undo ->
            stay (runCoreUnchecked Core.undo model)

        Redo ->
            stay (runCoreUnchecked Core.redo model)

        FitView ->
            let
                ( studio, cmd ) =
                    Core.fitView model.studio
            in
            ( { model | studio = studio }, Cmd.map StudioMsg cmd, Stay )

        Finish ->
            let
                doc =
                    Core.getDocument model.studio
            in
            case Bounds.validateDocument doc of
                Ok { heightPlates, partCount } ->
                    ( model
                    , sfx "save"
                    , Saved
                        { coord = model.coord
                        , name =
                            if String.isEmpty (String.trim model.elementName) then
                                "8×8 Lot"

                            else
                                String.trim model.elementName
                        , category = model.elementCategory
                        , templateId = model.templateId
                        , document = doc
                        , heightPlates = heightPlates
                        , partCount = partCount
                        }
                    )

                Err (issue :: _) ->
                    ( { model | notice = Just (T.boundsIssueText model.language issue) }, sfx "error", Stay )

                Err [] ->
                    ( model, Cmd.none, Stay )

        Cancel ->
            ( model, Cmd.none, Cancelled )

        ThumbnailLoaded thumb ->
            ( { model | thumbnails = Dict.insert (PartCard.thumbnailKey thumb.file thumb.color) thumb.url model.thumbnails }
            , Cmd.none
            , Stay
            )

        SelectTab tab ->
            ( toggleTab tab model, Cmd.none, Stay )

        ToggleCatalog ->
            ( toggleTab AllBricksTab model, Cmd.none, Stay )

        ApplyTemplate tmpl ->
            let
                doc =
                    Document.parse tmpl.ldrawText

                ( nextStudio, resetCmd, events ) =
                    Core.resetDocument doc model.studio

                ( fittedStudio, fitCmd ) =
                    Core.fitView nextStudio

                tmplName =
                    case model.language of
                        En ->
                            tmpl.nameEn

                        Fi ->
                            tmpl.nameFi

                closed =
                    setTab BuildBricksTab model
            in
            ( { closed
                | studio = fittedStudio
                , elementName = tmplName
                , elementCategory = tmpl.category
                , templateId = Just tmpl.id
                , baseLocked = Editor.baseplateIntact doc
              }
            , Cmd.batch (Cmd.map StudioMsg resetCmd :: Cmd.map StudioMsg fitCmd :: sfx "template" :: List.map eventCmd events)
            , Stay
            )

        ToggleBaseLock ->
            ( { model | baseLocked = not model.baseLocked, notice = Nothing }, Cmd.none, Stay )

        NameChanged nextName ->
            ( { model | elementName = nextName }, Cmd.none, Stay )

        PasteTextChanged textVal ->
            ( { model | pasteText = textVal, pasteError = Nothing }, Cmd.none, Stay )

        SubmitPaste ->
            loadImportedInput model.pasteText model

        UploadLdrawFile ->
            ( model, GamePorts.requestFileUpload (), Stay )

        CopyBuildLink ->
            let
                link =
                    Ingest.encodeElementLink (Core.getDocument model.studio)
            in
            ( { model | notice = Just (T.tr model.language "Build link copied!" "Rakennuslinkki kopioitu!") }
            , GamePorts.copyShareUrl link
            , Stay
            )

        CatalogFilterChanged q ->
            ( { model | catalogFilter = q }, Cmd.none, Stay )

        CanvasTapped ->
            ( { model | notice = Nothing }, Cmd.none, Stay )

        FileContentReceived raw ->
            loadImportedInput raw model

        FileLoadError err ->
            ( { model | pasteError = Just err, notice = Just err }, sfx "error", Stay )


loadImportedInput : String -> Model -> ( Model, Cmd Msg, Outcome )
loadImportedInput raw model =
    case Ingest.decodeElementInput Nothing raw of
        Ok decoded ->
            case Bounds.validateDocument decoded.document of
                Ok _ ->
                    let
                        ( nextStudio, resetCmd, events ) =
                            Core.resetDocument decoded.document model.studio

                        ( fittedStudio, fitCmd ) =
                            Core.fitView nextStudio

                        closed =
                            setTab BuildBricksTab model
                    in
                    ( { closed
                        | studio = fittedStudio
                        , elementName = decoded.title
                        , templateId = Nothing
                        , baseLocked = Editor.baseplateIntact decoded.document
                        , pasteText = ""
                        , pasteError = Nothing
                      }
                    , Cmd.batch (Cmd.map StudioMsg resetCmd :: Cmd.map StudioMsg fitCmd :: List.map eventCmd events)
                    , Stay
                    )

                Err (issue :: _) ->
                    ( { model | pasteError = Just (T.boundsIssueText model.language issue) }, Cmd.none, Stay )

                Err [] ->
                    ( model, Cmd.none, Stay )

        Err err ->
            ( { model | pasteError = Just err }, Cmd.none, Stay )


runCoreUnchecked : (Core.Model -> ( Core.Model, Cmd Core.Msg, List Core.Event )) -> Model -> ( Model, Cmd Msg )
runCoreUnchecked step model =
    let
        ( studio, cmd, events ) =
            step model.studio
    in
    ( { model | studio = studio }
    , Cmd.batch (Cmd.map StudioMsg cmd :: List.map eventCmd events)
    )


{-| Run a `Studio.Core` step while protecting the anchored 8x8 baseplate and
strictly enforcing `-80..80` X/Z bounds and max height `32` plates (`256` LDU).
If the step causes any non-drag violation or breaks the baseplate, reject it
and tell the builder why!
-}
runCore : (Core.Model -> ( Core.Model, Cmd Core.Msg, List Core.Event )) -> Model -> ( Model, Cmd Msg )
runCore step model =
    let
        wasDragging =
            Core.isDragging model.studio

        beforeDoc =
            Core.getDocument model.studio

        ( studio, cmd, events ) =
            step model.studio

        isNowDragging =
            Core.isDragging studio

        dragJustEnded =
            wasDragging && not isNowDragging

        afterDoc =
            Core.getDocument studio

        docMutated =
            afterDoc /= beforeDoc

        baseOk =
            not model.baseLocked
                || not (Editor.baseplateIntact beforeDoc)
                || Editor.baseplateIntact afterDoc

        boundsResult =
            Bounds.validateDocument afterDoc

        cancelOrRevert msgText =
            if wasDragging then
                let
                    ( cancelled, cancelCmd, cancelEvents ) =
                        Core.cancelDrag model.studio
                in
                ( { model | studio = cancelled, notice = Just msgText }
                , Cmd.batch (Cmd.map StudioMsg cancelCmd :: List.map eventCmd cancelEvents)
                )

            else
                ( { model | notice = Just msgText }, Cmd.none )
    in
    if not baseOk then
        if isNowDragging then
            ( { model | studio = studio, notice = Just (T.editorKeepBaseText model.language) }
            , Cmd.batch (Cmd.map StudioMsg cmd :: List.map eventCmd events)
            )

        else
            cancelOrRevert (T.editorKeepBaseText model.language)

    else if docMutated || dragJustEnded then
        case boundsResult of
            Ok _ ->
                ( { model | studio = studio, notice = Nothing }
                , Cmd.batch (Cmd.map StudioMsg cmd :: List.map eventCmd events)
                )

            Err (issue :: _) ->
                if isNowDragging then
                    -- Let user finish the drag; show warning banner while out of bounds
                    ( { model | studio = studio, notice = Just (T.boundsIssueText model.language issue) }
                    , Cmd.batch (Cmd.map StudioMsg cmd :: List.map eventCmd events)
                    )

                else
                    -- Drop invalid finalized edit or cancel drag that ended out of bounds
                    cancelOrRevert (T.boundsIssueText model.language issue)

            Err [] ->
                ( { model | studio = studio, notice = Nothing }
                , Cmd.batch (Cmd.map StudioMsg cmd :: List.map eventCmd events)
                )

    else
        ( { model | studio = studio }
        , Cmd.batch (Cmd.map StudioMsg cmd :: List.map eventCmd events)
        )


eventCmd : Core.Event -> Cmd Msg
eventCmd event =
    case event of
        Core.FlattenRequested payload ->
            GamePorts.requestGeometryFlatten payload

        _ ->
            Cmd.none


baseSelected : Model -> Bool
baseSelected model =
    model.baseLocked
        && (Core.getPlacements model.studio
                |> List.any (\p -> Set.member p.id model.studio.selection && Editor.isBaseplatePlacement p)
           )


requestThumbnails : Model -> Cmd Msg
requestThumbnails model =
    let
        missing =
            Editor.parts
                |> List.filter (\p -> not (Dict.member (PartCard.thumbnailKey p.file model.colour) model.thumbnails))
    in
    if List.isEmpty missing then
        Cmd.none

    else
        Ports.requestThumbnails
            (List.map
                (\p ->
                    { file = p.file
                    , color = model.colour
                    , rgb = Colors.resolveColor model.colour 16 |> (\c -> [ c.r, c.g, c.b ])
                    }
                )
                missing
            )


{-| Subscriptions for `Studio.Core`, geometry flattening worker, file drops,
and rendered part thumbnails.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map StudioMsg (Core.subscriptions model.studio)
        , GamePorts.geometryFlattened (StudioMsg << Core.geometryFlattened)
        , GamePorts.geometryFlattenFailed (StudioMsg << Core.geometryFlattenFailed)
        , GamePorts.fileContentReceived FileContentReceived
        , GamePorts.fileLoadError FileLoadError
        , Ports.thumbnailLoaded ThumbnailLoaded
        ]



-- ── View ─────────────────────────────────────────────────────────────────────


dockHeight : Int
dockHeight =
    132


drawerHeight : Model -> Int -> Int
drawerHeight model compact =
    if model.width >= 768 then
        max compact (model.height - 130)

    else
        max compact (min (2 * compact) (model.height - 200))


tabDrawerHeight : Model -> BuilderTab -> Int
tabDrawerHeight model tab =
    case tab of
        BuildBricksTab ->
            dockHeight

        AllBricksTab ->
            dockHeight + drawerHeight model 240

        _ ->
            drawerHeight model 280


canvasHeight : Model -> Int
canvasHeight model =
    max 160 (model.height - tabDrawerHeight model model.activeTab)


{-| Render the full 3D Element Editor screen.
-}
view : Model -> Html Msg
view model =
    let
        doc =
            Core.getDocument model.studio

        measured =
            Bounds.documentBounds doc

        violating =
            Bounds.violatingPlacementIds doc

        hasViolations =
            not (Set.isEmpty violating) || not (Bounds.isDocumentValid doc)
    in
    div
        [ Attr.class "fixed inset-0 overflow-hidden bg-slate-950 text-slate-50 font-sans select-none"
        , Attr.attribute "data-testid" "lot-builder-page"
        ]
        [ div
            [ Attr.class "absolute top-0 left-0 touch-none"
            , Attr.style "width" (px model.width)
            , Attr.style "height" (px (canvasHeight model))
            , Html.Events.on "pointerdown" (Decode.succeed CanvasTapped)
            ]
            [ Html.map StudioMsg
                (Core.viewWithExtra
                    { width = model.width
                    , height = canvasHeight model
                    , extraEntities =
                        \camera aspect ->
                            LotGuide.entities
                                { heightPlates = measured.heightPlates
                                , hasViolations = hasViolations
                                }
                                camera
                                aspect
                    }
                    model.studio
                )
            ]
        , topBar model measured.heightPlates hasViolations
        , noticeView model
        , bottomPanel model measured
        ]


topBar : Model -> Int -> Bool -> Html Msg
topBar model heightPlates hasViolations =
    let
        lang =
            model.language

        tabBtn tab dataAttr label icon =
            let
                isActive =
                    model.activeTab == tab
            in
            button
                [ Attr.type_ "button"
                , onClick (SelectTab tab)
                , Attr.attribute dataAttr ""
                , Attr.attribute "aria-label" label
                , Attr.attribute "aria-pressed"
                    (if isActive then
                        "true"

                     else
                        "false"
                    )
                , Attr.title label
                , Attr.class
                    ("inline-flex items-center justify-center w-10 h-10 shrink-0 rounded-xl border cursor-pointer transition-all "
                        ++ (if isActive then
                                "bg-brand-yellow border-brand-yellow text-brand font-bold"

                            else
                                "bg-slate-900/80 border-white/20 text-slate-200 hover:bg-slate-800"
                           )
                    )
                ]
                [ icon |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [] ]

        heightBadgeClass =
            if hasViolations then
                "bg-rose-600/90 border-rose-300 text-white"

            else if heightPlates >= 28 then
                "bg-amber-500/90 border-amber-200 text-slate-950"

            else
                "bg-slate-900/85 border-white/20 text-sky-300"
    in
    div
        [ Attr.class "absolute top-0 left-0 right-0 z-30 flex flex-wrap items-center gap-2 p-2.5 pointer-events-none"
        , Attr.attribute "data-testid" "builder-topbar"
        ]
        [ button
            [ Attr.type_ "button"
            , onClick Finish
            , Attr.attribute "data-testid" "save-lot-button"
            , Attr.title (T.saveLotButton lang)
            , Attr.attribute "aria-label" (T.saveLotButton lang)
            , Attr.class "pointer-events-auto inline-flex items-center gap-1.5 h-10 px-3.5 rounded-xl bg-brand-yellow text-brand font-extrabold text-sm cursor-pointer shadow-lg active:brightness-110"
            ]
            [ FeatherIcons.check |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml []
            , span [ Attr.class "hidden sm:inline" ] [ text (T.tr lang "Save Lot" "Tallenna") ]
            ]
        , button
            [ Attr.type_ "button"
            , onClick Cancel
            , Attr.attribute "data-testid" "cancel-lot-button"
            , Attr.title (T.cancelButton lang)
            , Attr.attribute "aria-label" (T.cancelButton lang)
            , Attr.class "pointer-events-auto inline-flex items-center justify-center w-10 h-10 rounded-xl bg-slate-900/85 border border-white/20 text-slate-200 cursor-pointer hover:bg-slate-800"
            ]
            [ FeatherIcons.x |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [] ]
        , input
            [ Attr.type_ "text"
            , Attr.value model.elementName
            , onInput NameChanged
            , Attr.attribute "data-testid" "element-name-input"
            , Attr.class "pointer-events-auto h-10 w-32 sm:w-44 px-3 rounded-xl bg-slate-900/85 border border-white/20 text-xs font-semibold text-white"
            ]
            []
        , div
            [ Attr.class "pointer-events-auto flex items-center justify-center gap-1.5 mx-auto"
            ]
            [ tabBtn TemplatesTab "data-templates-toggle" (T.editorTemplatesTab lang) FeatherIcons.layers
            , tabBtn ImportBuildTab "data-import-toggle" (T.editorOpenTab lang) FeatherIcons.download
            , tabBtn LimitsTab "data-limits-toggle" (T.editorLimitsTab lang) FeatherIcons.sliders
            ]
        , div
            [ Attr.class ("pointer-events-auto px-3 py-1.5 rounded-xl border text-xs font-mono font-bold " ++ heightBadgeClass)
            , Attr.attribute "data-testid" "height-indicator"
            ]
            [ text ("8×8 • " ++ String.fromInt heightPlates ++ "/32") ]
        , button
            [ Attr.type_ "button"
            , onClick FitView
            , Attr.title (T.fitViewLabel lang)
            , Attr.attribute "aria-label" (T.fitViewLabel lang)
            , Attr.class "pointer-events-auto inline-flex items-center justify-center w-10 h-10 rounded-xl bg-slate-900/85 border border-white/20 text-slate-200 cursor-pointer hover:bg-slate-800"
            ]
            [ FeatherIcons.maximize |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [] ]
        ]


noticeView : Model -> Html Msg
noticeView model =
    case model.notice of
        Just message ->
            div
                [ Attr.attribute "role" "status"
                , Attr.attribute "data-testid" "builder-notice"
                , Attr.class "absolute left-1/2 -translate-x-1/2 top-14 z-30 max-w-md px-3.5 py-1.5 rounded-xl bg-slate-900/95 border border-amber-400/50 text-amber-200 text-xs font-semibold text-center shadow-lg"
                ]
                [ text message ]

        Nothing ->
            text ""


bottomPanel : Model -> Bounds.DocumentBounds -> Html Msg
bottomPanel model measured =
    case model.activeTab of
        BuildBricksTab ->
            dock model

        AllBricksTab ->
            dock model

        TemplatesTab ->
            viewTemplatesDrawer model

        ImportBuildTab ->
            viewImportDrawer model

        LimitsTab ->
            viewLimitsDrawer model measured


dock : Model -> Html Msg
dock model =
    let
        isCatalogOpen =
            model.activeTab == AllBricksTab

        height =
            if isCatalogOpen then
                tabDrawerHeight model AllBricksTab

            else
                dockHeight
    in
    div
        [ Attr.attribute "data-testid" "builder-dock"
        , Attr.class "absolute left-0 right-0 bottom-0 flex flex-col gap-1.5 px-2.5 py-2 bg-slate-900/95 border-t border-slate-400/25 box-border"
        , Attr.style "height" (px height)
        ]
        ((if isCatalogOpen then
            [ catalogView model ]

          else
            []
         )
            ++ [ modellingControls model, paletteRow model ]
        )


modellingControls : Model -> Html Msg
modellingControls model =
    let
        lang =
            model.language

        hasSelection =
            Core.hasSelection model.studio

        onBase =
            baseSelected model

        wide =
            model.width >= 640
    in
    div
        [ Attr.attribute "role" "toolbar"
        , Attr.class "flex items-center justify-center gap-1.5 shrink-0"
        ]
        [ toolButton (T.tr lang "Undo" "Kumoa") FeatherIcons.cornerUpLeft (enabledIf (Core.canUndo model.studio) Undo)
        , toolButton (T.tr lang "Redo" "Tee uudelleen") FeatherIcons.cornerUpRight (enabledIf (Core.canRedo model.studio) Redo)
        , toolButton (T.tr lang "Turn left" "Käännä vasemmalle") FeatherIcons.rotateCcw (enabledIf (hasSelection && not onBase) (Turn TurnLeft))
        , toolButton (T.tr lang "Turn right" "Käännä oikealle") FeatherIcons.rotateCw (enabledIf (hasSelection && not onBase) (Turn TurnRight))
        , toolButton (T.tr lang "Raise" "Nosta") FeatherIcons.arrowUp (enabledIf (hasSelection && not onBase) Raise)
        , toolButton (T.tr lang "Lower" "Laske") FeatherIcons.arrowDown (enabledIf (hasSelection && not onBase) Lower)
        , if wide then
            toolButton (T.tr lang "Copy" "Kopioi") FeatherIcons.copy (enabledIf (hasSelection && not onBase) CopySelected)

          else
            text ""
        , if wide then
            toolButton (T.tr lang "Paste" "Liitä") FeatherIcons.clipboard (Just PasteSelected)

          else
            text ""
        , toolButton
            (if onBase then
                T.editorKeepBaseText lang

             else
                T.tr lang "Delete" "Poista"
            )
            FeatherIcons.trash2
            (enabledIf (hasSelection && not onBase) DeleteSelected)
        ]


toolButton : String -> FeatherIcons.Icon -> Maybe Msg -> Html Msg
toolButton label icon maybeMsg =
    let
        enabled =
            maybeMsg /= Nothing
    in
    button
        ([ Attr.type_ "button"
         , Attr.attribute "aria-label" label
         , Attr.title label
         , Attr.disabled (not enabled)
         , Attr.class
            ("inline-flex items-center justify-center w-11 h-11 shrink-0 rounded-lg border transition-all "
                ++ (if enabled then
                        "bg-slate-800/90 border-white/25 text-slate-50 cursor-pointer hover:bg-slate-700"

                    else
                        "bg-slate-900/40 border-white/10 text-slate-600 opacity-40 cursor-default"
                   )
            )
         ]
            ++ (case maybeMsg of
                    Just m ->
                        [ onClick m ]

                    Nothing ->
                        []
               )
        )
        [ icon |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]


paletteRow : Model -> Html Msg
paletteRow model =
    div
        [ Attr.class "flex items-center gap-2 flex-1 min-h-0 overflow-hidden" ]
        [ colourGrid model
        , button
            [ Attr.type_ "button"
            , onClick ToggleCatalog
            , Attr.attribute "data-testid" "toggle-catalog-button"
            , Attr.title (T.tr model.language "All microscale parts" "Kaikki pienoispalaosat")
            , Attr.class
                ("inline-flex items-center justify-center w-10 h-14 shrink-0 rounded-xl border cursor-pointer "
                    ++ (if model.activeTab == AllBricksTab then
                            "bg-brand-yellow border-brand-yellow text-brand"

                        else
                            "bg-slate-800/80 border-white/20 text-slate-100"
                       )
                )
            ]
            [ FeatherIcons.grid |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [] ]
        , div
            [ Attr.class "palette-scroll-x flex items-center gap-2 flex-1 min-w-0 overflow-x-auto py-0.5" ]
            (List.map (partButton model) (recentPartEntries model))
        , lockButton model
        ]


recentPartEntries : Model -> List PartEntry
recentPartEntries model =
    model.recentParts
        |> List.filterMap (\file -> Editor.parts |> List.filter (\p -> p.file == file) |> List.head)


colourGrid : Model -> Html Msg
colourGrid model =
    div
        [ Attr.class "grid grid-cols-8 gap-1 shrink-0"
        , Attr.attribute "role" "listbox"
        , Attr.attribute "aria-label" (T.tr model.language "Colour" "Väri")
        ]
        (List.map (colourSwatch model) Editor.palette)


colourSwatch : Model -> Swatch -> Html Msg
colourSwatch model swatch =
    let
        c =
            Colors.resolveColor 0 swatch.code

        selected =
            model.colour == swatch.code

        bg =
            "rgba("
                ++ String.fromInt (round (c.r * 255))
                ++ ","
                ++ String.fromInt (round (c.g * 255))
                ++ ","
                ++ String.fromInt (round (c.b * 255))
                ++ ","
                ++ String.fromFloat c.alpha
                ++ ")"

        label =
            T.tr model.language swatch.nameEn swatch.nameFi
    in
    button
        [ Attr.type_ "button"
        , onClick (PickColour swatch.code)
        , Attr.title label
        , Attr.attribute "aria-label" label
        , Attr.attribute "aria-selected"
            (if selected then
                "true"

             else
                "false"
            )
        , Attr.style "background" bg
        , Attr.class
            ("w-5 h-5 rounded-md cursor-pointer transition-transform "
                ++ (if selected then
                        "ring-2 ring-brand-yellow scale-110"

                    else
                        "ring-1 ring-white/30 hover:scale-105"
                   )
            )
        ]
        []


partButton : Model -> PartEntry -> Html Msg
partButton model entry =
    let
        label =
            T.tr model.language entry.nameEn entry.nameFi

        thumbUrl =
            Dict.get (PartCard.thumbnailKey entry.file model.colour) model.thumbnails
    in
    PartCard.compactView
        { imageUrl = thumbUrl
        , tooltip = label ++ " (" ++ entry.file ++ ")"
        , ariaLabel = label
        , badge = Nothing
        , onClick = Just (AddPart entry.file)
        , attributes = []
        }


lockButton : Model -> Html Msg
lockButton model =
    let
        lang =
            model.language

        label =
            if model.baseLocked then
                T.tr lang "8×8 baseplate locked" "8×8-pohjalevy lukittu"

            else
                T.tr lang "8×8 baseplate unlocked" "8×8-pohjalevy avattu"
    in
    button
        [ Attr.type_ "button"
        , onClick ToggleBaseLock
        , Attr.title label
        , Attr.attribute "aria-label" label
        , Attr.class
            ("inline-flex items-center justify-center w-10 h-14 shrink-0 rounded-xl border cursor-pointer "
                ++ (if model.baseLocked then
                        "bg-slate-800/80 border-white/20 text-slate-300"

                    else
                        "bg-amber-500/20 border-amber-400 text-amber-300"
                   )
            )
        ]
        [ (if model.baseLocked then
            FeatherIcons.lock

           else
            FeatherIcons.unlock
          )
            |> FeatherIcons.withSize 18
            |> FeatherIcons.toHtml []
        ]


catalogView : Model -> Html Msg
catalogView model =
    let
        lang =
            model.language

        query =
            String.toLower (String.trim model.catalogFilter)

        matches entry =
            String.isEmpty query
                || String.contains query (String.toLower entry.nameEn)
                || String.contains query (String.toLower entry.nameFi)
                || String.contains query (String.toLower entry.file)

        renderCategory cat =
            let
                filtered =
                    List.filter matches cat.parts
            in
            if List.isEmpty filtered then
                Nothing

            else
                Just
                    (div [ Attr.class "flex flex-col gap-1.5" ]
                        [ h3 [ Attr.class "text-xs font-bold uppercase tracking-wider text-slate-400" ]
                            [ text (T.tr lang cat.titleEn cat.titleFi) ]
                        , div [ Attr.class "flex flex-wrap gap-2" ]
                            (List.map (partButton model) filtered)
                        ]
                    )
    in
    div [ Attr.class "flex flex-col gap-2 flex-1 min-h-0" ]
        [ input
            [ Attr.type_ "text"
            , Attr.value model.catalogFilter
            , onInput CatalogFilterChanged
            , Attr.placeholder (T.searchPartsPlaceholder lang)
            , Attr.class "h-9 px-3 rounded-lg bg-slate-800 border border-white/15 text-xs text-white placeholder:text-slate-400"
            ]
            []
        , div [ Attr.class "palette-scroll-y flex-1 overflow-y-auto flex flex-col gap-3 pr-1" ]
            (List.filterMap renderCategory Editor.partCategories)
        ]


viewTemplatesDrawer : Model -> Html Msg
viewTemplatesDrawer model =
    let
        lang =
            model.language

        card tmpl =
            let
                titleText =
                    T.tr lang tmpl.nameEn tmpl.nameFi

                descText =
                    T.tr lang tmpl.descriptionEn tmpl.descriptionFi

                active =
                    model.templateId == Just tmpl.id
            in
            button
                [ Attr.type_ "button"
                , onClick (ApplyTemplate tmpl)
                , Attr.attribute "data-testid" ("template-card-" ++ tmpl.id)
                , Attr.class
                    ("flex flex-col items-start gap-1 p-3 rounded-xl border text-left cursor-pointer transition-all "
                        ++ (if active then
                                "bg-brand-yellow/15 border-brand-yellow text-white"

                            else
                                "bg-slate-800/80 border-white/15 text-slate-100 hover:border-white/35"
                           )
                    )
                ]
                [ div [ Attr.class "flex items-center justify-between w-full gap-2" ]
                    [ span [ Attr.class "text-sm font-bold" ] [ text titleText ]
                    , span [ Attr.class "text-[11px] font-mono text-sky-300" ]
                        [ text (T.heightUnit lang tmpl.heightPlates) ]
                    ]
                , p [ Attr.class "text-xs text-slate-400 leading-snug" ] [ text descText ]
                , span [ Attr.class "text-[11px] text-slate-400 font-medium" ]
                    [ text (T.categoryLabel lang tmpl.category ++ " • " ++ String.fromInt tmpl.partCount ++ " pcs") ]
                ]
    in
    drawerShell model
        (T.editorTemplatesTab lang)
        [ div [ Attr.class "grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-2.5" ]
            (List.map card Templates.all)
        ]


viewImportDrawer : Model -> Html Msg
viewImportDrawer model =
    let
        lang =
            model.language
    in
    drawerShell model
        (T.editorOpenTab lang)
        [ div [ Attr.class "flex flex-col gap-3 max-w-xl" ]
            [ div [ Attr.class "flex flex-wrap gap-2" ]
                [ button
                    [ Attr.type_ "button"
                    , onClick UploadLdrawFile
                    , Attr.class "inline-flex items-center gap-2 h-10 px-4 rounded-xl bg-slate-800 border border-white/20 text-xs font-bold text-white cursor-pointer hover:bg-slate-700"
                    ]
                    [ FeatherIcons.upload |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml []
                    , span [] [ text (T.uploadLdrawLabel lang) ]
                    ]
                , button
                    [ Attr.type_ "button"
                    , onClick CopyBuildLink
                    , Attr.class "inline-flex items-center gap-2 h-10 px-4 rounded-xl bg-brand-yellow/20 border border-brand-yellow/50 text-xs font-bold text-brand-yellow cursor-pointer hover:bg-brand-yellow/30"
                    ]
                    [ FeatherIcons.share2 |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml []
                    , span [] [ text (T.tr lang "Copy #b/... build link" "Kopioi #b/...-linkki") ]
                    ]
                ]
            , p [ Attr.class "text-xs text-slate-300" ] [ text (T.pasteBuildHint lang) ]
            , textarea
                [ Attr.value model.pasteText
                , onInput PasteTextChanged
                , Attr.rows 3
                , Attr.class "w-full p-2.5 rounded-xl bg-slate-950 border border-white/20 text-xs font-mono text-white"
                ]
                []
            , case model.pasteError of
                Just err ->
                    p [ Attr.class "text-xs text-rose-300 font-semibold" ] [ text err ]

                Nothing ->
                    text ""
            , button
                [ Attr.type_ "button"
                , onClick SubmitPaste
                , Attr.class "self-start h-10 px-4 rounded-xl bg-brand-yellow text-brand text-xs font-extrabold cursor-pointer"
                ]
                [ text (T.pasteButtonLabel lang) ]
            ]
        ]


viewLimitsDrawer : Model -> Bounds.DocumentBounds -> Html Msg
viewLimitsDrawer model measured =
    let
        lang =
            model.language

        doc =
            Core.getDocument model.studio

        validation =
            Bounds.validateDocument doc

        healthIssues =
            Health.check model.studio.viewport.partCache doc Set.empty

        heightPct =
            min 100 (round (toFloat measured.heightPlates / toFloat Bounds.maxHeightPlates * 100))

        barColor =
            if measured.heightPlates > Bounds.maxHeightPlates then
                "bg-rose-500"

            else if measured.heightPlates >= 28 then
                "bg-amber-400"

            else
                "bg-sky-400"
    in
    drawerShell model
        (T.editorLimitsTab lang)
        [ div [ Attr.class "flex flex-col gap-3 max-w-xl" ]
            [ div [ Attr.class "grid grid-cols-2 gap-3" ]
                [ div [ Attr.class "p-3 rounded-xl bg-slate-800/80 border border-white/10" ]
                    [ p [ Attr.class "text-xs text-slate-400" ] [ text (T.editorHeightLabel lang measured.heightPlates) ]
                    , div [ Attr.class "mt-2 h-2.5 rounded-full bg-slate-950 overflow-hidden" ]
                        [ div [ Attr.class ("h-full rounded-full " ++ barColor), Attr.style "width" (String.fromInt heightPct ++ "%") ] [] ]
                    ]
                , div [ Attr.class "p-3 rounded-xl bg-slate-800/80 border border-white/10" ]
                    [ p [ Attr.class "text-xs text-slate-400" ] [ text (T.editorPartsLabel lang measured.partCount) ]
                    , p [ Attr.class "mt-1 text-xs font-mono text-slate-200" ]
                        [ text ("X: " ++ String.fromInt (round measured.minX) ++ ".." ++ String.fromInt (round measured.maxX) ++ " • Z: " ++ String.fromInt (round measured.minZ) ++ ".." ++ String.fromInt (round measured.maxZ)) ]
                    ]
                ]
            , case validation of
                Ok _ ->
                    p [ Attr.class "p-3 rounded-xl bg-emerald-950/60 border border-emerald-500/40 text-xs text-emerald-300 font-semibold" ]
                        [ text (T.editorBoundsOk lang) ]

                Err issues ->
                    div [ Attr.class "flex flex-col gap-1.5" ]
                        (List.map
                            (\iss ->
                                p [ Attr.class "p-2.5 rounded-xl bg-rose-950/60 border border-rose-500/40 text-xs text-rose-200 font-semibold" ]
                                    [ text (T.boundsIssueText lang iss) ]
                            )
                            issues
                        )
            , p [ Attr.class "text-xs text-slate-400" ]
                [ text
                    (T.tr lang
                        ("Connectivity check: " ++ String.fromInt (List.length healthIssues) ++ " loose/overlap notes.")
                        ("Kiinnitystarkistus: " ++ String.fromInt (List.length healthIssues) ++ " irrallista tai päällekkäistä huomautusta.")
                    )
                ]
            ]
        ]


drawerShell : Model -> String -> List (Html Msg) -> Html Msg
drawerShell model heading body =
    div
        [ Attr.class "absolute left-0 right-0 bottom-0 flex flex-col gap-3 p-4 bg-slate-900/95 border-t border-slate-400/25 box-border"
        , Attr.style "height" (px (tabDrawerHeight model model.activeTab))
        ]
        [ div [ Attr.class "flex items-center justify-between" ]
            [ h3 [ Attr.class "text-sm font-extrabold text-white" ] [ text heading ]
            , button
                [ Attr.type_ "button"
                , onClick (SelectTab BuildBricksTab)
                , Attr.class "inline-flex items-center justify-center w-8 h-8 rounded-lg bg-slate-800 text-slate-200 cursor-pointer"
                ]
                [ FeatherIcons.chevronDown |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [] ]
            ]
        , div [ Attr.class "palette-scroll-y flex-1 overflow-y-auto pr-1" ] body
        ]


enabledIf : Bool -> msg -> Maybe msg
enabledIf ok msg =
    if ok then
        Just msg

    else
        Nothing


px : Int -> String
px n =
    String.fromInt n ++ "px"
