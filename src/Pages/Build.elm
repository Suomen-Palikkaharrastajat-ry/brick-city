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
  - Organized tabbed bottom dock with drawers for Microscale Bricks,
    Plates/Tiles, SNOT/Slopes, and Starter Templates, plus inline search and
    16 architectural & trans-glass colours.
  - Instant Undo/Redo and `Studio.Health` Tidy Up actions accessible right in
    the dock toolbar without cluttering the 3D canvas, plus clean Import/Share
    and Lot Limits modal overlays.

-}

import City.Bounds as Bounds
import City.Editor as Editor exposing (PartCategory, PartDrawer(..), PartEntry, Swatch)
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
import Set exposing (Set)
import Studio.Core as Core
import Studio.Health as Health
import Studio.Shortcuts exposing (TurnDirection(..))
import UI.PartCard as PartCard
import Viewport


{-| Organized drawer and overlay tabs in the 8x8 Element Editor.
-}
type BuilderTab
    = BricksTab
    | PlatesTilesTab
    | SnotSlopesTab
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
    , lastPartTab : BuilderTab
    , catalogSearchOpen : Bool
    , catalogFilter : String
    , pasteText : String
    , pasteError : Maybe String
    , activePart : String
    , recentParts : List String
    , tidyFixes : List Health.Fix
    , measuredBounds : Bounds.DocumentBounds
    , hasBoundsViolation : Bool
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
    | TidyUp
    | FitView
    | Finish
    | Cancel
    | ThumbnailLoaded { file : String, color : Int, url : String }
    | SelectTab BuilderTab
    | CloseOverlayTab
    | ToggleSearch
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
    | NoOp


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

        initialMeasure =
            measureBounds initial.document

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
            , activeTab = BricksTab
            , lastPartTab = BricksTab
            , catalogSearchOpen = False
            , catalogFilter = ""
            , pasteText = ""
            , pasteError = Nothing
            , activePart = "3005.dat"
            , recentParts = initialRecentParts
            , tidyFixes = computeTidyFixes fittedStudio
            , measuredBounds = initialMeasure.measuredBounds
            , hasBoundsViolation = initialMeasure.hasBoundsViolation
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


isPartDrawerTab : BuilderTab -> Bool
isPartDrawerTab tab =
    case tab of
        BricksTab ->
            True

        PlatesTilesTab ->
            True

        SnotSlopesTab ->
            True

        _ ->
            False


setTab : BuilderTab -> Model -> Model
setTab tab model =
    let
        nextLastPartTab =
            if isPartDrawerTab tab then
                tab

            else if isPartDrawerTab model.activeTab then
                model.activeTab

            else
                model.lastPartTab
    in
    { model
        | activeTab = tab
        , lastPartTab = nextLastPartTab
        , notice = Nothing
    }


toggleTab : BuilderTab -> Model -> Model
toggleTab tab model =
    if isPartDrawerTab tab then
        setTab tab model

    else if model.activeTab == tab then
        setTab model.lastPartTab model

    else
        setTab tab model


baseplateIds : Document -> Set Int
baseplateIds doc =
    Document.mainPlacements doc
        |> List.filter Editor.isBaseplatePlacement
        |> List.map .id
        |> Set.fromList


computeTidyFixes : Core.Model -> List Health.Fix
computeTidyFixes studio =
    let
        doc =
            Core.getDocument studio

        cache =
            studio.viewport.partCache

        glued =
            baseplateIds doc
    in
    Health.plan cache doc glued (Health.check cache doc glued)


measureBounds : Document -> { measuredBounds : Bounds.DocumentBounds, hasBoundsViolation : Bool }
measureBounds doc =
    let
        measured =
            Bounds.documentBounds doc

        violating =
            Bounds.violatingPlacementIds doc
    in
    { measuredBounds = measured
    , hasBoundsViolation = not (Set.isEmpty violating) || not (Bounds.isDocumentValid doc)
    }


withMeasuredBounds : Model -> Model
withMeasuredBounds model =
    let
        nextMeasure =
            measureBounds (Core.getDocument model.studio)
    in
    { model
        | measuredBounds = nextMeasure.measuredBounds
        , hasBoundsViolation = nextMeasure.hasBoundsViolation
    }


withTidy : Model -> Model
withTidy model =
    let
        nextMeasure =
            measureBounds (Core.getDocument model.studio)
    in
    { model
        | tidyFixes = computeTidyFixes model.studio
        , measuredBounds = nextMeasure.measuredBounds
        , hasBoundsViolation = nextMeasure.hasBoundsViolation
    }


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
    if tabDrawerHeight next next.activeTab /= tabDrawerHeight model model.activeTab then
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
                wasDragging =
                    Core.isDragging model.studio

                beforeDoc =
                    Core.getDocument model.studio

                beforeCacheCount =
                    Dict.size model.studio.viewport.partCache

                ( next, cmd ) =
                    runCore (Core.update studioMsg) model

                afterDoc =
                    Core.getDocument next.studio

                afterCacheCount =
                    Dict.size next.studio.viewport.partCache

                needsBoundsRefresh =
                    afterDoc /= beforeDoc

                needsTidyRefresh =
                    not (Core.isDragging next.studio)
                        && (needsBoundsRefresh || wasDragging || afterCacheCount /= beforeCacheCount)
            in
            ( if needsTidyRefresh then
                withTidy next

              else if needsBoundsRefresh then
                withMeasuredBounds next

              else
                next
            , cmd
            , Stay
            )

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
                ( withTidy { picked | studio = nextStudio }
                , Cmd.batch (Cmd.map StudioMsg cmd :: requestThumbnails picked :: sfx "snap" :: List.map eventCmd events)
                , Stay
                )

            else if Core.hasSelection model.studio then
                let
                    ( next, cmd ) =
                        runCore (Core.recolorSelection code) picked
                in
                ( withTidy next, Cmd.batch [ cmd, requestThumbnails next, sfx "snap" ], Stay )

            else
                ( picked, requestThumbnails picked, Stay )

        AddPart file ->
            let
                doc =
                    Core.getDocument model.studio

                matrix =
                    Editor.dropMatrix doc model.studio.selection file

                closed =
                    { model | activePart = file, notice = Nothing }
                        |> (\m ->
                                if isPartDrawerTab m.activeTab then
                                    m

                                else
                                    setTab m.lastPartTab m
                           )

                ( next, cmd ) =
                    runCore (Core.addPartAt file model.colour matrix) closed
            in
            ( withTidy next, Cmd.batch [ cmd, sfx "snap" ], Stay )

        Turn direction ->
            let
                ( next, cmd ) =
                    runCore (Core.turnSelected direction) model
            in
            ( withTidy next, Cmd.batch [ cmd, sfx "rotate" ], Stay )

        Raise ->
            let
                ( next, cmd ) =
                    runCore Core.raiseSelected model
            in
            ( withTidy next, cmd, Stay )

        Lower ->
            let
                ( next, cmd ) =
                    runCore Core.lowerSelected model
            in
            ( withTidy next, cmd, Stay )

        CopySelected ->
            stay (runCoreUnchecked (\s -> ( Core.copySelection s, Cmd.none, [] )) model)

        PasteSelected ->
            let
                ( next, cmd ) =
                    runCore Core.pasteClipboard model
            in
            ( withTidy next, cmd, Stay )

        DeleteSelected ->
            if baseSelected model then
                ( { model | notice = Just (T.editorKeepBaseText model.language) }, sfx "error", Stay )

            else
                let
                    ( next, cmd ) =
                        runCore Core.deleteSelection model
                in
                ( withTidy next, Cmd.batch [ cmd, sfx "delete" ], Stay )

        Undo ->
            let
                ( next, cmd ) =
                    runCoreUnchecked Core.undo model
            in
            ( withTidy { next | baseLocked = Editor.baseplateIntact (Core.getDocument next.studio) }
            , cmd
            , Stay
            )

        Redo ->
            let
                ( next, cmd ) =
                    runCoreUnchecked Core.redo model
            in
            ( withTidy { next | baseLocked = Editor.baseplateIntact (Core.getDocument next.studio) }
            , cmd
            , Stay
            )

        TidyUp ->
            if List.isEmpty model.tidyFixes then
                ( { model | notice = Just (T.tidyUpCleanLabel model.language) }, Cmd.none, Stay )

            else
                let
                    tryApply fixes =
                        let
                            ( nextStudio, cmd, events ) =
                                Core.applyFixes fixes model.studio

                            nextDoc =
                                Core.getDocument nextStudio
                        in
                        case Bounds.validateDocument nextDoc of
                            Ok _ ->
                                Just { studio = nextStudio, cmd = cmd, events = events, count = List.length fixes }

                            Err _ ->
                                Nothing

                    snapFixes =
                        List.filter (\f -> f.reason == Health.SnapToStud) model.tidyFixes

                    chosen =
                        case tryApply model.tidyFixes of
                            Just res ->
                                Just res

                            Nothing ->
                                if List.isEmpty snapFixes then
                                    Nothing

                                else
                                    tryApply snapFixes
                in
                case chosen of
                    Just res ->
                        ( withTidy
                            { model
                                | studio = res.studio
                                , notice = Just (T.tidyUpDoneNotice model.language res.count)
                            }
                        , Cmd.batch (Cmd.map StudioMsg res.cmd :: sfx "snap" :: List.map eventCmd res.events)
                        , Stay
                        )

                    Nothing ->
                        ( { model | notice = Just (T.tidyUpBoundsError model.language) }
                        , sfx "error"
                        , Stay
                        )

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

        CloseOverlayTab ->
            ( setTab model.lastPartTab model, Cmd.none, Stay )

        ToggleSearch ->
            let
                nextOpen =
                    not model.catalogSearchOpen
            in
            ( { model
                | catalogSearchOpen = nextOpen
                , catalogFilter =
                    if nextOpen then
                        model.catalogFilter

                    else
                        ""
              }
            , Cmd.none
            , Stay
            )

        ApplyTemplate tmpl ->
            let
                doc =
                    tmpl.document

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
                    setTab model.lastPartTab model
            in
            ( withTidy
                { closed
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

        NoOp ->
            ( model, Cmd.none, Stay )


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
                            setTab model.lastPartTab model
                    in
                    ( withTidy
                        { closed
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
                    ( { model | studio = studio, notice = Just (T.boundsIssueText model.language issue) }
                    , Cmd.batch (Cmd.map StudioMsg cmd :: List.map eventCmd events)
                    )

                else
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


dockHeightFor : Model -> Int
dockHeightFor model =
    if model.width < 768 then
        196

    else
        156


templatesDrawerHeight : Model -> Int
templatesDrawerHeight model =
    min 264 (max 210 (model.height // 2 - 20))


tabDrawerHeight : Model -> BuilderTab -> Int
tabDrawerHeight model tab =
    case tab of
        TemplatesTab ->
            templatesDrawerHeight model

        _ ->
            dockHeightFor model


canvasHeight : Model -> Int
canvasHeight model =
    max 180 (model.height - tabDrawerHeight model model.activeTab)


{-| Render the full 3D Element Editor screen.
-}
view : Model -> Html Msg
view model =
    let
        measured =
            model.measuredBounds

        hasViolations =
            model.hasBoundsViolation
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
        , bottomDock model
        , case model.activeTab of
            ImportBuildTab ->
                viewImportModal model

            LimitsTab ->
                viewLimitsModal model measured

            _ ->
                text ""
        ]


topBar : Model -> Int -> Bool -> Html Msg
topBar model heightPlates hasViolations =
    let
        lang =
            model.language

        measured =
            model.measuredBounds

        heightPct =
            min 100 (max 0 (round (toFloat heightPlates / toFloat Bounds.maxHeightPlates * 100)))

        meterColor =
            if hasViolations || heightPlates > Bounds.maxHeightPlates then
                Brand.brandRed

            else if heightPlates >= 28 then
                Brand.legoYellow

            else
                Brand.rainbowBrightLightBlue

        meterBorderClass =
            if hasViolations || heightPlates > Bounds.maxHeightPlates then
                "border-rose-400/70 text-rose-200"

            else if heightPlates >= 28 then
                "border-amber-300/60 text-amber-200"

            else if model.activeTab == LimitsTab then
                "border-brand-yellow text-white"

            else
                "border-white/15 text-slate-100 hover:border-white/30"

        coordTag =
            "(" ++ String.fromInt (model.coord.col + 1) ++ "," ++ String.fromInt (model.coord.row + 1) ++ ")"

        tidyCount =
            List.length model.tidyFixes

        hasTidyFixes =
            tidyCount > 0
    in
    div
        [ Attr.class "absolute top-0 left-0 right-0 z-30 flex flex-wrap items-center justify-between gap-2 p-2.5 sm:p-3 pointer-events-none"
        , Attr.attribute "data-testid" "builder-topbar"
        ]
        [ div [ Attr.class "pointer-events-auto flex items-center gap-1.5 min-w-0" ]
            [ button
                [ Attr.type_ "button"
                , onClick Finish
                , Attr.attribute "data-testid" "save-lot-button"
                , Attr.title (T.saveAndReturnButton lang)
                , Attr.attribute "aria-label" (T.saveAndReturnButton lang)
                , Attr.class "touch-target inline-flex items-center gap-1.5 h-10 px-3.5 sm:px-4 rounded-full bg-brand-yellow text-brand font-extrabold text-xs sm:text-sm cursor-pointer shadow-xl active:brightness-110 shrink-0"
                ]
                [ FeatherIcons.check |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml []
                , span [ Attr.class "hidden md:inline" ] [ text (T.saveAndReturnButton lang) ]
                , span [ Attr.class "md:hidden" ] [ text (T.saveAndReturnShort lang) ]
                ]
            , button
                [ Attr.type_ "button"
                , onClick Cancel
                , Attr.attribute "data-testid" "cancel-lot-button"
                , Attr.title (T.cancelButton lang)
                , Attr.attribute "aria-label" (T.cancelButton lang)
                , Attr.class "touch-target inline-flex items-center justify-center w-10 h-10 rounded-full bg-slate-900/80 backdrop-blur-md border border-white/15 text-slate-200 cursor-pointer shadow-lg hover:bg-slate-800/90 shrink-0"
                ]
                [ FeatherIcons.x |> FeatherIcons.withSize 17 |> FeatherIcons.toHtml [] ]
            , div [ Attr.class "inline-flex items-center gap-1.5 h-10 px-3 rounded-full bg-slate-900/80 backdrop-blur-md border border-white/15 shadow-lg min-w-0" ]
                [ span [ Attr.class "text-[11px] font-mono font-bold text-brand-yellow shrink-0" ]
                    [ text coordTag ]
                , input
                    [ Attr.type_ "text"
                    , Attr.value model.elementName
                    , onInput NameChanged
                    , Attr.attribute "aria-label" (T.tr lang "Element name" "Elementin nimi")
                    , Attr.attribute "data-testid" "element-name-input"
                    , Attr.class "w-24 sm:w-36 bg-transparent border-b border-transparent focus:border-brand-yellow outline-none text-xs sm:text-sm font-bold text-white truncate"
                    ]
                    []
                , span [ Attr.class "text-slate-400 hidden sm:inline shrink-0" ]
                    [ FeatherIcons.edit |> FeatherIcons.withSize 12 |> FeatherIcons.toHtml [] ]
                ]
            ]
        , div [ Attr.class "pointer-events-auto flex items-center gap-1.5 ml-auto" ]
            [ button
                [ Attr.type_ "button"
                , onClick TidyUp
                , Attr.attribute "data-testid" "top-tidy-button"
                , Attr.title
                    (if hasTidyFixes then
                        T.tidyUpLabel lang ++ " (" ++ String.fromInt tidyCount ++ ")"

                     else
                        T.tidyUpCleanLabel lang
                    )
                , Attr.attribute "aria-label"
                    (if hasTidyFixes then
                        T.tidyUpLabel lang ++ " (" ++ String.fromInt tidyCount ++ ")"

                     else
                        T.tidyUpCleanLabel lang
                    )
                , Attr.disabled (not hasTidyFixes)
                , Attr.class
                    ("touch-target inline-flex items-center gap-1.5 h-10 px-2.5 sm:px-3 rounded-full border text-xs font-extrabold whitespace-nowrap shrink-0 transition-all "
                        ++ (if hasTidyFixes then
                                "bg-brand-yellow border-brand-yellow text-brand cursor-pointer shadow-lg animate-pulse"

                            else
                                "bg-slate-900/80 border-white/15 text-slate-400 opacity-60 cursor-default"
                           )
                    )
                ]
                [ (if hasTidyFixes then
                    FeatherIcons.crosshair

                   else
                    FeatherIcons.checkCircle
                  )
                    |> FeatherIcons.withSize 14
                    |> FeatherIcons.toHtml []
                , span [ Attr.class "hidden lg:inline" ]
                    [ text
                        (if hasTidyFixes then
                            T.tidyUpLabel lang ++ " (" ++ String.fromInt tidyCount ++ ")"

                         else
                            T.tidyUpLabel lang
                        )
                    ]
                , if hasTidyFixes then
                    span [ Attr.class "lg:hidden font-mono" ] [ text (String.fromInt tidyCount) ]

                  else
                    text ""
                ]
            , div
                [ Attr.class "inline-flex items-center gap-1.5 h-10 px-2.5 sm:px-3 rounded-full bg-slate-900/80 backdrop-blur-md border border-white/15 shadow-lg shrink-0"
                , Attr.attribute "data-testid" "part-counter"
                , Attr.title (T.editorPartsLabel lang measured.partCount)
                , Attr.attribute "aria-label" (T.editorPartsLabel lang measured.partCount)
                ]
                [ span [ Attr.class "text-slate-400 shrink-0" ]
                    [ FeatherIcons.box |> FeatherIcons.withSize 13 |> FeatherIcons.toHtml [] ]
                , span [ Attr.class "text-xs font-mono font-bold text-white tabular-nums" ]
                    [ text (String.fromInt measured.partCount ++ " " ++ T.tr lang "pcs" "kpl") ]
                ]
            , button
                [ Attr.type_ "button"
                , onClick (SelectTab LimitsTab)
                , Attr.attribute "data-testid" "height-indicator"
                , Attr.attribute "data-limits-toggle" ""
                , Attr.title (T.editorLimitsTab lang)
                , Attr.attribute "aria-label" (T.editorHeightLabel lang heightPlates)
                , Attr.class ("touch-target inline-flex items-center gap-2.5 h-10 px-3 rounded-full bg-slate-900/80 backdrop-blur-md border shadow-lg cursor-pointer transition-all " ++ meterBorderClass)
                ]
                [ span [ Attr.class "text-xs font-mono font-bold tabular-nums whitespace-nowrap" ]
                    [ text (T.heightMeterLabel lang heightPlates) ]
                , div
                    [ Attr.attribute "data-testid" "height-progress-bar"
                    , Attr.class "w-14 sm:w-24 h-2 rounded-full bg-slate-950/90 border border-white/10 overflow-hidden shrink-0"
                    ]
                    [ div
                        [ Attr.class "h-full rounded-full transition-all duration-200"
                        , Attr.style "width" (String.fromInt heightPct ++ "%")
                        , Attr.style "background" meterColor
                        ]
                        []
                    ]
                ]
            , div [ Attr.class "inline-flex items-center gap-1 p-1 rounded-full bg-slate-900/80 backdrop-blur-md border border-white/15 shadow-lg" ]
                [ button
                    [ Attr.type_ "button"
                    , onClick (SelectTab ImportBuildTab)
                    , Attr.attribute "data-import-toggle" ""
                    , Attr.title (T.editorOpenTab lang)
                    , Attr.attribute "aria-label" (T.editorOpenTab lang)
                    , Attr.class
                        ("touch-target w-8 h-8 inline-flex items-center justify-center rounded-full cursor-pointer transition-all "
                            ++ (if model.activeTab == ImportBuildTab then
                                    "bg-brand-yellow text-brand"

                                else
                                    "text-slate-200 hover:bg-white/10"
                               )
                        )
                    ]
                    [ FeatherIcons.share2 |> FeatherIcons.withSize 15 |> FeatherIcons.toHtml [] ]
                , button
                    [ Attr.type_ "button"
                    , onClick FitView
                    , Attr.title (T.fitViewLabel lang)
                    , Attr.attribute "aria-label" (T.fitViewLabel lang)
                    , Attr.class "touch-target w-8 h-8 inline-flex items-center justify-center rounded-full text-slate-200 hover:bg-white/10 cursor-pointer transition-all"
                    ]
                    [ FeatherIcons.maximize |> FeatherIcons.withSize 15 |> FeatherIcons.toHtml [] ]
                ]
            ]
        ]


noticeView : Model -> Html Msg
noticeView model =
    case model.notice of
        Just message ->
            div
                [ Attr.attribute "role" "status"
                , Attr.attribute "data-testid" "builder-notice"
                , Attr.class "pointer-events-none absolute left-1/2 -translate-x-1/2 top-16 z-30 max-w-md px-4 py-1.5 rounded-full bg-slate-900/90 backdrop-blur-md border border-brand-yellow/50 text-brand-yellow text-xs font-bold text-center shadow-xl"
                ]
                [ text message ]

        Nothing ->
            text ""


bottomDock : Model -> Html Msg
bottomDock model =
    let
        isTemplates =
            model.activeTab == TemplatesTab

        activePartTab =
            if isPartDrawerTab model.activeTab then
                model.activeTab

            else
                model.lastPartTab

        height =
            tabDrawerHeight model model.activeTab
    in
    div
        [ Attr.attribute "data-testid" "builder-dock"
        , Attr.class "absolute left-0 right-0 bottom-0 z-20 flex flex-col gap-2 px-3 py-2 bg-slate-900/85 backdrop-blur-md border-t border-white/15 shadow-2xl box-border"
        , Attr.style "height" (px height)
        ]
        [ dockHeaderRow model
        , if isTemplates then
            templatesDrawerBody model

          else
            partsDrawerBody model activePartTab
        ]


dockHeaderRow : Model -> Html Msg
dockHeaderRow model =
    let
        lang =
            model.language

        isTemplates =
            model.activeTab == TemplatesTab

        activeDockTab =
            if isTemplates then
                TemplatesTab

            else if isPartDrawerTab model.activeTab then
                model.activeTab

            else
                model.lastPartTab

        dockTabBtn tab testId label =
            let
                active =
                    activeDockTab == tab
            in
            button
                [ Attr.type_ "button"
                , onClick (SelectTab tab)
                , Attr.attribute "data-testid" testId
                , Attr.attribute "aria-pressed" (boolAttr active)
                , Attr.class
                    ("touch-target px-3 h-8 sm:h-9 rounded-full text-xs font-bold whitespace-nowrap cursor-pointer transition-all "
                        ++ (if active then
                                "bg-brand-yellow text-brand shadow-sm font-extrabold"

                            else
                                "text-slate-300 hover:text-white hover:bg-white/10"
                           )
                    )
                ]
                [ text label ]
    in
    div [ Attr.class "flex flex-wrap items-center justify-between gap-2 pb-1.5 border-b border-white/10 shrink-0" ]
        [ div [ Attr.class "inline-flex items-center gap-1 p-1 rounded-full bg-slate-950/65 border border-white/10 overflow-x-auto palette-scroll-x max-w-full" ]
            [ dockTabBtn BricksTab "dock-tab-bricks" (T.tabBricksLabel lang)
            , dockTabBtn PlatesTilesTab "dock-tab-plates" (T.tabPlatesTilesLabel lang)
            , dockTabBtn SnotSlopesTab "dock-tab-snot" (T.tabSnotSlopesLabel lang)
            , dockTabBtn TemplatesTab "dock-tab-templates" (T.tabTemplatesLabel lang)
            ]
        , dockActionToolbar model (not isTemplates)
        ]


dockActionToolbar : Model -> Bool -> Html Msg
dockActionToolbar model showPartTools =
    let
        lang =
            model.language

        hasSelection =
            Core.hasSelection model.studio

        onBase =
            baseSelected model

        tidyCount =
            List.length model.tidyFixes

        wide =
            model.width >= 640
    in
    div
        [ Attr.attribute "role" "toolbar"
        , Attr.class "flex items-center gap-1 overflow-x-auto palette-scroll-x max-w-full"
        ]
        [ toolButton "undo-button" (T.tr lang "Undo" "Kumoa") FeatherIcons.cornerUpLeft (enabledIf (Core.canUndo model.studio) Undo)
        , toolButton "redo-button" (T.tr lang "Redo" "Tee uudelleen") FeatherIcons.cornerUpRight (enabledIf (Core.canRedo model.studio) Redo)
        , tidyUpButton lang tidyCount
        , boundsSafetyBadge model
        , span [ Attr.class "w-px h-5 bg-white/15 mx-0.5 shrink-0" ] []
        , toolButton "turn-left-button" (T.tr lang "Turn left" "Käännä vasemmalle") FeatherIcons.rotateCcw (enabledIf (hasSelection && not onBase) (Turn TurnLeft))
        , toolButton "turn-right-button" (T.tr lang "Turn right" "Käännä oikealle") FeatherIcons.rotateCw (enabledIf (hasSelection && not onBase) (Turn TurnRight))
        , toolButton "raise-button" (T.tr lang "Raise" "Nosta") FeatherIcons.arrowUp (enabledIf (hasSelection && not onBase) Raise)
        , toolButton "lower-button" (T.tr lang "Lower" "Laske") FeatherIcons.arrowDown (enabledIf (hasSelection && not onBase) Lower)
        , if wide then
            toolButton "copy-part-button" (T.tr lang "Copy" "Kopioi") FeatherIcons.copy (enabledIf (hasSelection && not onBase) CopySelected)

          else
            text ""
        , if wide then
            toolButton "paste-part-button" (T.tr lang "Paste" "Liitä") FeatherIcons.clipboard (Just PasteSelected)

          else
            text ""
        , toolButton
            "delete-part-button"
            (if onBase then
                T.editorKeepBaseText lang

             else
                T.tr lang "Delete" "Poista"
            )
            FeatherIcons.trash2
            (enabledIf (hasSelection && not onBase) DeleteSelected)
        , if showPartTools then
            div [ Attr.class "flex items-center gap-1 pl-1 ml-0.5 border-l border-white/15 shrink-0" ]
                [ searchToggleButton model
                , lockButton model
                ]

          else
            text ""
        ]


boundsSafetyBadge : Model -> Html Msg
boundsSafetyBadge model =
    let
        lang =
            model.language

        hasViolation =
            model.hasBoundsViolation

        nearCeiling =
            model.measuredBounds.heightPlates >= 28

        ( icon, textLabel, badgeClass ) =
            if hasViolation then
                ( FeatherIcons.alertTriangle
                , T.boundsExceededShort lang
                , "bg-rose-500/20 border-rose-400/50 text-rose-300 hover:bg-rose-500/30"
                )

            else if nearCeiling then
                ( FeatherIcons.alertCircle
                , T.boundsCeilingShort lang
                , "bg-amber-500/20 border-amber-400/50 text-amber-300 hover:bg-amber-500/30"
                )

            else
                ( FeatherIcons.shield
                , T.boundsSafeShort lang
                , "bg-emerald-500/15 border-emerald-400/30 text-emerald-300 hover:bg-emerald-500/25"
                )
    in
    button
        [ Attr.type_ "button"
        , onClick (SelectTab LimitsTab)
        , Attr.attribute "data-testid" "bounds-safety-indicator"
        , Attr.title (T.editorBoundsOk lang)
        , Attr.attribute "aria-label" (T.editorBoundsOk lang)
        , Attr.class ("touch-target inline-flex items-center gap-1.5 h-9 px-2.5 rounded-full border text-xs font-bold cursor-pointer transition-all shrink-0 " ++ badgeClass)
        ]
        [ icon |> FeatherIcons.withSize 13 |> FeatherIcons.toHtml []
        , span [ Attr.class "hidden sm:inline font-mono text-[11px]" ] [ text textLabel ]
        ]


tidyUpButton : Language -> Int -> Html Msg
tidyUpButton lang tidyCount =
    let
        hasFixes =
            tidyCount > 0

        label =
            if hasFixes then
                T.tidyUpLabel lang ++ " (" ++ String.fromInt tidyCount ++ ")"

            else
                T.tidyUpLabel lang

        tip =
            if hasFixes then
                label

            else
                T.tidyUpCleanLabel lang
    in
    button
        [ Attr.type_ "button"
        , onClick TidyUp
        , Attr.attribute "data-testid" "tidy-up-button"
        , Attr.title tip
        , Attr.attribute "aria-label" tip
        , Attr.disabled (not hasFixes)
        , Attr.class
            ("touch-target inline-flex items-center gap-1.5 h-9 px-2.5 rounded-full border text-xs font-extrabold whitespace-nowrap shrink-0 transition-all "
                ++ (if hasFixes then
                        "bg-brand-yellow border-brand-yellow text-brand cursor-pointer shadow-md"

                    else
                        "bg-white/5 border-white/10 text-slate-400 opacity-55 cursor-default"
                   )
            )
        ]
        [ (if hasFixes then
            FeatherIcons.crosshair

           else
            FeatherIcons.checkCircle
          )
            |> FeatherIcons.withSize 14
            |> FeatherIcons.toHtml []
        , span [ Attr.class "hidden sm:inline" ] [ text label ]
        , if hasFixes then
            span [ Attr.class "sm:hidden font-mono" ] [ text (String.fromInt tidyCount) ]

          else
            text ""
        ]


toolButton : String -> String -> FeatherIcons.Icon -> Maybe Msg -> Html Msg
toolButton testId label icon maybeMsg =
    let
        enabled =
            maybeMsg /= Nothing
    in
    button
        ([ Attr.type_ "button"
         , Attr.attribute "data-testid" testId
         , Attr.attribute "aria-label" label
         , Attr.title label
         , Attr.disabled (not enabled)
         , Attr.class
            ("touch-target inline-flex items-center justify-center w-9 h-9 shrink-0 rounded-full border transition-all "
                ++ (if enabled then
                        "bg-slate-800/80 border-white/15 text-slate-100 cursor-pointer hover:bg-slate-700"

                    else
                        "bg-slate-900/35 border-white/5 text-slate-500 opacity-40 cursor-default"
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
        [ icon |> FeatherIcons.withSize 15 |> FeatherIcons.toHtml [] ]


searchToggleButton : Model -> Html Msg
searchToggleButton model =
    let
        label =
            T.searchPartsPlaceholder model.language

        active =
            model.catalogSearchOpen || not (String.isEmpty (String.trim model.catalogFilter))
    in
    button
        [ Attr.type_ "button"
        , onClick ToggleSearch
        , Attr.attribute "data-testid" "toggle-search-button"
        , Attr.title label
        , Attr.attribute "aria-label" label
        , Attr.class
            ("touch-target inline-flex items-center justify-center w-9 h-9 shrink-0 rounded-full border cursor-pointer transition-all "
                ++ (if active then
                        "bg-brand-yellow border-brand-yellow text-brand"

                    else
                        "bg-slate-800/80 border-white/15 text-slate-200 hover:bg-slate-700"
                   )
            )
        ]
        [ FeatherIcons.search |> FeatherIcons.withSize 15 |> FeatherIcons.toHtml [] ]


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
            ("touch-target inline-flex items-center justify-center w-9 h-9 shrink-0 rounded-full border cursor-pointer transition-all "
                ++ (if model.baseLocked then
                        "bg-slate-800/80 border-white/15 text-slate-300 hover:bg-slate-700"

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
            |> FeatherIcons.withSize 15
            |> FeatherIcons.toHtml []
        ]


partsDrawerBody : Model -> BuilderTab -> Html Msg
partsDrawerBody model tab =
    let
        lang =
            model.language

        drawer =
            case tab of
                PlatesTilesTab ->
                    PlatesTilesDrawer

                SnotSlopesTab ->
                    SnotSlopesDrawer

                _ ->
                    BricksDrawer

        categories =
            Editor.categoriesForDrawer drawer

        query =
            String.toLower (String.trim model.catalogFilter)

        matches entry =
            String.isEmpty query
                || String.contains query (String.toLower entry.nameEn)
                || String.contains query (String.toLower entry.nameFi)
                || String.contains query (String.toLower entry.file)

        renderGroup cat =
            let
                filtered =
                    List.filter matches cat.parts
            in
            if List.isEmpty filtered then
                Nothing

            else
                Just
                    (div [ Attr.class "flex items-center gap-2 shrink-0" ]
                        [ span [ Attr.class "px-2 py-1 rounded-full bg-white/5 border border-white/10 text-[10px] font-bold uppercase tracking-wider text-slate-400 whitespace-nowrap" ]
                            [ text (T.tr lang cat.titleEn cat.titleFi) ]
                        , div [ Attr.class "flex items-center gap-2" ]
                            (List.map (partButton model) filtered)
                        ]
                    )

        visibleItems =
            if String.isEmpty query then
                List.filterMap renderGroup categories

            else
                List.filterMap renderGroup Editor.partCategories
    in
    div [ Attr.class "flex items-center gap-3 flex-1 min-h-0 overflow-hidden" ]
        [ colourGrid model
        , if model.catalogSearchOpen then
            div [ Attr.class "flex items-center gap-1.5 px-2.5 h-9 rounded-full bg-slate-950/80 border border-white/15 shrink-0" ]
                [ FeatherIcons.search |> FeatherIcons.withSize 13 |> FeatherIcons.toHtml [ Attr.class "text-slate-400" ]
                , input
                    [ Attr.type_ "text"
                    , Attr.value model.catalogFilter
                    , onInput CatalogFilterChanged
                    , Attr.placeholder (T.searchPartsPlaceholder lang)
                    , Attr.attribute "data-testid" "part-search-input"
                    , Attr.class "w-28 sm:w-40 bg-transparent text-xs text-white placeholder:text-slate-400 outline-none"
                    ]
                    []
                ]

          else
            text ""
        , div [ Attr.class "palette-scroll-x flex items-center gap-3 flex-1 min-w-0 overflow-x-auto py-0.5" ]
            (if List.isEmpty visibleItems then
                [ span [ Attr.class "text-xs text-slate-400 px-2" ]
                    [ text (T.tr lang "No matching parts." "Ei vastaavia paloja.") ]
                ]

             else
                visibleItems
            )
        ]


colourGrid : Model -> Html Msg
colourGrid model =
    div
        [ Attr.class "grid grid-cols-8 gap-1.5 shrink-0 p-1.5 rounded-2xl bg-slate-950/60 border border-white/10"
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
        , Attr.attribute "aria-selected" (boolAttr selected)
        , Attr.style "background" bg
        , Attr.class
            ("w-5 h-5 rounded-full cursor-pointer transition-transform "
                ++ (if selected then
                        "ring-2 ring-brand-yellow scale-110 shadow-sm"

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


templatesDrawerBody : Model -> Html Msg
templatesDrawerBody model =
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
                    ("flex flex-col items-start gap-1.5 p-3 rounded-2xl border text-left cursor-pointer transition-all "
                        ++ (if active then
                                "bg-brand-yellow/15 border-brand-yellow text-white shadow-md"

                            else
                                "bg-slate-800/70 border-white/10 text-slate-100 hover:border-white/30 hover:bg-slate-800"
                           )
                    )
                ]
                [ div [ Attr.class "flex items-center justify-between w-full gap-2" ]
                    [ span [ Attr.class "text-xs sm:text-sm font-extrabold truncate" ] [ text titleText ]
                    , span [ Attr.class "px-2 py-0.5 rounded-full bg-slate-950/70 text-[10px] font-mono font-bold text-sky-300 shrink-0" ]
                        [ text (T.heightUnit lang tmpl.heightPlates) ]
                    ]
                , p [ Attr.class "text-[11px] text-slate-300 leading-snug line-clamp-2" ] [ text descText ]
                , span [ Attr.class "text-[10px] text-slate-400 font-semibold" ]
                    [ text (T.categoryLabel lang tmpl.category ++ " • " ++ T.partCountShort lang tmpl.partCount) ]
                ]
    in
    div [ Attr.class "palette-scroll-y flex-1 overflow-y-auto pr-1" ]
        [ div [ Attr.class "grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-2" ]
            (List.map card Templates.all)
        ]


viewImportModal : Model -> Html Msg
viewImportModal model =
    let
        lang =
            model.language
    in
    modalOverlay (T.editorOpenTab lang)
        [ div [ Attr.class "flex flex-wrap gap-2" ]
            [ button
                [ Attr.type_ "button"
                , onClick UploadLdrawFile
                , Attr.class "touch-target inline-flex items-center gap-2 h-10 px-4 rounded-xl bg-slate-800/90 border border-white/15 text-xs font-bold text-white cursor-pointer hover:bg-slate-700"
                ]
                [ FeatherIcons.upload |> FeatherIcons.withSize 15 |> FeatherIcons.toHtml []
                , span [] [ text (T.uploadLdrawLabel lang) ]
                ]
            , button
                [ Attr.type_ "button"
                , onClick CopyBuildLink
                , Attr.class "touch-target inline-flex items-center gap-2 h-10 px-4 rounded-xl bg-brand-yellow/20 border border-brand-yellow/50 text-xs font-bold text-brand-yellow cursor-pointer hover:bg-brand-yellow/30"
                ]
                [ FeatherIcons.share2 |> FeatherIcons.withSize 15 |> FeatherIcons.toHtml []
                , span [] [ text (T.tr lang "Copy #b/... build link" "Kopioi #b/...-linkki") ]
                ]
            ]
        , p [ Attr.class "text-xs text-slate-300" ] [ text (T.pasteBuildHint lang) ]
        , textarea
            [ Attr.value model.pasteText
            , onInput PasteTextChanged
            , Attr.rows 3
            , Attr.class "w-full p-3 rounded-xl bg-slate-950/90 border border-white/15 text-xs font-mono text-white outline-none focus:border-brand-yellow"
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
            , Attr.class "touch-target self-start h-10 px-4 rounded-full bg-brand-yellow text-brand text-xs font-extrabold cursor-pointer shadow-md"
            ]
            [ text (T.pasteButtonLabel lang) ]
        ]


viewLimitsModal : Model -> Bounds.DocumentBounds -> Html Msg
viewLimitsModal model measured =
    let
        lang =
            model.language

        doc =
            Core.getDocument model.studio

        validation =
            Bounds.validateDocument doc

        healthIssues =
            Health.check model.studio.viewport.partCache doc (baseplateIds doc)

        heightPct =
            min 100 (round (toFloat measured.heightPlates / toFloat Bounds.maxHeightPlates * 100))

        barColor =
            if measured.heightPlates > Bounds.maxHeightPlates then
                Brand.brandRed

            else if measured.heightPlates >= 28 then
                Brand.legoYellow

            else
                Brand.rainbowBrightLightBlue
    in
    modalOverlay (T.editorLimitsTab lang)
        [ div [ Attr.class "grid grid-cols-2 gap-2.5" ]
            [ div [ Attr.class "p-3 rounded-2xl bg-slate-800/75 border border-white/10" ]
                [ p [ Attr.class "text-xs font-semibold text-slate-300" ] [ text (T.editorHeightLabel lang measured.heightPlates) ]
                , div [ Attr.class "mt-2 h-2.5 rounded-full bg-slate-950 overflow-hidden" ]
                    [ div
                        [ Attr.class "h-full rounded-full transition-all duration-200"
                        , Attr.style "width" (String.fromInt heightPct ++ "%")
                        , Attr.style "background" barColor
                        ]
                        []
                    ]
                ]
            , div [ Attr.class "p-3 rounded-2xl bg-slate-800/75 border border-white/10" ]
                [ p [ Attr.class "text-xs font-semibold text-slate-300" ] [ text (T.editorPartsLabel lang measured.partCount) ]
                , p [ Attr.class "mt-1.5 text-[11px] font-mono text-slate-300" ]
                    [ text ("X: " ++ String.fromInt (round measured.minX) ++ ".." ++ String.fromInt (round measured.maxX) ++ " • Z: " ++ String.fromInt (round measured.minZ) ++ ".." ++ String.fromInt (round measured.maxZ)) ]
                ]
            ]
        , case validation of
            Ok _ ->
                p [ Attr.class "p-3 rounded-2xl bg-emerald-950/60 border border-emerald-500/40 text-xs text-emerald-300 font-semibold" ]
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
        , div [ Attr.class "flex items-center justify-between gap-2 pt-1 text-xs text-slate-400" ]
            [ span []
                [ text
                    (T.tr lang
                        ("Connectivity check: " ++ String.fromInt (List.length healthIssues) ++ " loose/overlap notes.")
                        ("Kiinnitystarkistus: " ++ String.fromInt (List.length healthIssues) ++ " irrallista tai päällekkäistä huomautusta.")
                    )
                ]
            , if not (List.isEmpty model.tidyFixes) then
                button
                    [ Attr.type_ "button"
                    , onClick TidyUp
                    , Attr.class "px-3 py-1.5 rounded-full bg-brand-yellow text-brand font-extrabold text-xs cursor-pointer shrink-0"
                    ]
                    [ text (T.tidyUpLabel lang ++ " (" ++ String.fromInt (List.length model.tidyFixes) ++ ")") ]

              else
                text ""
            ]
        ]


modalOverlay : String -> List (Html Msg) -> Html Msg
modalOverlay heading body =
    div
        [ Attr.class "fixed inset-0 z-40 flex items-center justify-center bg-black/55 backdrop-blur-sm p-4"
        , onClick CloseOverlayTab
        ]
        [ div
            [ Attr.class "w-full max-w-md p-5 rounded-3xl bg-slate-900/90 backdrop-blur-xl border border-white/15 shadow-2xl flex flex-col gap-3.5"
            , Html.Events.stopPropagationOn "click" (Decode.succeed ( NoOp, True ))
            ]
            [ div [ Attr.class "flex items-center justify-between" ]
                [ h3 [ Attr.class "text-sm font-extrabold text-white" ] [ text heading ]
                , button
                    [ Attr.type_ "button"
                    , onClick CloseOverlayTab
                    , Attr.class "touch-target w-8 h-8 flex items-center justify-center rounded-full bg-white/10 text-slate-300 cursor-pointer hover:bg-white/20"
                    ]
                    [ FeatherIcons.x |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
                ]
            , div [ Attr.class "flex flex-col gap-3" ] body
            ]
        ]


boolAttr : Bool -> String
boolAttr on =
    if on then
        "true"

    else
        "false"


enabledIf : Bool -> msg -> Maybe msg
enabledIf ok msg =
    if ok then
        Just msg

    else
        Nothing


px : Int -> String
px n =
    String.fromInt n ++ "px"
