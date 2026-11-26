port module Ports exposing
    ( cityJsonImported
    , cityRemoteLotUpdated
    , cityRemoteSnapshot
    , cityRoomError
    , cityRoomStateChanged
    , createCityRoom
    , downloadCityFile
    , joinCityRoom
    , leaveCityRoom
    , playSfx
    , requestCityJsonImport
    , requestThumbnails
    , saveCity
    , sendCitySnapshot
    , sendLotFocus
    , sendLotUpdate
    , thumbnailLoaded
    )

{-| Elm ports specific to Brick City (`brick-city`):

  - Local persistence (`saveCity`), JSON import (`requestCityJsonImport` /
    `cityJsonImported`), and file download (`downloadCityFile` for `.mpd` / `.json`).
  - Collaborative room synchronization (`createCityRoom`, `joinCityRoom`,
    `leaveCityRoom`, `sendCitySnapshot`, `sendLotUpdate`, `sendLotFocus` and
    incoming room/snapshot/lot subscriptions).
  - Editor dock thumbnails (`requestThumbnails` / `thumbnailLoaded`) and
    tactile brick SFX (`playSfx`).

Shared LDraw asset/worker/clipboard ports come from `Game.Ports`.

-}

import Json.Decode as Decode
import Json.Encode as Encode


{-| Trigger a procedural Web Audio sound effect in `audio.js`.
-}
port playSfx : Encode.Value -> Cmd msg


{-| Request rendered part thumbnails from `thumbnail-pipeline.js`.
-}
port requestThumbnails : List { file : String, color : Int, rgb : List Float } -> Cmd msg


{-| Receive a rendered part thumbnail data URL.
-}
port thumbnailLoaded : ({ file : String, color : Int, url : String } -> msg) -> Sub msg


{-| Persist serialized `CityGrid` JSON to `localStorage`.
-}
port saveCity : String -> Cmd msg


{-| Trigger browser download of an `.mpd` or `.json` file.
-}
port downloadCityFile : { filename : String, content : String, mimeType : String } -> Cmd msg


{-| Open a browser file picker for `.json` city files.
-}
port requestCityJsonImport : () -> Cmd msg


{-| Receive the text content of an imported `.json` city file.
-}
port cityJsonImported : (String -> msg) -> Sub msg


{-| Create a collaborative city room as host.
-}
port createCityRoom : { roomId : String, name : String, city : Encode.Value } -> Cmd msg


{-| Join an existing collaborative city room by code.
-}
port joinCityRoom : { roomId : String, name : String } -> Cmd msg


{-| Leave the active collaborative city room.
-}
port leaveCityRoom : () -> Cmd msg


{-| Send an incremental lot update to the collaborative room.
-}
port sendLotUpdate : Encode.Value -> Cmd msg


{-| Broadcast a full city replacement (e.g. grid resize, template town, rename).
-}
port sendCitySnapshot : Encode.Value -> Cmd msg


{-| Broadcast which lot the local builder currently has selected.
-}
port sendLotFocus : Encode.Value -> Cmd msg


{-| Incoming room state update (roomId, role, status, peers).
-}
port cityRoomStateChanged : (Decode.Value -> msg) -> Sub msg


{-| Incoming full city snapshot from the collaborative room host.
-}
port cityRemoteSnapshot : (Decode.Value -> msg) -> Sub msg


{-| Incoming incremental lot update from another collaborator.
-}
port cityRemoteLotUpdated : (Decode.Value -> msg) -> Sub msg


{-| Incoming collaborative room error message.
-}
port cityRoomError : (String -> msg) -> Sub msg
