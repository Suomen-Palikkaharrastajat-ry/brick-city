module City.Types exposing
    ( BoundsIssue(..)
    , CityGrid
    , CityRoom
    , CityStats
    , ElementCategory(..)
    , LotCoord
    , LotElement
    , LotState(..)
    , PeerPresence
    , RoomConnection(..)
    , RoomRole(..)
    , ViewMode(..)
    , categoryFromString
    , categoryToString
    , lotKey
    )

{-| Core domain types for Brick City (`brick-city`):
the expandable microscale city grid, 8x8 lot elements, validation issues,
city statistics, and real-time room collaboration state.
-}

import Dict exposing (Dict)


{-| Coordinates of one 8x8 lot in the city grid (0-indexed).
-}
type alias LotCoord =
    { row : Int
    , col : Int
    }


{-| Canonical dictionary key for a lot coordinate `(row, col)`.
-}
lotKey : LotCoord -> ( Int, Int )
lotKey coord =
    ( coord.row, coord.col )


{-| Microscale city district classification for an 8x8 element.
-}
type ElementCategory
    = Residential
    | Commercial
    | Civic
    | Park
    | Road
    | Industrial
    | Waterfront
    | Custom


{-| Convert an `ElementCategory` to its stable serialization identifier.
-}
categoryToString : ElementCategory -> String
categoryToString category =
    case category of
        Residential ->
            "residential"

        Commercial ->
            "commercial"

        Civic ->
            "civic"

        Park ->
            "park"

        Road ->
            "road"

        Industrial ->
            "industrial"

        Waterfront ->
            "waterfront"

        Custom ->
            "custom"


{-| Parse an `ElementCategory` from its string identifier.
-}
categoryFromString : String -> ElementCategory
categoryFromString raw =
    case String.toLower (String.trim raw) of
        "residential" ->
            Residential

        "commercial" ->
            Commercial

        "civic" ->
            Civic

        "park" ->
            Park

        "road" ->
            Road

        "industrial" ->
            Industrial

        "waterfront" ->
            Waterfront

        _ ->
            Custom


{-| One 8x8 stud microscale city element placed on a lot.
-}
type alias LotElement =
    { id : String
    , name : String
    , category : ElementCategory
    , ldrawText : String
    , templateId : Maybe String
    , rotation : Int
    , updatedAt : Int
    , updatedBy : String
    , partCount : Int
    , heightPlates : Int
    }


{-| Contents of a lot in the city grid.
-}
type LotState
    = EmptyLot
    | BuiltLot LotElement


{-| Expandable city grid of 8x8 microscale lots.
-}
type alias CityGrid =
    { cityName : String
    , cols : Int
    , rows : Int
    , lots : Dict ( Int, Int ) LotElement
    , version : Int
    }


{-| Aggregate metrics computed across the city grid.
-}
type alias CityStats =
    { population : Int
    , buildings : Int
    , parks : Int
    , roads : Int
    , waterfronts : Int
    , industries : Int
    , totalParts : Int
    , builtLots : Int
    , totalLots : Int
    , score : Int
    , roadConnections : Int
    , maxHeightPlates : Int
    }


{-| Another builder (or yourself) connected in a collaborative city room.
-}
type alias PeerPresence =
    { slot : Int
    , id : String
    , name : String
    , color : String
    , focusedLot : Maybe LotCoord
    }


{-| Player role in a collaborative city room.
-}
type RoomRole
    = LocalSolo
    | RoomHost
    | RoomGuest


{-| Connection status of the collaborative room transport.
-}
type RoomConnection
    = Disconnected
    | Connecting
    | Connected
    | RoomError String


{-| Collaborative room state mirrored from `city-sync.js`.
-}
type alias CityRoom =
    { roomId : String
    , role : RoomRole
    , status : RoomConnection
    , mySlot : Int
    , myName : String
    , peers : List PeerPresence
    }


{-| Active top-level workspace mode.
-}
type ViewMode
    = ViewCity
    | ViewLotEditor


{-| Validation failure for an 8x8 microscale element.
-}
type BoundsIssue
    = EmptyElement
    | TooManyParts Int
    | OutOfBoundsX Float Float
    | OutOfBoundsZ Float Float
    | ExceedsMaxHeight Int
    | BelowBaseplate Float
