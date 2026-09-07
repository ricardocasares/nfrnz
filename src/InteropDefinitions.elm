module InteropDefinitions exposing (Flags, FromElm(..), ToElm(..), interop)

import Json.Encode
import Tuple
import TsJson.Decode as TsDecode exposing (Decoder)
import TsJson.Encode as TsEncode exposing (Encoder, optional, required)


interop :
    { toElm : Decoder ToElm
    , fromElm : Encoder FromElm
    , flags : Decoder Flags
    }
interop =
    { toElm = toElm
    , fromElm = fromElm
    , flags = flags
    }


type FromElm
    = StorageSet String String


type ToElm
    = AuthenticatedUser User


type alias User =
    { username : String }


type alias Flags =
    { theme : String }


fromElm : Encoder FromElm
fromElm =
    TsEncode.union
        (\vStorageSet value ->
            case value of
                StorageSet key storageValue ->
                    vStorageSet ( key, storageValue )
        )
        |> TsEncode.variantTagged "storageSet"
            (TsEncode.object
                [ required "key" Tuple.first TsEncode.string
                , required "value" Tuple.second TsEncode.string
                ]
            )
        |> TsEncode.buildUnion


toElm : Decoder ToElm
toElm =
    TsDecode.discriminatedUnion "tag"
        [ ( "authenticatedUser"
          , TsDecode.map AuthenticatedUser
                (TsDecode.map User
                    (TsDecode.field "username" TsDecode.string)
                )
          )
        ]


flags : Decoder Flags
flags =
    TsDecode.map Flags
        (TsDecode.field "theme" TsDecode.string)
