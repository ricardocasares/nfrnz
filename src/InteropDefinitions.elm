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
    | StorageGet String


type ToElm
    = StorageValue String (Maybe String)


type alias Flags =
    { theme : String }


fromElm : Encoder FromElm
fromElm =
    TsEncode.union
        (\vStorageSet vStorageGet value ->
            case value of
                StorageSet key storageValue ->
                    vStorageSet ( key, storageValue )

                StorageGet key ->
                    vStorageGet key
        )
        |> TsEncode.variantTagged "storageSet"
            (TsEncode.object
                [ required "key" Tuple.first TsEncode.string
                , required "value" Tuple.second TsEncode.string
                ]
            )
        |> TsEncode.variantTagged "storageGet"
            (TsEncode.object [ required "key" identity TsEncode.string ])
        |> TsEncode.buildUnion


toElm : Decoder ToElm
toElm =
    TsDecode.discriminatedUnion "tag"
        [ ( "storageValue"
          , TsDecode.map2 StorageValue
                (TsDecode.field "key" TsDecode.string)
                (TsDecode.field "value" (TsDecode.nullable TsDecode.string))
          )
        ]


flags : Decoder Flags
flags =
    TsDecode.map Flags
        (TsDecode.field "theme" TsDecode.string)
