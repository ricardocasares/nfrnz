module InteropDefinitions exposing (ChatTurn, Flags, FromElm(..), StreamRequest, ToElm(..), interop)

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
    | StreamStart StreamRequest
    | StreamCancel String


type ToElm
    = StorageValue String (Maybe String)
    | StreamChunk String String
    | StreamDone String
    | StreamError String String


{-| Describes one streamed completion, either for us or relayed to a peer.
-}
type alias StreamRequest =
    { requestId : String
    , replyTo : Maybe String
    , baseUrl : String
    , apiKey : Maybe String
    , model : String
    , messages : List ChatTurn
    }


{-| Describes one turn sent to an inference server.
-}
type alias ChatTurn =
    { role : String
    , content : String
    }


type alias Flags =
    { theme : String
    , providers : Maybe String
    , sharedModels : Maybe String
    }


fromElm : Encoder FromElm
fromElm =
    TsEncode.union
        (\vStorageSet vStorageGet vStreamStart vStreamCancel value ->
            case value of
                StorageSet key storageValue ->
                    vStorageSet ( key, storageValue )

                StorageGet key ->
                    vStorageGet key

                StreamStart request ->
                    vStreamStart request

                StreamCancel requestId ->
                    vStreamCancel requestId
        )
        |> TsEncode.variantTagged "storageSet"
            (TsEncode.object
                [ required "key" Tuple.first TsEncode.string
                , required "value" Tuple.second TsEncode.string
                ]
            )
        |> TsEncode.variantTagged "storageGet"
            (TsEncode.object [ required "key" identity TsEncode.string ])
        |> TsEncode.variantTagged "streamStart"
            (TsEncode.object
                [ required "requestId" .requestId TsEncode.string
                , optional "replyTo" .replyTo TsEncode.string
                , required "baseUrl" .baseUrl TsEncode.string
                , optional "apiKey" .apiKey TsEncode.string
                , required "model" .model TsEncode.string
                , required "messages" .messages (TsEncode.list chatTurn)
                ]
            )
        |> TsEncode.variantTagged "streamCancel"
            (TsEncode.object [ required "requestId" identity TsEncode.string ])
        |> TsEncode.buildUnion


chatTurn : Encoder ChatTurn
chatTurn =
    TsEncode.object
        [ required "role" .role TsEncode.string
        , required "content" .content TsEncode.string
        ]


toElm : Decoder ToElm
toElm =
    TsDecode.discriminatedUnion "tag"
        [ ( "storageValue"
          , TsDecode.map2 StorageValue
                (TsDecode.field "key" TsDecode.string)
                (TsDecode.field "value" (TsDecode.nullable TsDecode.string))
          )
        , ( "streamChunk"
          , TsDecode.map2 StreamChunk
                (TsDecode.field "requestId" TsDecode.string)
                (TsDecode.field "content" TsDecode.string)
          )
        , ( "streamDone"
          , TsDecode.map StreamDone (TsDecode.field "requestId" TsDecode.string)
          )
        , ( "streamError"
          , TsDecode.map2 StreamError
                (TsDecode.field "requestId" TsDecode.string)
                (TsDecode.field "reason" TsDecode.string)
          )
        ]


flags : Decoder Flags
flags =
    TsDecode.map3 Flags
        (TsDecode.field "theme" TsDecode.string)
        (TsDecode.field "providers" (TsDecode.nullable TsDecode.string))
        (TsDecode.field "sharedModels" (TsDecode.nullable TsDecode.string))
