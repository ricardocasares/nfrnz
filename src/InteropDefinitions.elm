module InteropDefinitions exposing (ChatTurn, Flags, FromElm(..), PeerRequest, StreamRequest, ToElm(..), interop)

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
    | ShareModels (List String)
    | AskPeer PeerRequest
    | RejectPeer String String String


type ToElm
    = StorageValue String (Maybe String)
    | StreamChunk String String
    | StreamDone String
    | StreamError String String
    | PeerJoined String
    | PeerLeft String
    | PeerModelsUpdated String (List String)
    | PeerAsked PeerRequest


{-| Describes one completion asked of, or by, a peer.
-}
type alias PeerRequest =
    { requestId : String
    , peerId : String
    , model : String
    , messages : List ChatTurn
    }


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
        (\vStorageSet vStorageGet vStreamStart vStreamCancel vShareModels vAskPeer vRejectPeer value ->
            case value of
                StorageSet key storageValue ->
                    vStorageSet ( key, storageValue )

                StorageGet key ->
                    vStorageGet key

                StreamStart request ->
                    vStreamStart request

                StreamCancel requestId ->
                    vStreamCancel requestId

                ShareModels shared ->
                    vShareModels shared

                AskPeer request ->
                    vAskPeer request

                RejectPeer requestId peerId reason ->
                    vRejectPeer { requestId = requestId, peerId = peerId, reason = reason }
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
        |> TsEncode.variantTagged "shareModels"
            (TsEncode.object [ required "models" identity (TsEncode.list TsEncode.string) ])
        |> TsEncode.variantTagged "askPeer" peerRequest
        |> TsEncode.variantTagged "rejectPeer"
            (TsEncode.object
                [ required "requestId" .requestId TsEncode.string
                , required "peerId" .peerId TsEncode.string
                , required "reason" .reason TsEncode.string
                ]
            )
        |> TsEncode.buildUnion


chatTurn : Encoder ChatTurn
chatTurn =
    TsEncode.object
        [ required "role" .role TsEncode.string
        , required "content" .content TsEncode.string
        ]


peerRequest : Encoder PeerRequest
peerRequest =
    TsEncode.object
        [ required "requestId" .requestId TsEncode.string
        , required "peerId" .peerId TsEncode.string
        , required "model" .model TsEncode.string
        , required "messages" .messages (TsEncode.list chatTurn)
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
        , ( "peerJoined"
          , TsDecode.map PeerJoined (TsDecode.field "peerId" TsDecode.string)
          )
        , ( "peerLeft"
          , TsDecode.map PeerLeft (TsDecode.field "peerId" TsDecode.string)
          )
        , ( "peerModelsUpdated"
          , TsDecode.map2 PeerModelsUpdated
                (TsDecode.field "peerId" TsDecode.string)
                (TsDecode.field "models" (TsDecode.list TsDecode.string))
          )
        , ( "peerAsked"
          , TsDecode.map PeerAsked
                (TsDecode.map4 PeerRequest
                    (TsDecode.field "requestId" TsDecode.string)
                    (TsDecode.field "peerId" TsDecode.string)
                    (TsDecode.field "model" TsDecode.string)
                    (TsDecode.field "messages" (TsDecode.list chatTurnDecoder))
                )
          )
        ]


chatTurnDecoder : Decoder ChatTurn
chatTurnDecoder =
    TsDecode.map2 ChatTurn
        (TsDecode.field "role" TsDecode.string)
        (TsDecode.field "content" TsDecode.string)


flags : Decoder Flags
flags =
    TsDecode.map3 Flags
        (TsDecode.field "theme" TsDecode.string)
        (TsDecode.field "providers" (TsDecode.nullable TsDecode.string))
        (TsDecode.field "sharedModels" (TsDecode.nullable TsDecode.string))
