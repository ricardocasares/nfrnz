module Chat exposing
    ( Conversation, Message(..), ReplyState(..), ActiveRequest
    , empty, reset, isEmpty, title, searchText, selectedModel
    , ask, appendChunk, finishReply, failReply
    , turns, awaiting
    )

{-| Holds one conversation and the reply currently streaming into it.

@docs Conversation, Message, ReplyState, ActiveRequest
@docs empty, reset, isEmpty, title, searchText, selectedModel
@docs ask, appendChunk, finishReply, failReply
@docs turns, awaiting

-}

import ChadCn.Select as Select
import InteropDefinitions exposing (ChatTurn)


{-| Holds the messages, the composer draft and the in-flight request.
-}
type alias Conversation =
    { id : Int
    , messages : List Message
    , draft : String
    , modelSelect : Select.Model
    , request : Maybe ActiveRequest
    }


{-| Describes one message in a conversation.
-}
type Message
    = Asked String
    | Replied String ReplyState


{-| Describes how far a reply got.
-}
type ReplyState
    = Streaming
    | Complete
    | Failed String


{-| Identifies the reply currently streaming in.
-}
type alias ActiveRequest =
    { id : String
    , model : String
    }


{-| Creates an empty conversation.
-}
empty : Int -> Conversation
empty conversationId =
    { id = conversationId
    , messages = []
    , draft = ""
    , modelSelect = Select.init (selectId conversationId) Nothing
    , request = Nothing
    }


{-| Rebuilds a conversation's select state while keeping its selection.
-}
reset : Conversation -> Conversation
reset conversation =
    { conversation
        | modelSelect =
            Select.init (selectId conversation.id) (Select.selected conversation.modelSelect)
    }


{-| Returns the DOM identifier of a conversation's model select.
-}
selectId : Int -> String
selectId conversationId =
    "model-select-" ++ String.fromInt conversationId


{-| Returns whether nothing has been said yet.
-}
isEmpty : Conversation -> Bool
isEmpty conversation =
    List.isEmpty conversation.messages


{-| Returns a conversation's title, taken from its first question.
-}
title : Conversation -> String
title conversation =
    conversation.messages
        |> List.filterMap
            (\message ->
                case message of
                    Asked question ->
                        Just question

                    Replied _ _ ->
                        Nothing
            )
        |> List.head
        |> Maybe.withDefault "New chat"


{-| Returns a conversation's searchable text.
-}
searchText : Conversation -> String
searchText conversation =
    conversation.messages
        |> List.map content
        |> String.join " "


{-| Returns the model a conversation is set to talk to.
-}
selectedModel : Conversation -> Maybe String
selectedModel conversation =
    Select.selected conversation.modelSelect


{-| Returns the message text of any message.
-}
content : Message -> String
content message =
    case message of
        Asked asked ->
            asked

        Replied replied _ ->
            replied


{-| Records a question and the reply streaming in behind it.
-}
ask : ActiveRequest -> String -> Conversation -> Conversation
ask request question conversation =
    { conversation
        | messages = conversation.messages ++ [ Asked question, Replied "" Streaming ]
        , draft = ""
        , request = Just request
    }


{-| Grows the streaming reply of a matching request.
-}
appendChunk : String -> String -> Conversation -> Conversation
appendChunk requestId chunk conversation =
    mapStreaming requestId
        (\replied _ -> Replied (replied ++ chunk) Streaming)
        conversation


{-| Completes the streaming reply of a matching request.
-}
finishReply : String -> Conversation -> Conversation
finishReply requestId conversation =
    mapStreaming requestId (\replied _ -> Replied replied Complete) conversation
        |> clearRequest requestId


{-| Fails the streaming reply of a matching request.
-}
failReply : String -> String -> Conversation -> Conversation
failReply requestId reason conversation =
    mapStreaming requestId (\replied _ -> Replied replied (Failed reason)) conversation
        |> clearRequest requestId


{-| Returns the request a conversation is waiting on, if it is the given one.
-}
awaiting : String -> Conversation -> Maybe ActiveRequest
awaiting requestId conversation =
    conversation.request
        |> Maybe.andThen
            (\request ->
                if request.id == requestId then
                    Just request

                else
                    Nothing
            )


{-| Rewrites the trailing streaming reply when the request matches.
-}
mapStreaming : String -> (String -> ReplyState -> Message) -> Conversation -> Conversation
mapStreaming requestId rewrite conversation =
    case awaiting requestId conversation of
        Nothing ->
            conversation

        Just _ ->
            { conversation | messages = mapLastStreaming rewrite conversation.messages }


{-| Rewrites the last message when it is still streaming.
-}
mapLastStreaming : (String -> ReplyState -> Message) -> List Message -> List Message
mapLastStreaming rewrite messages =
    case List.reverse messages of
        (Replied replied Streaming) :: earlier ->
            List.reverse (rewrite replied Streaming :: earlier)

        _ ->
            messages


{-| Forgets the in-flight request when it is the given one.
-}
clearRequest : String -> Conversation -> Conversation
clearRequest requestId conversation =
    case awaiting requestId conversation of
        Nothing ->
            conversation

        Just _ ->
            { conversation | request = Nothing }


{-| Returns the turns to send to an inference server, skipping unfinished replies.
-}
turns : Conversation -> List ChatTurn
turns conversation =
    List.filterMap
        (\message ->
            case message of
                Asked asked ->
                    Just { role = "user", content = asked }

                Replied replied Complete ->
                    Just { role = "assistant", content = replied }

                Replied _ Streaming ->
                    Nothing

                Replied _ (Failed _) ->
                    Nothing
        )
        conversation.messages
