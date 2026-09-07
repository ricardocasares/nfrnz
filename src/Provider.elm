module Provider exposing
    ( Provider, Status(..)
    , defaults, restore, encodeAll
    , listModels, setStatus, errorToString
    , models, localModels, serving
    )

{-| Describes an OpenAI-compatible inference server the user can reach.

Ollama, LM Studio and llama.cpp all expose `GET /v1/models` and
`POST /v1/chat/completions`, so one shape covers the detected defaults and any
endpoint the user configures by hand.

@docs Provider, Status
@docs defaults, restore, encodeAll
@docs listModels, setStatus, errorToString
@docs models, localModels, serving

-}

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| One configured inference server.
-}
type alias Provider =
    { id : String
    , label : String
    , baseUrl : String
    , apiKey : Maybe String
    , status : Status
    }


{-| Describes what the last model listing told us about a provider.
-}
type Status
    = Unchecked
    | Checking
    | Unavailable String
    | Available (List String)


{-| Returns the providers detected out of the box.
-}
defaults : List Provider
defaults =
    [ provider "ollama" "Ollama" "http://localhost:11434"
    , provider "lmstudio" "LM Studio" "http://localhost:1234"
    , provider "llamacpp" "llama.cpp" "http://localhost:8080"
    ]


{-| Creates an unchecked provider without credentials.
-}
provider : String -> String -> String -> Provider
provider providerId label baseUrl =
    { id = providerId
    , label = label
    , baseUrl = baseUrl
    , apiKey = Nothing
    , status = Unchecked
    }


{-| Restores stored providers, falling back to the detected defaults.
-}
restore : Maybe String -> List Provider
restore stored =
    stored
        |> Maybe.andThen (Decode.decodeString (Decode.list storedDecoder) >> Result.toMaybe)
        |> Maybe.andThen
            (\restored ->
                if List.isEmpty restored then
                    Nothing

                else
                    Just restored
            )
        |> Maybe.withDefault defaults


{-| Encodes providers for storage, dropping transient status.
-}
encodeAll : List Provider -> String
encodeAll providers =
    Encode.encode 0 (Encode.list encodeOne providers)


{-| Encodes one provider for storage.
-}
encodeOne : Provider -> Encode.Value
encodeOne configured =
    Encode.object
        ([ ( "id", Encode.string configured.id )
         , ( "label", Encode.string configured.label )
         , ( "baseUrl", Encode.string configured.baseUrl )
         ]
            ++ (configured.apiKey
                    |> Maybe.map (\key -> [ ( "apiKey", Encode.string key ) ])
                    |> Maybe.withDefault []
               )
        )


{-| Decodes one stored provider back into an unchecked provider.
-}
storedDecoder : Decoder Provider
storedDecoder =
    Decode.map4
        (\providerId label baseUrl apiKey ->
            { id = providerId
            , label = label
            , baseUrl = baseUrl
            , apiKey = apiKey
            , status = Unchecked
            }
        )
        (Decode.field "id" Decode.string)
        (Decode.field "label" Decode.string)
        (Decode.field "baseUrl" Decode.string)
        (Decode.maybe (Decode.field "apiKey" Decode.string))


{-| Asks a provider which models it serves.
-}
listModels : (String -> Result Http.Error (List String) -> msg) -> Provider -> Cmd msg
listModels toMsg configured =
    Http.request
        { method = "GET"
        , headers = authHeaders configured
        , url = configured.baseUrl ++ "/v1/models"
        , body = Http.emptyBody
        , expect = Http.expectJson (toMsg configured.id) modelsDecoder
        , timeout = Just 5000
        , tracker = Nothing
        }


{-| Returns the authorization headers a provider needs, if any.
-}
authHeaders : Provider -> List Http.Header
authHeaders configured =
    configured.apiKey
        |> Maybe.map (\key -> [ Http.header "Authorization" ("Bearer " ++ key) ])
        |> Maybe.withDefault []


{-| Decodes the OpenAI model listing shape.
-}
modelsDecoder : Decoder (List String)
modelsDecoder =
    Decode.field "data" (Decode.list (Decode.field "id" Decode.string))


{-| Replaces the status of one provider.
-}
setStatus : String -> Status -> List Provider -> List Provider
setStatus providerId status providers =
    List.map
        (\configured ->
            if configured.id == providerId then
                { configured | status = status }

            else
                configured
        )
        providers


{-| Describes a failed model listing in words a user can act on.
-}
errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "Invalid address: " ++ url

        Http.Timeout ->
            "Timed out"

        Http.NetworkError ->
            "Not reachable"

        Http.BadStatus status ->
            "Responded with " ++ String.fromInt status

        Http.BadBody _ ->
            "Unexpected response"


{-| Returns the models one provider serves.
-}
models : Provider -> List String
models configured =
    case configured.status of
        Available available ->
            available

        Unchecked ->
            []

        Checking ->
            []

        Unavailable _ ->
            []


{-| Returns every model reachable locally, without duplicates.
-}
localModels : List Provider -> List String
localModels providers =
    providers
        |> List.concatMap models
        |> List.foldr
            (\name unique ->
                if List.member name unique then
                    unique

                else
                    name :: unique
            )
            []


{-| Returns the first provider serving a model.
-}
serving : String -> List Provider -> Maybe Provider
serving modelName providers =
    providers
        |> List.filter (\configured -> List.member modelName (models configured))
        |> List.head
