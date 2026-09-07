module Lobby exposing
    ( Lobby, Offer
    , empty, joined, left, offers
    , peerCount, offering, models
    )

{-| Tracks which models the peers in the lobby are currently sharing.

@docs Lobby, Offer
@docs empty, joined, left, offers
@docs peerCount, offering, models

-}

import Dict exposing (Dict)


{-| Holds the models each connected peer shares.
-}
type Lobby
    = Lobby (Dict String (List String))


{-| Describes one model name and the peers serving it.
-}
type alias Offer =
    { model : String
    , peers : List String
    }


{-| Creates a lobby with nobody in it.
-}
empty : Lobby
empty =
    Lobby Dict.empty


{-| Records a peer that arrived, before it has shared anything.
-}
joined : String -> Lobby -> Lobby
joined peerId (Lobby peers) =
    Lobby (Dict.update peerId (Maybe.withDefault [] >> Just) peers)


{-| Forgets a peer that left.
-}
left : String -> Lobby -> Lobby
left peerId (Lobby peers) =
    Lobby (Dict.remove peerId peers)


{-| Replaces what one peer shares.
-}
offers : String -> List String -> Lobby -> Lobby
offers peerId shared (Lobby peers) =
    Lobby (Dict.insert peerId shared peers)


{-| Returns how many peers are connected.
-}
peerCount : Lobby -> Int
peerCount (Lobby peers) =
    Dict.size peers


{-| Returns every shared model with the peers serving it, by model name.
-}
models : Lobby -> List Offer
models (Lobby peers) =
    Dict.foldl
        (\peerId shared byModel ->
            List.foldl
                (\model -> Dict.update model (Maybe.withDefault [] >> (::) peerId >> Just))
                byModel
                shared
        )
        Dict.empty
        peers
        |> Dict.toList
        |> List.map (\( model, servers ) -> { model = model, peers = servers })


{-| Returns the peers serving one model.
-}
offering : String -> Lobby -> List String
offering model lobby =
    models lobby
        |> List.filter (\offer -> offer.model == model)
        |> List.concatMap .peers
