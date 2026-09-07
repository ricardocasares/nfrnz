module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (a, div, h1, header, main_, p, span, text)
import Html.Attributes exposing (class, href)
import Url exposing (Url)


type alias Model =
    { key : Nav.Key
    , url : Url
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key, url = url }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "NFRNZ — Elm on the web"
    , body =
        [ main_
            [ class "min-h-screen bg-slate-950 text-white" ]
            [ header
                [ class "mx-auto flex max-w-6xl items-center justify-between px-6 py-8" ]
                [ a [ class "text-lg font-bold tracking-[0.25em]", href "/" ] [ text "NFRNZ" ]
                , span [ class "rounded-full border border-emerald-400/30 bg-emerald-400/10 px-3 py-1 text-xs font-medium text-emerald-300" ]
                    [ text "Browser.application" ]
                ]
            , div
                [ class "mx-auto grid max-w-6xl gap-12 px-6 py-20 lg:grid-cols-[1.4fr_1fr] lg:items-center lg:py-32" ]
                [ div []
                    [ p [ class "mb-5 font-mono text-sm uppercase tracking-[0.3em] text-emerald-400" ]
                        [ text "Compiled with confidence" ]
                    , h1 [ class "max-w-3xl text-5xl font-black leading-tight tracking-tight sm:text-7xl" ]
                        [ text "Your Elm app is "
                        , span [ class "text-emerald-400" ] [ text "alive." ]
                        ]
                    , p [ class "mt-7 max-w-2xl text-lg leading-8 text-slate-300" ]
                        [ text "It now owns the page, responds to URL changes, and is ready to grow into a fully routed application." ]
                    ]
                , div
                    [ class "rounded-3xl border border-white/10 bg-white/5 p-8 shadow-2xl shadow-emerald-950/40 backdrop-blur" ]
                    [ p [ class "text-sm font-semibold uppercase tracking-widest text-slate-400" ] [ text "Current path" ]
                    , p [ class "mt-3 break-all font-mono text-xl text-emerald-300" ] [ text model.url.path ]
                    , div [ class "mt-8 h-1.5 overflow-hidden rounded-full bg-slate-800" ]
                        [ div [ class "h-full w-2/3 rounded-full bg-emerald-400" ] [] ]
                    , p [ class "mt-4 text-sm text-slate-400" ] [ text "Navigation events are wired and ready." ]
                    ]
                ]
            ]
        ]
    }
