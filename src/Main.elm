module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Nav
import ChadCn.Badge as Badge
import ChadCn.Button as Button
import ChadCn.Command as Command
import ChadCn.Dialog as Dialog
import ChadCn.Icon as Icon
import ChadCn.Input as Input
import ChadCn.Message as Message
import ChadCn.Popup.Types as Popup
import ChadCn.Select as Select
import ChadCn.Sidebar as Sidebar
import ChadCn.Textarea as Textarea
import Chat exposing (Conversation)
import Html exposing (Html, div, form, h1, header, main_, p, section, span, text)
import Html.Attributes exposing (attribute, class, classList, id, rows, title)
import Html.Events exposing (onClick, onSubmit, preventDefaultOn)
import Http
import InteropDefinitions exposing (FromElm(..), ToElm(..))
import InteropPorts
import Json.Decode as Decode
import Json.Encode as Encode
import Lobby exposing (Lobby)
import Provider exposing (Provider)
import Task
import Url exposing (Url)


type alias Model =
    { key : Nav.Key
    , url : Url
    , current : Conversation
    , history : List Conversation
    , nextId : Int
    , sidebar : Sidebar.State
    , mobileSidebar : Bool
    , search : String
    , theme : Theme
    , providers : List Provider
    , sharedModels : List String
    , nextRequestId : Int
    , lobby : Lobby
    , sharing : Dialog.Model
    , sharingSearch : String
    , sharingCommand : Command.Model
    , config : Dialog.Model
    , nextProviderId : Int
    }


type Theme
    = Light
    | Dark


providersStorageKey : String
providersStorageKey =
    "providers"


sharedModelsStorageKey : String
sharedModelsStorageKey =
    "sharedModels"


themeStorageKey : String
themeStorageKey =
    "theme"


themeToString : Theme -> String
themeToString theme =
    case theme of
        Light ->
            "light"

        Dark ->
            "dark"


themeFromString : String -> Maybe Theme
themeFromString string =
    case string of
        "light" ->
            Just Light

        "dark" ->
            Just Dark

        _ ->
            Nothing


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | DraftChanged String
    | ModelSelectMsg Select.Msg
    | SendMessage
    | NewChat
    | SelectChat Int
    | DeleteCurrentChat
    | SearchChanged String
    | UseSuggestion String
    | ToggleSidebar
    | ToggleMobileSidebar
    | CloseMobileSidebar
    | ToggleTheme
    | DomFinished
    | InteropMsgReceived (Result Decode.Error ToElm)
    | ProviderModelsReceived String (Result Http.Error (List String))
    | OpenSharing
    | SharingMsg Dialog.Msg
    | SharingCommandMsg Command.Msg
    | SharingSearchChanged String
    | ToggleShared String
    | OpenConfig
    | ConfigMsg Dialog.Msg
    | ProviderLabelChanged String String
    | ProviderUrlChanged String String
    | ProviderKeyChanged String String
    | AddProvider
    | RemoveProvider String
    | ScanProviders


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ Select.subscriptions (modelSelectConfig model) model.current.modelSelect
                    , Dialog.subscriptions (sharingDialogConfig model) model.sharing
                    , Dialog.subscriptions (configDialogConfig model) model.config
                    , InteropPorts.toElm |> Sub.map InteropMsgReceived
                    , Browser.Events.onKeyDown
                        (Decode.field "key" Decode.string
                            |> Decode.andThen
                                (\key ->
                                    if key == "Escape" then
                                        Decode.succeed CloseMobileSidebar

                                    else
                                        Decode.fail "Not a sidebar shortcut"
                                )
                        )
                    ]
        , view = view
        }


init : Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flagsValue url key =
    let
        flags =
            InteropPorts.decodeFlags flagsValue |> Result.toMaybe

        providers =
            Provider.restore (Maybe.andThen .providers flags)
    in
    ( { key = key
      , url = url
      , current = Chat.empty 0
      , history = []
      , nextId = 1
      , nextRequestId = 0
      , sidebar = Sidebar.Expanded
      , mobileSidebar = False
      , search = ""
      , theme =
            flags
                |> Maybe.andThen (.theme >> themeFromString)
                |> Maybe.withDefault Light
      , providers = List.map (\configured -> { configured | status = Provider.Checking }) providers
      , sharedModels = decodeSharedModels (Maybe.andThen .sharedModels flags)
      , lobby = Lobby.empty
      , sharing = Dialog.init "sharing-dialog"
      , sharingSearch = ""
      , sharingCommand = Command.init
      , config = Dialog.init "config-dialog"
      , nextProviderId = 0
      }
    , detectProviders providers
    )


detectProviders : List Provider -> Cmd Msg
detectProviders providers =
    providers
        |> List.map (Provider.listModels ProviderModelsReceived)
        |> Cmd.batch


decodeSharedModels : Maybe String -> List String
decodeSharedModels stored =
    stored
        |> Maybe.andThen (Decode.decodeString (Decode.list Decode.string) >> Result.toMaybe)
        |> Maybe.withDefault []


focusComposer : Cmd Msg
focusComposer =
    Dom.focus "composer" |> Task.attempt (\_ -> DomFinished)


scrollToLatest : Cmd Msg
scrollToLatest =
    Dom.getViewportOf "chat-scroll"
        |> Task.andThen (\viewport -> Dom.setViewportOf "chat-scroll" 0 viewport.scene.height)
        |> Task.attempt (\_ -> DomFinished)


rememberCurrent : Model -> List Conversation
rememberCurrent model =
    if Chat.isEmpty model.current then
        model.history

    else
        Chat.reset model.current :: model.history


sendQuestion : String -> String -> Model -> ( Model, Cmd Msg )
sendQuestion question modelName model =
    let
        request =
            { id = "request-" ++ String.fromInt model.nextRequestId
            , model = modelName
            }

        asked =
            Chat.ask request question model.current

        sent =
            case Provider.serving modelName model.providers of
                Just provider ->
                    Just
                        (StreamStart
                            { requestId = request.id
                            , replyTo = Nothing
                            , baseUrl = provider.baseUrl
                            , apiKey = provider.apiKey
                            , model = modelName
                            , messages = Chat.turns asked
                            }
                        )

                Nothing ->
                    Lobby.offering modelName model.lobby
                        |> List.head
                        |> Maybe.map
                            (\peerId ->
                                AskPeer
                                    { requestId = request.id
                                    , peerId = peerId
                                    , model = modelName
                                    , messages = Chat.turns asked
                                    }
                            )
    in
    case sent of
        Nothing ->
            ( model, Cmd.none )

        Just fromElm ->
            ( { model
                | current = asked
                , nextRequestId = model.nextRequestId + 1
                , search = ""
              }
            , Cmd.batch [ InteropPorts.fromElm fromElm, focusComposer, scrollToLatest ]
            )


{-| Answers a peer asking for one of the models we share.
-}
servePeer : InteropDefinitions.PeerRequest -> Model -> Cmd Msg
servePeer request model =
    case
        ( List.member request.model (servableModels model)
        , Provider.serving request.model model.providers
        )
    of
        ( True, Just provider ) ->
            InteropPorts.fromElm
                (StreamStart
                    { requestId = request.requestId
                    , replyTo = Just request.peerId
                    , baseUrl = provider.baseUrl
                    , apiKey = provider.apiKey
                    , model = request.model
                    , messages = List.take inboundTurnLimit request.messages
                    }
                )

        _ ->
            InteropPorts.fromElm
                (RejectPeer request.requestId request.peerId "That model is not shared")


{-| Caps how much conversation a peer can push through our provider at once.
-}
inboundTurnLimit : Int
inboundTurnLimit =
    50


{-| Returns the shared models we can actually serve right now.
-}
servableModels : Model -> List String
servableModels model =
    List.filter
        (\name -> Provider.serving name model.providers /= Nothing)
        model.sharedModels


{-| Tells the lobby what we serve, and remembers what the user chose.
-}
shareModels : Model -> Cmd Msg
shareModels model =
    Cmd.batch
        [ InteropPorts.fromElm (ShareModels (servableModels model))
        , InteropPorts.fromElm
            (StorageSet sharedModelsStorageKey (encodeStrings model.sharedModels))
        ]


{-| Applies one change to a single configured provider.
-}
editProvider : String -> (Provider -> Provider) -> Model -> ( Model, Cmd Msg )
editProvider providerId change model =
    let
        edited =
            { model
                | providers =
                    List.map
                        (\configured ->
                            if configured.id == providerId then
                                change configured

                            else
                                configured
                        )
                        model.providers
            }
    in
    ( edited, persistProviders edited )


persistProviders : Model -> Cmd Msg
persistProviders model =
    InteropPorts.fromElm
        (StorageSet providersStorageKey (Provider.encodeAll model.providers))


encodeStrings : List String -> String
encodeStrings values =
    Encode.encode 0 (Encode.list Encode.string values)


cancelRequest : Conversation -> Cmd Msg
cancelRequest conversation =
    conversation.request
        |> Maybe.map (\request -> InteropPorts.fromElm (StreamCancel request.id))
        |> Maybe.withDefault Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        current =
            model.current
    in
    case msg of
        UrlRequested request ->
            case request of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        DraftChanged draft ->
            ( { model | current = { current | draft = draft } }, Cmd.none )

        ModelSelectMsg selectMsg ->
            let
                ( modelSelect, cmd ) =
                    Select.update (modelSelectConfig model) selectMsg current.modelSelect
            in
            ( { model | current = { current | modelSelect = modelSelect } }, cmd )

        SendMessage ->
            case ( String.trim current.draft, Chat.selectedModel current ) of
                ( "", _ ) ->
                    ( model, Cmd.none )

                ( _, Nothing ) ->
                    ( model, Cmd.none )

                ( question, Just modelName ) ->
                    sendQuestion question modelName model

        NewChat ->
            if Chat.isEmpty current then
                ( { model | mobileSidebar = False, search = "" }, focusComposer )

            else
                ( { model
                    | current = Chat.empty model.nextId
                    , history = rememberCurrent model
                    , nextId = model.nextId + 1
                    , mobileSidebar = False
                    , search = ""
                  }
                , focusComposer
                )

        SelectChat conversationId ->
            case List.filter (\conversation -> conversation.id == conversationId) model.history |> List.head of
                Just conversation ->
                    ( { model
                        | current = conversation
                        , history = List.filter (\chat -> chat.id /= conversationId) (rememberCurrent model)
                        , mobileSidebar = False
                      }
                    , Cmd.batch [ focusComposer, scrollToLatest ]
                    )

                Nothing ->
                    ( { model | mobileSidebar = False }, focusComposer )

        DeleteCurrentChat ->
            ( { model | current = Chat.empty model.nextId, nextId = model.nextId + 1 }
            , Cmd.batch [ cancelRequest current, focusComposer ]
            )

        SearchChanged query ->
            ( { model | search = query }, Cmd.none )

        UseSuggestion prompt ->
            ( { model | current = { current | draft = prompt } }, focusComposer )

        ToggleSidebar ->
            ( { model | sidebar = Sidebar.toggle model.sidebar }, Cmd.none )

        ToggleMobileSidebar ->
            ( { model | mobileSidebar = True }
            , Dom.focus "chat-search" |> Task.attempt (\_ -> DomFinished)
            )

        CloseMobileSidebar ->
            ( { model | mobileSidebar = False }
            , if model.mobileSidebar then
                Dom.focus "mobile-sidebar-trigger" |> Task.attempt (\_ -> DomFinished)

              else
                Cmd.none
            )

        ToggleTheme ->
            let
                theme =
                    case model.theme of
                        Light ->
                            Dark

                        Dark ->
                            Light
            in
            ( { model | theme = theme }
            , InteropPorts.fromElm (StorageSet themeStorageKey (themeToString theme))
            )

        DomFinished ->
            ( model, Cmd.none )

        InteropMsgReceived (Ok (StorageValue _ _)) ->
            ( model, Cmd.none )

        InteropMsgReceived (Ok (StreamChunk requestId chunk)) ->
            ( { model | current = Chat.appendChunk requestId chunk current }, scrollToLatest )

        InteropMsgReceived (Ok (StreamDone requestId)) ->
            ( { model | current = Chat.finishReply requestId current }, scrollToLatest )

        InteropMsgReceived (Ok (StreamError requestId reason)) ->
            ( { model | current = Chat.failReply requestId reason current }, scrollToLatest )

        InteropMsgReceived (Ok (PeerJoined peerId)) ->
            ( { model | lobby = Lobby.joined peerId model.lobby }, Cmd.none )

        InteropMsgReceived (Ok (PeerLeft peerId)) ->
            ( { model | lobby = Lobby.left peerId model.lobby }, Cmd.none )

        InteropMsgReceived (Ok (PeerModelsUpdated peerId shared)) ->
            ( { model | lobby = Lobby.offers peerId shared model.lobby }, Cmd.none )

        InteropMsgReceived (Ok (PeerAsked request)) ->
            ( model, servePeer request model )

        InteropMsgReceived (Err _) ->
            ( model, Cmd.none )

        OpenSharing ->
            let
                ( sharing, cmd ) =
                    Dialog.open (sharingDialogConfig model) model.sharing
            in
            ( { model | sharing = sharing, sharingSearch = "" }, cmd )

        SharingMsg dialogMsg ->
            let
                ( sharing, cmd ) =
                    Dialog.update (sharingDialogConfig model) dialogMsg model.sharing
            in
            ( { model | sharing = sharing }, cmd )

        SharingCommandMsg commandMsg ->
            let
                ( sharingCommand, cmd ) =
                    Command.update (sharingCommandConfig model) commandMsg model.sharingCommand
            in
            ( { model | sharingCommand = sharingCommand }, cmd )

        SharingSearchChanged query ->
            ( { model | sharingSearch = query }, Cmd.none )

        ToggleShared name ->
            let
                sharedModels =
                    if List.member name model.sharedModels then
                        List.filter ((/=) name) model.sharedModels

                    else
                        model.sharedModels ++ [ name ]

                shared =
                    { model | sharedModels = sharedModels }
            in
            ( shared, shareModels shared )

        OpenConfig ->
            let
                ( config, cmd ) =
                    Dialog.open (configDialogConfig model) model.config
            in
            ( { model | config = config }, cmd )

        ConfigMsg dialogMsg ->
            let
                ( config, cmd ) =
                    Dialog.update (configDialogConfig model) dialogMsg model.config
            in
            ( { model | config = config }, cmd )

        ProviderLabelChanged providerId label ->
            editProvider providerId (\configured -> { configured | label = label }) model

        ProviderUrlChanged providerId baseUrl ->
            editProvider providerId
                (\configured ->
                    { configured | baseUrl = baseUrl, status = Provider.Unchecked }
                )
                model

        ProviderKeyChanged providerId apiKey ->
            editProvider providerId
                (\configured ->
                    { configured
                        | apiKey =
                            if String.isEmpty apiKey then
                                Nothing

                            else
                                Just apiKey
                        , status = Provider.Unchecked
                    }
                )
                model

        AddProvider ->
            let
                added =
                    { model
                        | providers = model.providers ++ [ Provider.custom model.nextProviderId ]
                        , nextProviderId = model.nextProviderId + 1
                    }
            in
            ( added, persistProviders added )

        RemoveProvider providerId ->
            let
                removed =
                    { model
                        | providers =
                            List.filter (\configured -> configured.id /= providerId) model.providers
                    }
            in
            ( removed, Cmd.batch [ persistProviders removed, shareModels removed ] )

        ScanProviders ->
            ( { model
                | providers =
                    List.map
                        (\configured -> { configured | status = Provider.Checking })
                        model.providers
              }
            , Cmd.batch [ persistProviders model, detectProviders model.providers ]
            )

        ProviderModelsReceived providerId result ->
            let
                status =
                    case result of
                        Ok models ->
                            Provider.Available models

                        Err error ->
                            Provider.Unavailable (Provider.errorToString error)

                detected =
                    { model | providers = Provider.setStatus providerId status model.providers }
            in
            ( detected, InteropPorts.fromElm (ShareModels (servableModels detected)) )


view : Model -> Browser.Document Msg
view model =
    { title = Chat.title model.current ++ " · nfrnz"
    , body =
        [ div
            [ class """
                    flex h-dvh overflow-hidden bg-background text-foreground font-sans antialiased [--sidebar-width:17rem]
                    [--sidebar-width-icon:4.25rem] md:max-[1051px]:[--sidebar-width:15rem] [&_button]:cursor-pointer
                    [&_button:disabled]:cursor-default motion-reduce:[&_*]:transition-none! motion-reduce:[&_*]:animate-none!
                    motion-reduce:[&_*]:scroll-auto! motion-reduce:[&_*::before]:transition-none! motion-reduce:[&_*::before]:animate-none! motion-reduce:[&_*::before]:scroll-auto!
                    motion-reduce:[&_*::after]:transition-none! motion-reduce:[&_*::after]:animate-none! motion-reduce:[&_*::after]:scroll-auto!
                    """
            , classList [ ( "dark", model.theme == Dark ) ]
            ]
            [ viewSidebar model
            , viewSharingDialog model
            , viewConfigDialog model
            , Button.new []
                |> Button.withVariant Button.Ghost
                |> Button.withOnClick CloseMobileSidebar
                |> Button.withAttributes [ classList [ ( "hidden!", not model.mobileSidebar ), ( "fixed! inset-0! z-20! size-full! rounded-none! border-0! bg-black/40! md:hidden!", model.mobileSidebar ) ], attribute "aria-label" "Close sidebar" ]
                |> Button.view
            , main_ [ class "flex min-w-0 flex-1 flex-col" ]
                [ viewHeader model
                , div [ id "chat-scroll", class "min-h-0 flex-1 overflow-y-auto overscroll-contain [scrollbar-width:thin]", classList [ ( "flex", Chat.isEmpty model.current ) ] ]
                    [ if Chat.isEmpty model.current then
                        viewWelcome model

                      else
                        viewConversation model.current
                    ]
                , if Chat.isEmpty model.current then
                    text ""

                  else
                    div [ class "mx-auto w-full max-w-[800px] flex-none px-8 pt-3 md:max-[1051px]:px-[26px] max-md:px-3.5 max-md:pt-2.5" ] [ viewComposer model ]
                , p [ class "m-0 flex-none px-4 pt-3.5 pb-5 text-center text-[10px] tracking-[.15px] text-muted-foreground max-md:pb-[max(14px,env(safe-area-inset-bottom))]" ] [ text "A little curiosity goes a long way." ]
                ]
            ]
        ]
    }


viewSidebar : Model -> Html Msg
viewSidebar model =
    let
        conversations =
            rememberCurrent model
                |> List.sortBy (\chat -> -chat.id)
                |> List.filter
                    (\chat ->
                        String.contains (String.toLower model.search)
                            (String.toLower (Chat.searchText chat))
                    )

        historyGroup =
            List.foldl
                (\conversation group ->
                    Sidebar.newItem (Chat.title conversation) (SelectChat conversation.id)
                        |> Sidebar.withItemIcon (Icon.render Icon.MessageSquare)
                        |> Sidebar.withItemActive (conversation.id == model.current.id)
                        |> (\item -> Sidebar.addConfiguredItem item group)
                )
                (Sidebar.newGroup
                    (if List.isEmpty conversations then
                        if String.isEmpty model.search then
                            "Your chats appear here"

                        else
                            "No matching chats"

                     else
                        "Your chats"
                    )
                )
                conversations
    in
    div
        [ class """
                    flex-none max-md:fixed max-md:inset-y-0 max-md:left-0 max-md:z-30 max-md:w-[min(285px,85vw)] max-md:transition-transform
                    max-md:duration-200 max-md:ease-[ease] [&_[data-slot=sidebar]]:h-dvh! max-md:[&_[data-slot=sidebar]]:flex
                    max-md:[&_[data-slot=sidebar]]:w-full max-md:[&_[data-slot=sidebar]]:flex-col max-md:[&_[data-slot=sidebar]]:border-r
                    max-md:[&_[data-slot=sidebar]]:border-sidebar-border [&_[data-slot=sidebar-header]]:px-4
                    [&_[data-slot=sidebar-header]]:pt-[23px] [&_[data-slot=sidebar-header]]:pb-3 [&_[data-slot=sidebar-content]]:px-4
                    [&_[data-slot=sidebar-content]]:py-3.5 [&_[data-slot=sidebar-content]]:gap-0
                    [&_[data-slot=sidebar-group-label]]:text-[11px] [&_[data-slot=sidebar-group-label]]:normal-case
                    [&_[data-slot=sidebar-group-label]]:tracking-[.1px] [&_[data-slot=sidebar-group-label]]:font-medium
                    [&_[data-slot=sidebar-group-label]]:mb-[9px] [&_[data-slot=sidebar-menu]]:gap-1
                    [&_[data-slot=sidebar-menu-button]]:min-h-[37px] [&_[data-slot=sidebar-menu-button]>span]:truncate
                    [&_[data-slot=sidebar-menu-button]_svg]:text-muted-foreground [&_[data-slot=sidebar-footer]]:p-4
                    md:[&_[data-state=collapsed]_[data-slot=sidebar-content]]:px-[18px]
                    """
        , classList
            [ ( "max-md:-translate-x-full max-md:invisible", not model.mobileSidebar )
            , ( "max-md:translate-x-0 max-md:visible", model.mobileSidebar )
            ]
        ]
        [ Sidebar.new
            |> Sidebar.withNavigationLabel "Chat history"
            |> Sidebar.withHeader
                (div []
                    [ div [ class "mb-[30px] flex items-center justify-between px-1 md:group-data-[state=collapsed]/sidebar:justify-center md:group-data-[state=collapsed]/sidebar:p-0" ]
                        [ div [ class "flex items-center gap-[9px] md:group-data-[state=collapsed]/sidebar:hidden" ]
                            [ brandMark
                            , span [ class "text-[25px] leading-none font-[650] tracking-[-1.5px] md:group-data-[state=collapsed]/sidebar:hidden" ] [ text "nfrnz" ]
                            ]
                        , Sidebar.trigger ToggleSidebar [ class "text-muted-foreground max-md:hidden!", title "Toggle sidebar" ]
                        , Sidebar.trigger CloseMobileSidebar [ class "hidden! text-muted-foreground max-md:inline-flex!", title "Close sidebar" ]
                        ]
                    , Sidebar.newMenuButton [ Icon.render Icon.Plus, span [ class "md:group-data-[state=collapsed]/sidebar:hidden" ] [ text "New chat" ] ]
                        |> Sidebar.withMenuButtonAttributes [ onClick NewChat, class """
                    h-[41px]! gap-[11px]! rounded-[9px]! border border-border! bg-background! px-3! py-2.5! font-medium
                    shadow-[0_1px_2px_#00000003]! md:group-data-[state=collapsed]/sidebar:size-9!
                    md:group-data-[state=collapsed]/sidebar:p-2!
                    """, attribute "aria-label" "New chat" ]
                        |> Sidebar.withMenuButtonTooltip "new-chat-tooltip" "New chat"
                        |> Sidebar.viewMenuButton
                    , div [ class "relative mt-2 flex items-center text-muted-foreground md:group-data-[state=collapsed]/sidebar:hidden [&>svg]:pointer-events-none [&>svg]:absolute [&>svg]:left-[13px] [&>svg]:w-4" ]
                        [ Icon.render Icon.Search
                        , Input.new
                            |> Input.withType Input.Search
                            |> Input.withAriaLabel "Search chats"
                            |> Input.withPlaceholder "Search chats"
                            |> Input.withValue model.search
                            |> Input.withOnInput SearchChanged
                            |> Input.withAttributes [ id "chat-search", class "h-10! border-transparent! bg-transparent! py-2.5! pr-3! pl-[39px]! text-[13px]! shadow-none!" ]
                            |> Input.view
                        ]
                    ]
                )
            |> Sidebar.addGroup historyGroup
            |> Sidebar.withFooter
                (div []
                    [ Sidebar.newMenuButton
                        [ Icon.render Icon.Share
                        , span [ class "md:group-data-[state=collapsed]/sidebar:hidden" ] [ text "Share models" ]
                        ]
                        |> Sidebar.withMenuButtonAttributes
                            [ onClick OpenSharing
                            , id "share-models"
                            , class "mb-3 h-[37px]! gap-[11px]! rounded-[9px]! px-3! font-medium md:group-data-[state=collapsed]/sidebar:size-9! md:group-data-[state=collapsed]/sidebar:p-2!"
                            , attribute "aria-label" "Share models"
                            ]
                        |> Sidebar.withMenuButtonTooltip "share-models-tooltip" "Share models"
                        |> Sidebar.viewMenuButton
                    , div [ class """
                    flex items-start gap-2.5 px-[9px] pt-[6px] pb-[22px] text-xs leading-[1.6] text-muted-foreground
                    md:group-data-[state=collapsed]/sidebar:hidden [&_svg]:mt-0.5 [&_svg]:size-4 [&_svg]:flex-none [&_p]:m-0 [&_span]:block
                    [&_span]:text-[11px] [&_span]:opacity-80
                    """ ]
                        [ Icon.render Icon.MessageCircle
                        , p []
                            [ text (lobbySummary model)
                            , span [] [ text (sharingSummary model) ]
                            ]
                        ]
                    , div [ class "flex items-center gap-2.5 border-t border-sidebar-border px-1 pt-[19px] pb-px md:group-data-[state=collapsed]/sidebar:justify-center md:group-data-[state=collapsed]/sidebar:px-0" ]
                        [ div [ class "grid size-[33px] flex-none place-items-center rounded-full bg-[#e6e8df] text-xs leading-[1.5] font-semibold text-[#646957]" ] [ text "N" ]
                        , div [ class "flex flex-col gap-[3px] text-xs leading-[1.5] font-medium md:group-data-[state=collapsed]/sidebar:hidden [&>span:last-child]:text-[11px] [&>span:last-child]:font-normal [&>span:last-child]:text-muted-foreground" ]
                            [ span [] [ text "Personal workspace" ]
                            , span [] [ text "Local session" ]
                            ]
                        ]
                    ]
                )
            |> Sidebar.view model.sidebar
        ]


brandMark : Html msg
brandMark =
    span [ class "inline-flex h-8 w-[29px] flex-none items-center justify-center text-[#bf683e] [&_svg]:size-7 [&_svg]:stroke-[2.3]", attribute "aria-hidden" "true" ] [ Icon.render Icon.Zap ]


viewHeader : Model -> Html Msg
viewHeader model =
    header [ class "flex min-h-20 flex-none items-center justify-between gap-4 px-8 max-md:min-h-[68px] max-md:gap-2 max-md:px-4" ]
        [ div [ class "flex min-w-0 items-center gap-3.5 overflow-hidden max-md:gap-2.5" ]
            [ Sidebar.trigger ToggleMobileSidebar
                [ class "hidden! flex-none max-md:inline-flex!"
                , id "mobile-sidebar-trigger"
                , title "Open sidebar"
                , attribute "aria-expanded"
                    (if model.mobileSidebar then
                        "true"

                     else
                        "false"
                    )
                ]
            , span [ class "text-base font-semibold tracking-[-.5px]" ] [ text "nfrnz" ]
            , span [ class "text-xl leading-[1.5] font-light text-border max-md:hidden" ] [ text "/" ]
            , span [ class "max-w-60 truncate text-[13px] text-muted-foreground max-md:hidden" ] [ text (Chat.title model.current) ]
            ]
        , div [ class "flex min-w-0 flex-none items-center gap-2.5 text-muted-foreground max-md:gap-[3px]" ]
            [ viewProviderBadge model.providers
            , if Chat.isEmpty model.current then
                text ""

              else
                iconButton "Delete current chat" Icon.Trash2 DeleteCurrentChat
            , Button.new [ Icon.render Icon.Settings ]
                |> Button.withVariant Button.Ghost
                |> Button.withSize Button.Icon
                |> Button.withOnClick OpenConfig
                |> Button.withAttributes
                    [ attribute "aria-label" "Configure providers"
                    , title "Configure providers"
                    , id "open-config"
                    ]
                |> Button.view
            , case model.theme of
                Light ->
                    iconButton "Switch to dark theme" Icon.Moon ToggleTheme

                Dark ->
                    iconButton "Switch to light theme" Icon.Sun ToggleTheme
            ]
        ]


lobbySummary : Model -> String
lobbySummary model =
    case Lobby.peerCount model.lobby of
        0 ->
            "Looking for peers…"

        peers ->
            pluralize "peer" (List.repeat peers ()) ++ " in the lobby"


sharingSummary : Model -> String
sharingSummary model =
    case servableModels model of
        [] ->
            "You share nothing yet."

        shared ->
            "You share " ++ pluralize "model" shared ++ "."


configDialogConfig : Model -> Dialog.Config Msg
configDialogConfig model =
    Dialog.new ConfigMsg [ text "Inference providers" ]
        |> Dialog.withDescription
            [ text "Any server speaking the OpenAI API works. Detected ones are filled in already." ]
        |> Dialog.withRestoreFocusId "open-config"
        |> Dialog.withContentAttributes [ class "sm:max-w-lg!" ]
        |> Dialog.withContent
            [ div [ class "flex flex-col gap-4 max-h-[55vh] overflow-y-auto pt-1 [scrollbar-width:thin]" ]
                (List.map viewProviderRow model.providers)
            , Dialog.footer []
                [ Button.new [ Icon.render Icon.Plus, text "Add provider" ]
                    |> Button.withVariant Button.Outline
                    |> Button.withOnClick AddProvider
                    |> Button.view
                , Button.new [ Icon.render Icon.RefreshCw, text "Scan again" ]
                    |> Button.withOnClick ScanProviders
                    |> Button.view
                ]
            ]


viewProviderRow : Provider -> Html Msg
viewProviderRow configured =
    div [ class "flex flex-col gap-2 rounded-lg border border-border p-3" ]
        [ div [ class "flex items-center gap-2" ]
            [ Input.new
                |> Input.withAriaLabel "Provider name"
                |> Input.withValue configured.label
                |> Input.withOnInput (ProviderLabelChanged configured.id)
                |> Input.withAttributes [ class "h-8! font-medium" ]
                |> Input.view
            , viewProviderStatus configured
            , iconButton ("Remove " ++ configured.label) Icon.Trash2 (RemoveProvider configured.id)
            ]
        , Input.new
            |> Input.withAriaLabel (configured.label ++ " address")
            |> Input.withPlaceholder "http://localhost:11434"
            |> Input.withValue configured.baseUrl
            |> Input.withOnInput (ProviderUrlChanged configured.id)
            |> Input.withAttributes [ class "h-8! text-[13px]!" ]
            |> Input.view
        , Input.new
            |> Input.withType Input.Password
            |> Input.withAriaLabel (configured.label ++ " API key")
            |> Input.withPlaceholder "API key (optional)"
            |> Input.withValue (Maybe.withDefault "" configured.apiKey)
            |> Input.withOnInput (ProviderKeyChanged configured.id)
            |> Input.withAttributes [ class "h-8! text-[13px]!" ]
            |> Input.view
        ]


viewProviderStatus : Provider -> Html Msg
viewProviderStatus configured =
    let
        label =
            case configured.status of
                Provider.Unchecked ->
                    "Not scanned"

                Provider.Checking ->
                    "Scanning…"

                Provider.Unavailable reason ->
                    reason

                Provider.Available available ->
                    pluralize "model" available
    in
    span [ class "ml-auto flex-none text-[11px] text-muted-foreground" ] [ text label ]


viewConfigDialog : Model -> Html Msg
viewConfigDialog model =
    Dialog.view model.config (configDialogConfig model)


sharingDialogConfig : Model -> Dialog.Config Msg
sharingDialogConfig _ =
    Dialog.new SharingMsg [ text "Share your models" ]
        |> Dialog.withDescription
            [ text "Anyone in the lobby can chat with the models you tick here." ]
        |> Dialog.withRestoreFocusId "share-models"


sharingCommandConfig : Model -> Command.Config Msg
sharingCommandConfig model =
    let
        local =
            Provider.localModels model.providers
                |> List.filter
                    (\name ->
                        String.contains
                            (String.toLower model.sharingSearch)
                            (String.toLower name)
                    )
    in
    Command.new "share-models-command" SharingCommandMsg SharingSearchChanged
        |> Command.withInputPlaceholder "Search your models"
        |> Command.withInputValue model.sharingSearch
        |> Command.withContent
            (if List.isEmpty local then
                [ Command.empty [] [ text "No local models to share" ] ]

             else
                [ Command.group []
                    [ text "Your models" ]
                    (List.map
                        (\name ->
                            Command.newItem name name (ToggleShared name) [ text name ]
                                |> Command.withItemChecked (List.member name model.sharedModels)
                        )
                        local
                    )
                ]
            )


viewSharingDialog : Model -> Html Msg
viewSharingDialog model =
    Command.dialog model.sharing
        (sharingDialogConfig model)
        model.sharingCommand
        (sharingCommandConfig model)


viewProviderBadge : List Provider -> Html Msg
viewProviderBadge providers =
    let
        available =
            List.filter (\configured -> not (List.isEmpty (Provider.models configured))) providers

        ( dot, label ) =
            if List.any (\configured -> configured.status == Provider.Checking) providers then
                ( "bg-[#b2b4a9]", "Detecting providers…" )

            else
                case available of
                    [] ->
                        ( "bg-[#b2b4a9]", "No provider detected" )

                    [ only ] ->
                        ( "bg-[#7f9a6b]", only.label ++ " · " ++ pluralize "model" (Provider.localModels available) )

                    _ ->
                        ( "bg-[#7f9a6b]", pluralize "provider" available ++ " · " ++ pluralize "model" (Provider.localModels available) )
    in
    Badge.new [ span [ class ("size-[5px] rounded-full " ++ dot) ] [], text label ]
        |> Badge.withVariant Badge.Outline
        |> Badge.withAttributes [ class "gap-[7px]! rounded-[6px]! border-border! px-[9px]! py-[5px]! text-[10px]! leading-[1.6]! font-normal! text-muted-foreground! max-md:gap-[5px]! max-md:px-1.5! max-md:py-1! max-md:text-[9px]!" ]
        |> Badge.view


pluralize : String -> List a -> String
pluralize noun values =
    case values of
        [ _ ] ->
            "1 " ++ noun

        _ ->
            String.fromInt (List.length values) ++ " " ++ noun ++ "s"


iconButton : String -> Icon.Icon -> Msg -> Html Msg
iconButton label icon msg =
    Button.new [ Icon.render icon ]
        |> Button.withVariant Button.Ghost
        |> Button.withSize Button.Icon
        |> Button.withOnClick msg
        |> Button.withAttributes [ attribute "aria-label" label, title label ]
        |> Button.view


viewWelcome : Model -> Html Msg
viewWelcome model =
    section [ class "m-auto w-full max-w-[800px] px-8 pt-6 pb-[75px] md:max-[1051px]:px-[26px] max-md:px-5 max-md:pt-[30px] max-md:pb-[35px]" ]
        [ div [ class "mb-[33px] text-center max-md:mb-[26px]" ]
            [ div [ class "mb-[22px] max-md:mb-5 [&>span]:size-[52px] [&>span]:rounded-[17px] [&>span]:border [&>span]:border-[#bf683e18] [&>span]:bg-[#bf683e08] [&_svg]:size-[30px]! [&_svg]:stroke-[1.7]!" ] [ brandMark ]
            , p [ class "m-0 mb-[13px] text-[9px] font-semibold tracking-[2px] text-muted-foreground" ] [ text "ROOM FOR A NEW IDEA" ]
            , h1 [ class "m-0 mb-3.5 text-[clamp(27px,3.2vw,37px)] leading-[1.2] font-medium tracking-[-1.25px]" ] [ text "Where should we start?" ]
            , p [ class "m-0 text-[13px] leading-[1.8] text-muted-foreground max-md:mx-auto max-md:max-w-[260px]" ] [ text "Ask a question, explore a thought, or make something great." ]
            ]
        , viewComposer model
        , div [ class "mt-[34px] grid grid-cols-4 gap-3 md:max-[1051px]:gap-2 max-md:mt-6 max-md:grid-cols-2 max-md:gap-[9px]", attribute "aria-label" "Suggested prompts" ]
            [ suggestion Icon.Pencil "Write something" "Find the right words" "Help me write a clear, thoughtful message about "
            , suggestion Icon.Code "Build an idea" "From what if to what's next" "Help me turn this idea into a plan: "
            , suggestion Icon.BookOpen "Learn something" "Make the complex click" "Explain a complex topic in simple terms: "
            , suggestion Icon.Sparkles "Get inspired" "See a different perspective" "Brainstorm some fresh ideas for "
            ]
        ]


suggestion : Icon.Icon -> String -> String -> String -> Html Msg
suggestion icon label description prompt =
    Button.new
        [ Icon.render icon
        , span [ class "text-xs font-medium leading-[1.6]" ] [ text label ]
        , span [ class "mt-0.5 text-[10px] leading-[1.6] text-muted-foreground md:max-[1051px]:hidden" ] [ text description ]
        ]
        |> Button.withVariant Button.Ghost
        |> Button.withOnClick (UseSuggestion prompt)
        |> Button.withAttributes [ class """
                    flex h-auto! min-h-[105px] flex-col items-start! justify-start! gap-0! rounded-[11px]! border border-border! px-[13px]!
                    py-3.5! text-left font-normal! whitespace-normal! shadow-none! [&>svg]:mb-3 [&>svg]:size-[17px]! [&>svg]:stroke-[1.5]
                    [&>svg]:text-muted-foreground md:max-[1051px]:px-2.5! md:max-[1051px]:py-3! max-md:min-h-[95px]
                    """, attribute "aria-label" label ]
        |> Button.view


{-| Describes one model the user can talk to, and where it runs.
-}
type alias Available =
    { name : String
    , local : Bool
    , peers : Int
    }


{-| Merges local models with everything the lobby offers.
-}
availableModels : Model -> List Available
availableModels model =
    let
        local =
            Provider.localModels model.providers

        shared =
            Lobby.models model.lobby

        fromPeers =
            List.map
                (\offer ->
                    { name = offer.model
                    , local = List.member offer.model local
                    , peers = List.length offer.peers
                    }
                )
                shared

        onlyLocal =
            local
                |> List.filter (\name -> not (List.any (\offer -> offer.model == name) shared))
                |> List.map (\name -> { name = name, local = True, peers = 0 })
    in
    List.sortBy .name (fromPeers ++ onlyLocal)


{-| Describes where a model runs, for the picker.
-}
availableLabel : Available -> String
availableLabel available =
    case ( available.local, available.peers ) of
        ( True, 0 ) ->
            "local"

        ( True, peers ) ->
            "local + " ++ pluralize "peer" (List.repeat peers ())

        ( False, peers ) ->
            pluralize "peer" (List.repeat peers ())


modelSelectConfig : Model -> Select.Config Msg
modelSelectConfig model =
    let
        available =
            availableModels model
    in
    Select.new ModelSelectMsg (\_ -> DomFinished)
        |> Select.withAriaLabel "Select model"
        |> Select.withAlign Popup.End
        |> Select.withDisabled (List.isEmpty available)
        |> Select.withPlaceholder
            (if List.isEmpty available then
                "No models"

             else
                "Select model"
            )
        |> Select.withTriggerAttributes [ attribute "aria-describedby" "composer-hint" ]
        |> Select.withContentAttributes [ class "p-2" ]
        |> Select.withContent
            (Select.label [] [ text "Available models" ]
                :: List.map
                    (\model_ ->
                        Select.newItem model_.name model_.name
                            |> Select.withItemContent
                                [ Icon.render Icon.Zap
                                , text model_.name
                                , span [ class "ml-auto pl-3 text-[10px] text-muted-foreground" ]
                                    [ text (availableLabel model_) ]
                                ]
                            |> Select.item
                    )
                    available
            )


viewComposer : Model -> Html Msg
viewComposer model =
    let
        conversation =
            model.current

        streaming =
            conversation.request /= Nothing

        sendable =
            not streaming
                && not (String.isEmpty (String.trim conversation.draft))
                && Chat.selectedModel conversation /= Nothing
    in
    div []
        [ form [ class """
                    rounded-[19px] border border-border bg-background px-[13px] pt-[9px] pb-3
                    shadow-[0_5px_18px_#00000004,0_1px_3px_#00000004] transition-[border-color,box-shadow] duration-150 ease-[ease]
                    focus-within:border-ring focus-within:shadow-[0_0_0_2px_color-mix(in_srgb,var(--ring)_10%,transparent)]
                    """, onSubmit SendMessage ]
            [ Textarea.new
                |> Textarea.withAriaLabel "Message nfrnz"
                |> Textarea.withPlaceholder "Ask anything, or dream up something…"
                |> Textarea.withValue conversation.draft
                |> Textarea.withOnInput DraftChanged
                |> Textarea.withAttributes [ class "min-h-[74px]! max-h-[220px] resize-none rounded-none! border-0! bg-transparent! px-[7px]! py-3! [font-family:inherit] text-sm! leading-[1.6]! shadow-none! focus-visible:outline-none! focus-visible:shadow-none! focus-visible:ring-0! max-md:text-base!", id "composer", rows 2, composerKeys, attribute "aria-describedby" "composer-hint" ]
                |> Textarea.view
            , div [ class "flex items-center justify-between gap-2 pl-1.5" ]
                [ div [ class "ml-auto min-w-0" ]
                    [ Select.view conversation.modelSelect (modelSelectConfig model) ]
                , Button.new [ Icon.render Icon.ArrowUp ]
                    |> Button.withType Button.Submit
                    |> Button.withSize Button.Icon
                    |> Button.withDisabled (not sendable)
                    |> Button.withAttributes [ attribute "aria-label" "Send message", title "Send message" ]
                    |> Button.view
                ]
            ]
        , p [ id "composer-hint", class "m-0 mt-[11px] flex justify-between gap-3 px-1 text-[10px] leading-[1.5] text-muted-foreground max-md:text-[9px] max-md:[&>span]:hidden" ]
            [ text (composerHint model)
            , span [] [ text "Shift + Enter for a new line" ]
            ]
        ]


composerKeys : Html.Attribute Msg
composerKeys =
    preventDefaultOn "keydown"
        (Decode.map3
            (\key shift composing -> ( key, shift, composing ))
            (Decode.field "key" Decode.string)
            (Decode.field "shiftKey" Decode.bool)
            (Decode.oneOf [ Decode.field "isComposing" Decode.bool, Decode.succeed False ])
            |> Decode.andThen
                (\( key, shift, composing ) ->
                    if key == "Enter" && not shift && not composing then
                        Decode.succeed ( SendMessage, True )

                    else
                        Decode.fail "Keep native textarea behavior"
                )
        )


viewConversation : Conversation -> Html Msg
viewConversation conversation =
    section [ class "mx-auto w-full max-w-[800px] px-8 pt-[25px] pb-10 md:max-[1051px]:px-[26px] max-md:p-5", attribute "aria-label" "Conversation" ]
        [ div [ class "m-0 mb-9 text-center text-[9px] tracking-[1.5px] text-muted-foreground" ] [ text "THE START OF SOMETHING" ]
        , Message.group [ class "gap-[27px]!", attribute "role" "log", attribute "aria-live" "polite", attribute "aria-relevant" "additions" ]
            (List.map viewMessage conversation.messages)
        ]


viewMessage : Chat.Message -> Html Msg
viewMessage message =
    case message of
        Chat.Asked question ->
            Message.new
                [ Message.content [ class "items-end" ]
                    [ Message.header [] [ text "You" ]
                    , div [ class "max-w-[85%] rounded-[19px_19px_5px_19px] bg-accent px-[19px] py-[13px] text-sm leading-[1.8] whitespace-pre-wrap text-foreground [overflow-wrap:anywhere]" ] [ text question ]
                    ]
                ]
                |> Message.withAlignment Message.End
                |> Message.view

        Chat.Replied reply state ->
            Message.new
                [ brandMark
                , Message.content []
                    [ Message.header [ class "p-0! text-[13px]! text-foreground!" ] [ text "nfrnz" ]
                    , viewReply reply state
                    ]
                ]
                |> Message.withAttributes [ class "gap-3!" ]
                |> Message.view


viewReply : String -> Chat.ReplyState -> Html Msg
viewReply reply state =
    case state of
        Chat.Failed reason ->
            p [ class "m-0 max-w-[560px] text-sm leading-[1.9] text-destructive", attribute "role" "status" ]
                [ text reason ]

        Chat.Streaming ->
            if String.isEmpty (String.trim reply) then
                p [ class "m-0 max-w-[560px] text-sm leading-[1.9] text-muted-foreground", attribute "role" "status" ]
                    [ text "Thinking…" ]

            else
                replyText reply

        Chat.Complete ->
            replyText reply


replyText : String -> Html Msg
replyText reply =
    p [ class "m-0 max-w-[640px] text-sm leading-[1.9] whitespace-pre-wrap text-foreground [overflow-wrap:anywhere]" ]
        [ text reply ]


composerHint : Model -> String
composerHint model =
    if model.current.request /= Nothing then
        "Generating…"

    else if List.isEmpty (availableModels model) then
        "No models yet · add a provider or wait for peers"

    else
        case Chat.selectedModel model.current of
            Just name ->
                "Talking to " ++ name

            Nothing ->
                "Pick a model to start"
