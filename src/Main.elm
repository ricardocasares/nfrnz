module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Nav
import ChadCn.Badge as Badge
import ChadCn.Button as Button
import ChadCn.Icon as Icon
import ChadCn.Input as Input
import ChadCn.Message as Message
import ChadCn.Popup.Types as Popup
import ChadCn.Select as Select
import ChadCn.Sidebar as Sidebar
import ChadCn.Textarea as Textarea
import Html exposing (Html, div, form, h1, header, main_, p, section, span, text)
import Html.Attributes exposing (attribute, class, classList, id, rows, title)
import Html.Events exposing (onClick, onSubmit, preventDefaultOn)
import InteropDefinitions exposing (FromElm(..), ToElm(..))
import InteropPorts
import Json.Decode as Decode
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
    }


type alias Conversation =
    { id : Int
    , messages : List String
    , draft : String
    , modelSelect : Select.Model
    }


type Theme
    = Light
    | Dark


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
                    [ Select.subscriptions modelSelectConfig model.current.modelSelect
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
        theme =
            InteropPorts.decodeFlags flagsValue
                |> Result.toMaybe
                |> Maybe.andThen (\flags -> themeFromString flags.theme)
                |> Maybe.withDefault Light
    in
    ( initialModel url key theme, Cmd.none )


initialModel : Url -> Nav.Key -> Theme -> Model
initialModel url key theme =
    { key = key
    , url = url
    , current = emptyConversation 0
    , history = []
    , nextId = 1
    , sidebar = Sidebar.Expanded
    , mobileSidebar = False
    , search = ""
    , theme = theme
    }


emptyConversation : Int -> Conversation
emptyConversation conversationId =
    { id = conversationId
    , messages = []
    , draft = ""
    , modelSelect = Select.init "model-select" (Just "preview-balanced")
    }


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
    if List.isEmpty model.current.messages then
        model.history

    else
        let
            current =
                model.current
        in
        { current | modelSelect = Select.init "model-select" (Select.selected current.modelSelect) } :: model.history


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
                    Select.update modelSelectConfig selectMsg current.modelSelect
            in
            ( { model | current = { current | modelSelect = modelSelect } }, cmd )

        SendMessage ->
            case String.trim current.draft of
                "" ->
                    ( model, Cmd.none )

                content ->
                    ( { model
                        | current = { current | messages = current.messages ++ [ content ], draft = "" }
                        , search = ""
                      }
                    , Cmd.batch [ focusComposer, scrollToLatest ]
                    )

        NewChat ->
            if List.isEmpty current.messages then
                ( { model | mobileSidebar = False, search = "" }, focusComposer )

            else
                ( { model
                    | current = emptyConversation model.nextId
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
            ( { model | current = emptyConversation model.nextId, nextId = model.nextId + 1 }, focusComposer )

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

        InteropMsgReceived (Err _) ->
            ( model, Cmd.none )


conversationTitle : Conversation -> String
conversationTitle conversation =
    List.head conversation.messages |> Maybe.withDefault "New chat"


view : Model -> Browser.Document Msg
view model =
    { title = conversationTitle model.current ++ " · nfrnz"
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
            , Button.new []
                |> Button.withVariant Button.Ghost
                |> Button.withOnClick CloseMobileSidebar
                |> Button.withAttributes [ classList [ ( "hidden!", not model.mobileSidebar ), ( "fixed! inset-0! z-20! size-full! rounded-none! border-0! bg-black/40! md:hidden!", model.mobileSidebar ) ], attribute "aria-label" "Close sidebar" ]
                |> Button.view
            , main_ [ class "flex min-w-0 flex-1 flex-col" ]
                [ viewHeader model
                , div [ id "chat-scroll", class "min-h-0 flex-1 overflow-y-auto overscroll-contain [scrollbar-width:thin]", classList [ ( "flex", List.isEmpty model.current.messages ) ] ]
                    [ if List.isEmpty model.current.messages then
                        viewWelcome model

                      else
                        viewConversation model.current
                    ]
                , if List.isEmpty model.current.messages then
                    text ""

                  else
                    div [ class "mx-auto w-full max-w-[800px] flex-none px-8 pt-3 md:max-[1051px]:px-[26px] max-md:px-3.5 max-md:pt-2.5" ] [ viewComposer model.current ]
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
                            (String.toLower (String.join " " chat.messages))
                    )

        historyGroup =
            List.foldl
                (\conversation group ->
                    Sidebar.newItem (conversationTitle conversation) (SelectChat conversation.id)
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
                    [ div [ class """
                    flex items-start gap-2.5 px-[9px] pt-[15px] pb-[22px] text-xs leading-[1.6] text-muted-foreground
                    md:group-data-[state=collapsed]/sidebar:hidden [&_svg]:mt-0.5 [&_svg]:size-4 [&_svg]:flex-none [&_p]:m-0 [&_span]:block
                    [&_span]:text-[11px] [&_span]:opacity-80
                    """ ]
                        [ Icon.render Icon.MessageCircle
                        , p [] [ text "A space for your ideas.", span [] [ text "Chats stay in this session." ] ]
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
            , span [ class "max-w-60 truncate text-[13px] text-muted-foreground max-md:hidden" ] [ text (conversationTitle model.current) ]
            ]
        , div [ class "flex min-w-0 flex-none items-center gap-2.5 text-muted-foreground max-md:gap-[3px]" ]
            [ Badge.new [ span [ class "size-[5px] rounded-full bg-[#b2b4a9]" ] [], text "No model connected" ]
                |> Badge.withVariant Badge.Outline
                |> Badge.withAttributes [ class "gap-[7px]! rounded-[6px]! border-border! px-[9px]! py-[5px]! text-[10px]! leading-[1.6]! font-normal! text-muted-foreground! max-md:gap-[5px]! max-md:px-1.5! max-md:py-1! max-md:text-[9px]!" ]
                |> Badge.view
            , if List.isEmpty model.current.messages then
                text ""

              else
                iconButton "Delete current chat" Icon.Trash2 DeleteCurrentChat
            , case model.theme of
                Light ->
                    iconButton "Switch to dark theme" Icon.Moon ToggleTheme

                Dark ->
                    iconButton "Switch to light theme" Icon.Sun ToggleTheme
            ]
        ]


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
        , viewComposer model.current
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


modelSelectConfig : Select.Config Msg
modelSelectConfig =
    Select.new ModelSelectMsg (\_ -> DomFinished)
        |> Select.withAriaLabel "Select model"
        |> Select.withAlign Popup.End
        |> Select.withTriggerAttributes [ attribute "aria-describedby" "composer-hint" ]
        |> Select.withContentAttributes [ class "p-2" ]
        |> Select.withContent
            (Select.label [] [ text "Select model" ]
                :: List.map
                    (\( modelId, label ) ->
                        Select.newItem modelId label
                            |> Select.withItemContent [ Icon.render Icon.Zap, text label ]
                            |> Select.item
                    )
                    [ ( "preview-balanced", "Balanced" )
                    , ( "preview-fast", "Fast" )
                    , ( "preview-reasoning", "Reasoning" )
                    ]
            )


viewComposer : Conversation -> Html Msg
viewComposer conversation =
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
                    [ Select.view conversation.modelSelect modelSelectConfig ]
                , Button.new [ Icon.render Icon.ArrowUp ]
                    |> Button.withType Button.Submit
                    |> Button.withSize Button.Icon
                    |> Button.withDisabled (String.isEmpty (String.trim conversation.draft))
                    |> Button.withAttributes [ attribute "aria-label" "Send message", title "Send message" ]
                    |> Button.view
                ]
            ]
        , p [ id "composer-hint", class "m-0 mt-[11px] flex justify-between gap-3 px-1 text-[10px] leading-[1.5] text-muted-foreground max-md:text-[9px] max-md:[&>span]:hidden" ]
            [ text "Interface preview · No AI responses yet"
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
        , Message.group [ class "gap-[27px]! [&_[data-slot=message-content]]:items-end", attribute "role" "log", attribute "aria-live" "polite", attribute "aria-relevant" "additions" ]
            (List.map
                (\content ->
                    Message.new
                        [ Message.content []
                            [ Message.header [] [ text "You" ]
                            , div [ class "max-w-[85%] rounded-[19px_19px_5px_19px] bg-accent px-[19px] py-[13px] text-sm leading-[1.8] whitespace-pre-wrap text-foreground [overflow-wrap:anywhere]" ] [ text content ]
                            ]
                        ]
                        |> Message.withAlignment Message.End
                        |> Message.view
                )
                conversation.messages
            )
        , Message.new
            [ brandMark
            , Message.content []
                [ Message.header [ class "p-0! text-[13px]! text-foreground!" ] [ text "nfrnz" ]
                , p [ class "m-0 max-w-[560px] text-sm leading-[1.9] text-muted-foreground", attribute "role" "status" ]
                    [ text "Your conversation is ready. Once an inference provider is connected, AI responses will appear here." ]
                ]
            ]
            |> Message.withAttributes [ class "mt-[30px] gap-3!" ]
            |> Message.view
        ]
