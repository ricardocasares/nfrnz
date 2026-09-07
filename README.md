# nfrnz

An Elm application using Vite, vite-plugin-elm-watch, Tailwind CSS, and
[elm-chadcn](https://github.com/mbr/elm-chadcn).

## Development

```sh
bun install
bun run dev
```

Open the local URL printed by the development server.
Edit `src/Main.elm` to build the UI and add Tailwind utility classes with
`Html.Attributes.class`. Theme tokens and Tailwind setup live in `src/style.css`.

## Production

```sh
bun run build
bun run preview
```

The production build is written to `dist/`.

## Elm / TypeScript interop

`src/InteropDefinitions.elm` is the source of truth for flags and port types.
`src/InteropPorts.elm` provides typed flag decoding and port helpers, following
the [elm-ts-interop setup guide](https://elm-ts-interop.com/setup/).

`bun run interop` generates the ignored `src/interop.d.ts`. Development startup
and Elm edits regenerate it automatically; `bun run typecheck` and production
builds regenerate it before checking TypeScript. `src/elm.d.ts` adapts the
generated `Main` type to vite-plugin-elm-watch's default export.

The app currently passes `null` flags and has no port traffic. The generated
`Alert` and `AuthenticatedUser` definitions are starter examples. Replace them
when adding interop, decode flags with `InteropPorts.decodeFlags`, send commands
with `InteropPorts.fromElm`, and subscribe with `InteropPorts.toElm`. Handle
decoder errors in Elm; keep browser/API glue in `src/main.ts`.

## Chat interface

The interface uses elm-chadcn for the sidebar, buttons, search input, composer,
messages, and status badge. Elm owns chat history, per-conversation drafts,
search, theme switching, and desktop/mobile navigation. `src/main.ts` only
loads styles and starts Elm.

Enter submits a message; Shift+Enter adds a newline. Suggested prompts fill the
composer for editing. Chats exist only in memory and reset on refresh. No
inference provider is connected yet, so submitting shows your message and a
connection notice, without generating an AI response.

The composer model dropdown offers Balanced, Fast, and Reasoning preview options.
Each chat remembers its selection for this session; these are placeholders until
an inference provider supplies real models.

## Components and theme

Import components from the `ChadCn` namespace:

```elm
import ChadCn.Badge as Badge
import Html exposing (Html, text)


statusBadge : Html msg
statusBadge =
    Badge.new [ text "Ready" ]
        |> Badge.withVariant Badge.Secondary
        |> Badge.view
```

`src/style.css` supplies the semantic color tokens, radius scale, fonts, and
shadcn data-attribute variants. Edit `:root` and `.dark` to customize the palette.
Layout, sidebar dimensions, responsive states, and component styling use
Tailwind classes directly in `src/Main.elm`. The Elm theme toggle adds `dark`
to the app container.

Vite adds the installed package's `src/ChadCn` directory to Tailwind's sources.
The location follows `ELM_HOME` (defaulting to `~/.elm`) and the compiler and
package versions in `elm.json`; no machine-specific path needs to be configured.

## References

- [elm-chadcn setup and component APIs](https://github.com/mbr/elm-chadcn)
- [shadcn theming](https://ui.shadcn.com/docs/theming)
- [Tailwind CSS with Vite](https://tailwindcss.com/docs/installation/using-vite)
