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
`Html.Attributes.class`. Global styles live in `src/style.css`.

## Production

```sh
bun run build
bun run preview
```

The production build is written to `dist/`.

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

`src/style.css` supplies the semantic color tokens, radius scale, heading font,
sidebar dimensions, and shadcn data-attribute variants. Edit `:root` and `.dark`
to customize the palette. Add the `dark` class to the HTML element to activate
dark mode.

Vite adds the installed package's `src/ChadCn` directory to Tailwind's sources.
The location follows `ELM_HOME` (defaulting to `~/.elm`) and the compiler and
package versions in `elm.json`; no machine-specific path needs to be configured.

## References

- [elm-chadcn setup and component APIs](https://github.com/mbr/elm-chadcn)
- [shadcn theming](https://ui.shadcn.com/docs/theming)
- [Tailwind CSS with Vite](https://tailwindcss.com/docs/installation/using-vite)
