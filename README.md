# nfrnz

A blank Elm application using Vite, vite-plugin-elm, and Tailwind CSS.

## Development

```sh
npm install
npm run dev
```

Open the local URL printed by Vite. The application starts as an empty white page.
Edit `src/Main.elm` to build the UI and add Tailwind utility classes with
`Html.Attributes.class`. Global styles live in `src/style.css`.

## Production

```sh
npm run build
npm run preview
```

The production build is written to `dist/`.

## References

- [Elm Vite plugin](https://github.com/hmsk/vite-plugin-elm)
- [Tailwind CSS with Vite](https://tailwindcss.com/docs/installation/using-vite)
