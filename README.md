# nfrnz

nfrnz is a browser chat for OpenAI-compatible inference servers. It can use a
server running on your own machine, or route a request through another nfrnz
browser that has chosen to share that model.

## Run it

Install dependencies and start the Vite development server:

```sh
bun install
bun run dev
```

Open the local URL printed by Vite. For a production build, run `bun run build`
and serve the generated `dist/` directory; `bun run preview` serves that build
locally.

## Connect an inference server

Open **Inference providers** in the app. nfrnz starts with these provider
addresses and checks each one for models:

| Provider | Default address |
| --- | --- |
| Ollama | `http://localhost:11434` |
| LM Studio | `http://localhost:1234` |
| llama.cpp | `http://localhost:8080` |

The provider must expose the OpenAI-compatible `GET /v1/models` and
`POST /v1/chat/completions` endpoints. Add or edit a provider to use another
server, and enter an API key if it requires one. Since requests come directly
from the browser, the server must be reachable from that browser and allow the
app's origin through CORS.

Choose an available model in the composer, write a prompt, and submit it. The
browser sends the conversation turns to the selected server and displays the
assistant response as it streams back. A failed request is shown in the chat.
The current chat and chat history live in memory, so reloading the page clears
them.

## Share models with peers

nfrnz instances join the same Trystero Nostr lobby. The sidebar reports the
connected peer count and **Share models** lets you choose which of your
currently available local models peers can use. Other peers' offered models
appear in the model picker. If a selected model is available both locally and
through peers, a local provider is used first; otherwise nfrnz asks the first
peer offering it.

When another peer requests a model you share, your browser sends that peer's
conversation turns to your configured inference server and relays the streamed
answer back. Sharing is available to anyone in the lobby: only select models
you are willing to serve, and remember that requests include conversation
content. Provider API keys stay in your browser and are not sent to peers.
Incoming requests are limited to 50 conversation turns.

## What runs where

Elm owns the interface and application behavior: provider configuration,
discovery, model selection, chat state, and peer request routing. `src/main.ts`
handles browser-only work: local storage, theme integration, fetch-based
streaming, and the Trystero connection. `src/InteropDefinitions.elm` defines
the typed Elm/TypeScript messages; `src/InteropPorts.elm` exposes their ports.
After changing interop definitions, regenerate TypeScript declarations with:

```sh
bun run interop
```

Provider addresses and API keys, the theme, and selected shared models persist
in browser local storage. Chat transcripts do not persist. The UI uses
elm-chadcn components and Tailwind CSS; theme tokens and global styles are in
`src/style.css`.
