import './style.css';
import { joinRoom } from '@trystero-p2p/nostr';
import { Elm } from './Main.elm';
import type { FromElm } from './Main.elm';

type StreamRequest = Extract<FromElm, { tag: 'streamStart' }>['data'];
type PeerRequest = Extract<FromElm, { tag: 'askPeer' }>['data'];
type PeerChunk = { requestId: string; content: string };
type PeerEnd = { requestId: string; reason: string | null };

const app = Elm.Main.init({
  flags: {
    theme: window.localStorage.getItem('theme') ?? 'light',
    providers: window.localStorage.getItem('providers'),
    sharedModels: window.localStorage.getItem('sharedModels'),
  },
});

/** Cancels in-flight completions by request id. */
const streams = new Map<string, AbortController>();

/** Names the peer a relayed completion streams out to, by request id. */
const relays = new Map<string, string>();

/** Remembers which peer we asked, so a disconnect can end that request. */
const asked = new Map<string, string>();

const room = joinRoom({ appId: 'nfrnz-lobby-v1' }, 'lobby');
const models = room.makeAction<string[]>('models');
const ask = room.makeAction<PeerRequest>('ask');
const chunks = room.makeAction<PeerChunk>('chunk');
const ends = room.makeAction<PeerEnd>('end');

/** The models we currently share, rebroadcast to peers that arrive later. */
let shared: string[] = [];

room.onPeerJoin = (peerId) => {
  app.ports.interopToElm.send({ tag: 'peerJoined', peerId });
  void models.send(shared, { target: peerId });
};

room.onPeerLeave = (peerId) => {
  app.ports.interopToElm.send({ tag: 'peerLeft', peerId });

  for (const [requestId, servingPeer] of asked) {
    if (servingPeer !== peerId) continue;
    asked.delete(requestId);
    app.ports.interopToElm.send({ tag: 'streamError', requestId, reason: 'Peer disconnected' });
  }

  for (const [requestId, listener] of relays) {
    if (listener !== peerId) continue;
    streams.get(requestId)?.abort();
    streams.delete(requestId);
    relays.delete(requestId);
  }
};

models.onMessage = (shares, { peerId }) => {
  app.ports.interopToElm.send({
    tag: 'peerModelsUpdated',
    peerId,
    models: Array.isArray(shares) ? shares.filter((model) => typeof model === 'string') : [],
  });
};

ask.onMessage = (request, { peerId }) => {
  app.ports.interopToElm.send({ tag: 'peerAsked', ...request, peerId });
};

chunks.onMessage = (chunk, { peerId }) => {
  if (asked.get(chunk.requestId) !== peerId) return;
  app.ports.interopToElm.send({
    tag: 'streamChunk',
    requestId: chunk.requestId,
    content: chunk.content,
  });
};

ends.onMessage = (end, { peerId }) => {
  if (asked.get(end.requestId) !== peerId) return;
  asked.delete(end.requestId);

  app.ports.interopToElm.send(
    end.reason === null
      ? { tag: 'streamDone', requestId: end.requestId }
      : { tag: 'streamError', requestId: end.requestId, reason: end.reason },
  );
};

app.ports.interopFromElm.subscribe((fromElm) => {
  switch (fromElm.tag) {
    case 'storageSet':
      window.localStorage.setItem(fromElm.data.key, fromElm.data.value);
      break;

    case 'storageGet':
      app.ports.interopToElm.send({
        tag: 'storageValue',
        key: fromElm.data.key,
        value: window.localStorage.getItem(fromElm.data.key),
      });
      break;

    case 'streamStart':
      void stream(fromElm.data);
      break;

    case 'streamCancel':
      streams.get(fromElm.data.requestId)?.abort();
      streams.delete(fromElm.data.requestId);
      asked.delete(fromElm.data.requestId);
      break;

    case 'shareModels':
      shared = fromElm.data.models;
      void models.send(shared);
      break;

    case 'askPeer':
      asked.set(fromElm.data.requestId, fromElm.data.peerId);
      void ask.send(fromElm.data, { target: fromElm.data.peerId });
      break;

    case 'rejectPeer':
      void ends.send(
        { requestId: fromElm.data.requestId, reason: fromElm.data.reason },
        { target: fromElm.data.peerId },
      );
      break;

    default:
      return fromElm satisfies never;
  }
});

/** Streams one chat completion, forwarding its deltas as they arrive. */
async function stream(request: StreamRequest): Promise<void> {
  const { requestId, replyTo, baseUrl, apiKey, model, messages } = request;
  const controller = new AbortController();
  streams.set(requestId, controller);
  if (replyTo !== undefined) relays.set(requestId, replyTo);

  const emitChunk = (content: string) =>
    replyTo === undefined
      ? app.ports.interopToElm.send({ tag: 'streamChunk', requestId, content })
      : void chunks.send({ requestId, content }, { target: replyTo });

  const emitEnd = (reason: string | null) =>
    replyTo === undefined
      ? app.ports.interopToElm.send(
          reason === null
            ? { tag: 'streamDone', requestId }
            : { tag: 'streamError', requestId, reason },
        )
      : void ends.send({ requestId, reason }, { target: replyTo });

  try {
    const response = await fetch(`${baseUrl}/v1/chat/completions`, {
      method: 'POST',
      signal: controller.signal,
      headers: {
        'Content-Type': 'application/json',
        ...(apiKey ? { Authorization: `Bearer ${apiKey}` } : {}),
      },
      body: JSON.stringify({ model, messages, stream: true }),
    });

    if (!response.ok || !response.body) {
      throw new Error(`Provider responded with ${response.status}`);
    }

    for await (const delta of deltas(response.body)) {
      emitChunk(delta);
    }

    emitEnd(null);
  } catch (error) {
    if (!controller.signal.aborted) {
      emitEnd(error instanceof Error ? error.message : 'Stream failed');
    }
  } finally {
    streams.delete(requestId);
    relays.delete(requestId);
  }
}

/** Yields the text deltas of an OpenAI-compatible server-sent event stream. */
async function* deltas(body: ReadableStream<Uint8Array>): AsyncGenerator<string> {
  const reader = body.getReader();
  const decoder = new TextDecoder();
  let buffer = '';

  while (true) {
    const { done, value } = await reader.read();
    if (done) break;

    buffer += decoder.decode(value, { stream: true });
    const lines = buffer.split('\n');
    buffer = lines.pop() ?? '';

    for (const line of lines) {
      if (!line.startsWith('data:')) continue;

      const payload = line.slice('data:'.length).trim();
      if (payload === '' || payload === '[DONE]') continue;

      const delta = JSON.parse(payload)?.choices?.[0]?.delta?.content;
      if (typeof delta === 'string' && delta !== '') yield delta;
    }
  }
}
