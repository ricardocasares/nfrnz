import './style.css';
import { Elm } from './Main.elm';
import type { FromElm } from './Main.elm';

type StreamRequest = Extract<FromElm, { tag: 'streamStart' }>['data'];

const app = Elm.Main.init({
  flags: {
    theme: window.localStorage.getItem('theme') ?? 'light',
    providers: window.localStorage.getItem('providers'),
    sharedModels: window.localStorage.getItem('sharedModels'),
  },
});

/** Cancels in-flight completions by request id. */
const streams = new Map<string, AbortController>();

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
      break;

    default:
      return fromElm satisfies never;
  }
});

/** Streams one chat completion, forwarding its deltas as they arrive. */
async function stream(request: StreamRequest): Promise<void> {
  const { requestId, baseUrl, apiKey, model, messages } = request;
  const controller = new AbortController();
  streams.set(requestId, controller);

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
      app.ports.interopToElm.send({ tag: 'streamChunk', requestId, content: delta });
    }

    app.ports.interopToElm.send({ tag: 'streamDone', requestId });
  } catch (error) {
    if (!controller.signal.aborted) {
      app.ports.interopToElm.send({
        tag: 'streamError',
        requestId,
        reason: error instanceof Error ? error.message : 'Stream failed',
      });
    }
  } finally {
    streams.delete(requestId);
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
