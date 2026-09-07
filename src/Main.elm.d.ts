export type JsonObject = { [Key in string]?: JsonValue };
export type JsonArray = JsonValue[];

/**
Matches any valid JSON value.
Source: https://github.com/sindresorhus/type-fest/blob/master/source/basic.d.ts
*/
export type JsonValue =
  | string
  | number
  | boolean
  | null
  | JsonObject
  | JsonArray;

export interface ElmApp {
  ports: {
    interopFromElm: PortFromElm<FromElm>;
    interopToElm: PortToElm<ToElm>;
    [key: string]: UnknownPort;
  };
}

export type FromElm = { data : { requestId : string }; tag : "streamCancel" } | { data : { apiKey? : string; baseUrl : string; messages : { content : string; role : string }[]; model : string; replyTo? : string; requestId : string }; tag : "streamStart" } | { data : { key : string }; tag : "storageGet" } | { data : { key : string; value : string }; tag : "storageSet" };

export type ToElm = { key : string; tag : "storageValue"; value : string | null } | { content : string; requestId : string; tag : "streamChunk" } | { requestId : string; tag : "streamDone" } | { reason : string; requestId : string; tag : "streamError" };

export type Flags = { providers : string | null; sharedModels : string | null; theme : string };

export namespace Main {
  function init(options: { node?: HTMLElement | null; flags: Flags }): ElmApp;
}

export as namespace Elm;

export { Elm };

export type UnknownPort = PortFromElm<unknown> | PortToElm<unknown> | undefined;

export type PortFromElm<Data> = {
  subscribe(callback: (fromElm: Data) => void): void;
  unsubscribe(callback: (fromElm: Data) => void): void;
};

export type PortToElm<Data> = { send(data: Data): void };
