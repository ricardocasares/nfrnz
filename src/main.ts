import './style.css';
import { Elm } from './Main.elm';

const theme = window.localStorage.getItem('theme') ?? 'light';

const app = Elm.Main.init({ flags: { theme } });

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

    default:
      return fromElm satisfies never;
  }
});
