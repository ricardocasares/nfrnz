import './style.css';
import { Elm } from './Main.elm';

const theme = window.localStorage.getItem('theme') ?? 'light';

const app = Elm.Main.init({ flags: { theme } });

app.ports.interopFromElm.subscribe((fromElm) => {
  switch (fromElm.tag) {
    case 'storageSet':
      window.localStorage.setItem(fromElm.data.key, fromElm.data.value);
      break;
  }
});
