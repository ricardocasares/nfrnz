import { readFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { defineConfig, normalizePath } from 'vite';
import elm from 'vite-plugin-elm';
import css from '@tailwindcss/vite';

const root = fileURLToPath(new URL('.', import.meta.url));
const elmJson = JSON.parse(readFileSync(resolve(root, 'elm.json'), 'utf8'));
const chadcnSource = normalizePath(resolve(
  process.env.ELM_HOME ?? resolve(homedir(), '.elm'),
  elmJson['elm-version'],
  'packages/mbr/elm-chadcn',
  elmJson.dependencies.direct['mbr/elm-chadcn'],
  'src/ChadCn',
));

export default defineConfig({
  plugins: [
    elm(),
    {
      name: 'chadcn-tailwind-source',
      enforce: 'pre',
      transform(code, id) {
        if (normalizePath(id.split('?')[0]!) === normalizePath(resolve(root, 'src/style.css'))) {
          return { code: `${code}\n@source ${JSON.stringify(chadcnSource)};`, map: null };
        }
      },
    },
    css(),
  ],
});
