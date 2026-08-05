import { defineConfig } from 'vite'
import fs from 'node:fs';
import { resolve } from 'node:path'

const https_options = {
  key: fs.readFileSync('certs/localhost+2-key.pem'),
  cert: fs.readFileSync('certs/localhost+2.pem'),
};

const root = 'docs/grist'
export default defineConfig({
  root,
  "base": "./",

  build: {
    rolldownOptions: {
      input: {
        main: resolve(import.meta.dirname, root, 'index.html'),
        plannings: resolve(import.meta.dirname, root, 'plannings.html'),
      },
    },
  },

  server: {
    https: https_options,
    cors: true,
    proxy: {
      '/check-data': 'http://localhost:1357/grist',
      '/optim-stream': 'http://localhost:1357/grist',
      '/optim': 'http://localhost:1357/grist',
    }
  },
})
