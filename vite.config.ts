import { UserConfig } from 'vite'
import fs from 'node:fs';

const https_options = {
  key: fs.readFileSync('certs/privatekey.pem'),
  cert: fs.readFileSync('certs/certificate.pem'),
};

export default {
  root: 'docs/grist',
  publicDir: 'docs',
  server: {
    https: https_options,
    proxy: {
      '/check-data': 'http://localhost:1357/grist',
      '/optim-stream': 'http://localhost:1357/grist',
      '/optim': 'http://localhost:1357/grist',
    }
  },
} satisfies UserConfig
