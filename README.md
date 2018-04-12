# PushJet-WebUI

WebUI (Google Chrome, electron.js, etc) frontend for PushJet using the WebSockets API.

## Limitations

Currently, the `api.pushjet.io` API does not allow CORS, because they do not
set `Access-Control-Allow-Origin: *` as appropriate. As such, we must pass our
app through an nginx reverse proxy; such a configuration can be found in
`nginx.conf` in the repo.
