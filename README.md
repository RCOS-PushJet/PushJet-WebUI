# PushJet-LibNotify

WebUI (Google Chrome, electron.js, etc) frontend for PushJet using the WebSockets API.

Anyone on this projects channels will be held to the NodeJS code of conduct,
Found here https://github.com/nodejs/admin/blob/master/CODE_OF_CONDUCT.md

## Limitations

Currently, the `api.pushjet.io` API does not allow CORS, because they do not
set `Access-Control-Allow-Origin: *` as appropriate. As such, we must pass our
app through an nginx reverse proxy; such a configuration can be found in
`nginx.conf` in the repo.
