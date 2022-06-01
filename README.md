irmin-browser-tests
-------------------

Testing various Irmin backend in the browser using the `irmin-test` package. See the results at https://patricoferris.github.io/irmin-browser-tests 

If you want to take the `irmin-server` example for a spin (a client connecting over a websocket), you'll need to run an irmin-server on the appropriate port. This should do the trick:

```
dune exec -- irmin-server/examples/server.exe ws://localhost:9090/ws
```

Also note, Chrome seems to kill all of the websockets probably for security. Try it in Safari or Firefox. Happy Irmin-ing!