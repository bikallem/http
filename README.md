# tez-http
A HTTP 1.1 server possibly for use with tezos-node

# Goals/Considerations
- Based on OCaml 5.0 features - specifically `Domains` and `effects`.
- Preference on direct style code.
- Easy experimentation with various threading models - such as domainslib, eio or some other new mechanisms.
- Compatibility with lwt/async is a non-goal.
- Able to be used as a HTTP server in resto (https://gitlab.com/nomadic-labs/resto)
- linux-x86 only (due to OCaml 5.0 being x86 only) 
- minimal dependencies (?)
- Able to build on (4.12+domains or perhaps 4.14+domains?)
