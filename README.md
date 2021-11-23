# http
A HTTP 1.1 server with support for mulitple cores and parallel request processing.

Below is the screenshot displaying all cores being used while processing requests generated by `wrk -t12 -c400 -d30s http://localhost:3000`.

![image](https://user-images.githubusercontent.com/315907/143028630-65dc149a-2afc-49fd-9b73-b82885d1d995.png)

# Goals/Considerations
- Able to be used as a HTTP server in resto (https://gitlab.com/nomadic-labs/resto)
- Utilize all cores/CPUs by exploiting OCaml 5.0 features - specifically `Domains` and `effects`.
- Prefer direct style api over monadic api.
- Easy experimentation with various threading models - such as domainslib, eio or some other new mechanisms.
- Maintain Minimal dependencies (?)

# Limitations
- linux-x86 only (due to OCaml 5.0 being x86 only)
- Use unix blocking IO for now (will be replaced by [eio](https://github.com/ocaml-multicore/eio) in the future)

# Non Goals
- Compatibility with lwt/async
