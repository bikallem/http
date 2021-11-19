Start hello-world http server.

  $ hello-world&

Test GET method
  $ http -v GET localhost:3000
  GET / HTTP/1.1
  User-Agent: HTTPie/2.6.0
  Accept-Encoding: gzip, deflate, br
  Accept: application/json, */*;q=0.5
  Connection: keep-alive
  Content-Type: application/json
  Host: localhost:3000
  
  
  
  HTTP/1.1 200 OK
  date: Sun, 00 Jan 1900 00:00:00 GMT
  content-length: 11
  
  hello world

Test POST method
  $ http -v POST localhost:3000
  POST / HTTP/1.1
  User-Agent: HTTPie/2.6.0
  Accept-Encoding: gzip, deflate, br
  Accept: application/json, */*;q=0.5
  Connection: keep-alive
  Content-Type: application/json
  Content-Length: 0
  Host: localhost:3000
  
  
  
  HTTP/1.1 200 OK
  date: Sun, 00 Jan 1900 00:00:00 GMT
  content-length: 11
  
  hello world

Shutdown hello-world server.
  $ lsof -ti tcp:3000 | xargs kill
