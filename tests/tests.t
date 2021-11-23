Start hello-world http server

  $ hello-world&

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

  $ http -v --ignore-stdin localhost:3000 name=bikal
  POST / HTTP/1.1
  User-Agent: HTTPie/2.6.0
  Accept-Encoding: gzip, deflate, br
  Accept: application/json, */*;q=0.5
  Connection: keep-alive
  Content-Type: application/json
  Content-Length: 17
  Host: localhost:3000
  
  {"name": "bikal"}
  
  HTTP/1.1 200 OK
  date: Sun, 00 Jan 1900 00:00:00 GMT
  content-length: 11
  
  hello world

Shut down hello-world server

  $ lsof -ti tcp:3000 | xargs kill

Start echo http server

  $ echo-server&

  $ http -v --ignore-stdin localhost:3000 name=helloworld
  POST / HTTP/1.1
  User-Agent: HTTPie/2.6.0
  Accept-Encoding: gzip, deflate, br
  Accept: application/json, */*;q=0.5
  Connection: keep-alive
  Content-Type: application/json
  Content-Length: 22
  Host: localhost:3000
  
  {"name": "helloworld"}
  
  HTTP/1.1 200 OK
  date: Sun, 00 Jan 1900 00:00:00 GMT
  content-length: 268
  
  meth: POST
  target: /
  http_version: 1,1
  headers:
   host: localhost:3000
   user-agent: HTTPie/2.6.0
   accept-encoding: gzip, deflate, br
   accept: application/json, */*;q=0.5
   connection: keep-alive
   content-type: application/json
   content-length: 22
  
  {"name": "helloworld"}

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
  content-length: 225
  
  meth: GET
  target: /
  http_version: 1,1
  headers:
   host: localhost:3000
   user-agent: HTTPie/2.6.0
   accept-encoding: gzip, deflate, br
   accept: application/json, */*;q=0.5
   connection: keep-alive
   content-type: application/json
  

  $ lsof -ti tcp:3000 | xargs kill

