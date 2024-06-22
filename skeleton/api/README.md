# <% @var name %>

<%- @if description %>
> <% @var description %>
<%- @endif %>

<% @var long-description %>

---

Vinland web framework: https://github.com/lisplizards/vinland

## Installation

### Build

Build the program by running:

```common-lisp
(asdf:make :<% @var name %>)
```

The binary is written to "./bin/<% @var program %>".

### Run

```sh
./bin/<% @var program %> start -p 8080 -a 0.0.0.0 -w 4
```

## Development

First clone the repository under "~/quicklisp/local-projects/" or another location where ASDF can locate the system.

Start server:

```common-lisp
(ql:quickload '("<% @var name %>" "clack"))

(clack:clackup <% @var name %>/app:*app* :server :hunchentoot :port 5000 :use-default-middlewares nil :debug nil)
```

Run tests:

```common-lisp
(asdf:test-system :<% @var name %>)
```

## Dependencies

* [cl-jschema](https://github.com/MauDagos/cl-jschema)
* [clingon](https://github.com/dnaeon/clingon)
* [com.inuoe.jzon](https://github.com/Zulu-Inuoe/jzon)
* [foo.lisp.lack-middleware-debug](https://github.com/lisplizards/lack-middleware-debug)
* [foo.lisp.lack-middleware-head](https://github.com/lisplizards/lack-middleware-head)
* [foo.lisp.lack-middleware-http-methods](https://github.com/lisplizards/lack-middleware-http-methods)
* [foo.lisp.lack-middleware-redis](https://github.com/lisplizards/lack-middleware-redis)
* [foo.lisp.lack-middleware-request-id](https://github.com/lisplizards/lack-middleware-request-id)
* [foo.lisp.lack-middleware-user](https://github.com/lisplizards/lack-middleware-user)
* [foo.lisp.vinland](https://github.com/lisplizards/vinland)
* [frugal-uuid](https://github.com/ak-coram/cl-frugal-uuid)
* [lack](https://github.com/fukamachi/lack/blob/master/lack.asd)
* [lack-middleware-backtrace](https://github.com/fukamachi/lack/blob/master/lack-middleware-backtrace.asd)
* [local-time](https://github.com/dlowe-net/local-time)
* [make-hash](https://github.com/genovese/make-hash)
* [safety-params](https://github.com/fukamachi/safety-params)
* [trivia](https://github.com/guicho271828/trivia)
* [woo](https://github.com/fukamachi/woo)

### Tests

<%- @if parachute %>
* [parachute](https://github.com/Shinmera/parachute)
<%- @endif %>
<%- @if rove %>
* [rove](https://github.com/fukamachi/rove)
<%- @endif %>

<%- @if author %>
## Author

* <% @var author %>

<%- @endif %>
## Copyright

<%- @if author %>
Copyright (c) 2024 <% @var author %>
<%- @else %>
FIXME: add copyright information
<%- @endif %>

<%- @if license %>
## License

<% @var license %>
<%- @endif %>
