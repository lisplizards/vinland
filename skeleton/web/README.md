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

```lisp
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

(clack:clackup <% @var name %>/app:*app* :server :hunchentoot :port 5000 :use-default-middlewares nil)
```

Run tests:

```common-lisp
(asdf:test-system :<% @var name %>)
```

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
