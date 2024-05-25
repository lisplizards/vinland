# vinland

> Common Lisp web framework

Website: https://www.vinland.dev/

Vinland is a Clack-based web framework built on the [Raven](https://github.com/lisplizards/raven) router.

## Warning

Vinland is BETA software. Breaking API changes may be introduced.

Additionally, struct definitions may be updated from time to time, so it is recommended to restart the Lisp process whenever upgrading.

## Getting Started

### Generate project

Vinland provides a project generator, contained in system `FOO.LISP.VINLAND.SKELETON`. Use of the skeleton is optional but recommended: it's the easiest way to get started, and you might want to at least try it in order to gain an understanding of the framework and its capabilities.

You can generate a project in one of two flavors, "web" (the default) or "api".

Among other options, you can also choose the test framework ("parachute" or "rove"); the default is "parachute".

Web-flavor example: "web" is the default, but for clarity here we explicitly specify the option.

```common-lisp
(ql:quickload "foo.lisp.vinland.skeleton")

(foo.lisp.vinland.skeleton:make-project
  :name :web-demo
  :license "MIT"
  :author "Erik the Red"
  :flavor "web")

```

API-flavor example: specify a non-default name for the binary executable and rove as the test framework.

```common-lisp
(ql:quickload "foo.lisp.vinland.skeleton")

(foo.lisp.vinland.skeleton:make-project
  :name :api-demo
  :program "api-server"
  :author "Bjarni Herjólfsson"
  :maintainer "Leif Erikson"
  :flavor "api"
  :test-framework "rove")

```

Inspect the complete list of options by running:

```common-lisp
(documentation 'foo.lisp.vinland.skeleton:make-project 'function)
```

For production, you can build a binary executable for your platform by running `(asdf:make :demo-app)`.

In development, you can start your application by calling `CLACK:CLACKUP`.

```common-lisp
(ql:quickload '("demo-app" "clack"))

(clack:clackup demo-app/app:*app* :server :hunchentoot :port 5000 :use-default-middlewares nil)
```

### Rough edges

The Vinland skeleton does not yet provide, but plans to support:
* custom error pages (you can define your own middleware for this as a workaround)
* migrations and database integrations (support is planned for at least: cl-migratum, cl-dbi, and postmodern)
* functions for easily starting and stopping the server in development
* integration with a configuration library
* integration with system managers: Qlot, CLPM, ocicl
* integration with a templating library (currently supports only Spinneret)
* integration with a JavaScript bundler as an alternative to importmaps

These features will be added in approximately the order listed above.

### Demo app

See [vinland-todo-app](https://github.com/lisplizards/vinland-todo-app) for an example of an application built with Vinland and Hotwire.

## Installation

Not in Quicklisp, so clone the repository to "local-projects/".

## Development

Run tests:

```common-lisp
(ql:quickload "foo.lisp.vinland")

(asdf:test-system :foo.lisp.vinland)
```

## Dependencies

* [cl-project](https://github.com/fukamachi/cl-project)
* [foo.lisp.lack-middleware-flash](https://github.com/lisplizards/lack-middleware-flash)
* [foo.lisp.lack-request](https://github.com/lisplizards/lack-request)
* [foo.lisp.params](https://github.com/lisplizards/params)
* [foo.lisp.raven](https://github.com/lisplizards/raven)
* [foo.lisp.resource](https://github.com/lisplizards/resource)
* [lack-middleware-csrf](https://github.com/fukamachi/lack/blob/master/lack-middleware-csrf.asd)
* [lack-response](https://github.com/fukamachi/lack/blob/master/lack-response.asd)
* [uiop](https://gitlab.common-lisp.net/asdf/asdf/-/tree/master/uiop)

## Contributing

Open an Issue or Pull Request on GitHub.

If you decide to open a Pull Request, please provide context to your changes: describe the impact, what problem it solves for you, and any alternative approaches you considered.

## Author

* John Newton (<a href="mailto:jnewton@lisplizards.dev">jnewton@lisplizards.dev</a>)

## Copyright

Copyright (c) 2024 John Newton

## License

Apache-2.0
