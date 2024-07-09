# vinland

> Common Lisp web framework

Website: https://www.vinland.dev/

Vinland is a Clack-based web framework built on the [Raven](https://github.com/lisplizards/raven) URL router.

## Warning

Vinland is BETA software. Breaking API changes may be introduced.

Additionally, struct definitions may be updated from time to time, so it is recommended to restart the Lisp process whenever upgrading.

## Getting Started

### Quickstart

Refer to the [Quickstart Guide](https://github.com/lisplizards/vinland/blob/master/docs/QUICKSTART.md) in this repository for instructions on generating a new project and getting started with Vinland.

### Future goals

The Vinland skeleton does not yet provide, but plans to support:

* integration with a configuration library [under development]
* migrations and database integrations (support is planned for at least: cl-migratum, cl-dbi, and postmodern)
* integration with system managers: Qlot, CLPM, ocicl
* integration with a templating library as an alternative to Spinneret
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
* [foo.lisp.http-response](https://github.com/lisplizards/http-response)
* [foo.lisp.lack-middleware-errors](https://github.com/lisplizards/lack-middleware-errors)
* [foo.lisp.lack-middleware-flash](https://github.com/lisplizards/lack-middleware-flash)
* [foo.lisp.lack-request](https://github.com/lisplizards/lack-request)
* [foo.lisp.params](https://github.com/lisplizards/params)
* [foo.lisp.raven](https://github.com/lisplizards/raven)
* [foo.lisp.resource](https://github.com/lisplizards/resource)
* [lack-app-file](https://github.com/fukamachi/lack/blob/master/lack-app-file.asd)
* [lack-component](https://github.com/fukamachi/lack/blob/master/lack-component.asd)
* [lack-middleware-csrf](https://github.com/fukamachi/lack/blob/master/lack-middleware-csrf.asd)
* [lack-response](https://github.com/fukamachi/lack/blob/master/lack-response.asd)
* [uiop](https://gitlab.common-lisp.net/asdf/asdf/-/tree/master/uiop)

### Test dependencies

* [alexandria](https://gitlab.common-lisp.net/alexandria/alexandria)
* [jonathan](https://github.com/rudolph-miller/jonathan)
* [rove](https://github.com/fukamachi/rove)

## Contributing

Open an Issue or Pull Request on GitHub.

If you decide to open a Pull Request, please provide context to your changes: describe the impact, what problem it solves for you, and any alternative approaches you considered.

## Author

* John Newton (<a href="mailto:jnewton@lisplizards.dev">jnewton@lisplizards.dev</a>)

## Copyright

Copyright (c) 2024 John Newton

## License

Apache-2.0
