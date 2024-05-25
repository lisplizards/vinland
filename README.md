# vinland

> Common Lisp web framework

Website: https://www.vinland.dev/

Vinland is a Clack-based web framework built on the [Raven](https://github.com/lisplizards/raven) router.

## Warning

Vinland is BETA software. Breaking API changes may be introduced.

Additionally, struct definitions may be updated from time to time, so it is recommended to restart the Lisp process whenever upgrading.

## Usage

See [vinland-todo-app](https://github.com/lisplizards/vinland-todo-app) for an example of an application built with Vinland and Hotwire.

## Installation

Not in Quicklisp, so clone the repository to "local-projects/".

## Dependencies

* [foo.lisp.lack-middleware-flash](https://github.com/lisplizards/lack-middleware-flash)
* [foo.lisp.lack-request](https://github.com/lisplizards/lack-request)
* [foo.lisp.params](https://github.com/lisplizards/params)
* [foo.lisp.raven](https://github.com/lisplizards/raven)
* [foo.lisp.resource](https://github.com/lisplizards/resource)
* [foo.lisp.lack-session-store-redis-pool](https://github.com/lisplizards/lack-session-store-redis-pool)
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
