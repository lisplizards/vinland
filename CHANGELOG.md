## [1.6.0] - 2024-07-13
### Changed
* Features:
  * Add macro APPEND-RESPONSE-HEADERS
    - change SET-RESPONSE-HEADERS to replace instead of append

* Bugfixes:
  * Fix local variable reference in dynamic-override app
  * Resolve warnings related to unbound special variables

* Other:
  * Add QUICKSTART.md
  * Update README.md

## [1.5.1] - 2024-06-22
### Changed
* Bugfixes:
  * Revert to append instead of nconc for SET-RESPONSE-HEADERS

## [1.5.0] - 2024-06-22
### Changed
* Features:
  * Re-export ROUTE-PATH macro from Raven (path generators)
  * Add ROUTE-URL macro
  * Add list-routes subcommand to CLI skeleton

* Misc:
  * Update README.md
  * Add declaim statements
  * Hoist condition definitions and struct above macros in web.lisp
  * Make RENDER parameter required in RESPOND
  * Optimize redirect functions to resolve warnings: check if append arg is NIL
  * Check that redirect functions do not attempt to set location in HEADERS param

* Bugfixes:
  * Add direct dependency to foo.lisp.lack-middleware-errors to fix compilation
  * Fix symbol reference in app.lisp by providing absolute package path
  * Fix SET-SESSION-OPTIONS subtle bug by copying options list
    - test results had shown difference in behavior between SBCL and ECL

## [1.4.1] - 2024-06-14
### Changed
* Bugfixes:
  * Fix variable reference in dynamic-override errors app

## [1.4.0] - 2024-06-14
### Changed
* Features:
  * Add redact, errors, debug middlewares to skeleton
  * Add errors-app packages for use with errors middleware
  * Add package FOO.LISP.VINLAND.STATIC to generate error files
  * Add http-error package to skeleton
  * Add var `*STATIC-ERRORS-DIRECTORY*` to skeleton
  * Add static files to skeleton

* Misc:
  * Extract FOO.LISP.VINLAND/RESPONSE to FOO.LISP.HTTP-RESPONSE
  * Remove backtrace middleware
  * Add program to .gitignore
  * Specify ':debug nil' clackup option in README.md
  * Add dependencies to skeleton README.md
  * Rename test packages
  * Setup special vars early in %HANDLE-REQUEST
  * Signal CLIENT-ERROR or SERVER-ERROR from %HANDLE-REQUEST
  * Add additional tests for NEGOTIATE macro
  * Call ERROR instead of SIGNAL
  * Revert how to lookup origin host, to be slightly more efficient

* Bugfixes:
  * Fix bug with skeleton generation when name includes dot
  * Use CATCH/THROW for NEGOTIATE macro
    - avoid possible issues when calling RETURN from body clause

## [1.3.0] - 2024-05-28
### Changed
* Features:
  * add project skeleton, web and api flavors

* Misc:
  * Remove handler clause for redis-pool-timeout-error
    - move redis-pool dependency to skeleton
  * Simplify logic to check request host from redirects
  * Update README.md

## [1.2.0] - 2024-05-25
### Changed
* Bugfixes:
  * Resolve warning from Lack session by specifying Samesite
    attribute when setting cookie from DELETE-COOKIE

* Misc:
  * Add dependency: foo.lisp.lack-session-store-redis-pool
    - add handler-case clause for REDIS-POOL-TIMEOUT-ERROR
  * Specify dependencies in README

## [1.1.0] - 2024-05-19
### Changed
* Bugfixes:
  * Set *ORIGIN* correctly
  * Remove unnecessary lexical var around unwind-protect expression
  * Fix response type declarations
  * Fix STATUS-KEYWORD-TO-TEXT
  * Fix fallback clause for NEGOTIATE
  * Fix SET-SESSION-OPTIONS
  * Fix logic: signal when redirecting to different host from
    REDIRECT and REDIRECT-BACK unless ALLOW-OTHER-HOST is T

* Misc:
  * Specify LACK/RESPONSE not old-style LACK.RESPONSE
  * Report on UNKNOWN-STATUS-ERROR
  * Optimize response lookup hash-tables
  * Replace LET* with LET

* Breaking changes:
  * Replace UNAUTHORIZED-REDIRECT-ERROR with two separate conditions:
    - REDIRECT-NOT-ALLOWED-ERROR
    - UNSAFE-REDIRECT-ERROR
  * Change VALIDATE-PARAMS from a function to a macro
  * Change keyword parameter name from ALLOW-OTHER-HOST-P to
    ALLOW-OTHER-HOST for REDIRECT and REDIRECT-BACK

## [1.0.0] - 2024-05-15
### Changed
- Initial check-in
- Add special variables
- Add macros and functions to call from controllers
- Add response utilities
- Add parameters handling generic functions
- Add route/simple Raven sub-protocol handler

[1.5.0] https://github.com/lisplizards/vinland/compare/v1.4.1...v1.5.0

[1.4.1] https://github.com/lisplizards/vinland/compare/v1.4.0...v1.4.1

[1.4.0] https://github.com/lisplizards/vinland/compare/v1.3.0...v1.4.0

[1.3.0] https://github.com/lisplizards/vinland/compare/v1.2.0...v1.3.0

[1.2.0] https://github.com/lisplizards/vinland/compare/v1.1.0...v1.2.0

[1.1.0] https://github.com/lisplizards/vinland/compare/v1.0.0...v1.1.0

[1.0.0] https://github.com/lisplizards/vinland/releases/tag/v1.0.0
