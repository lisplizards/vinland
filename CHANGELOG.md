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

[1.2.0] https://github.com/lisplizards/vinland/compare/v1.1.0...v1.2.0

[1.1.0] https://github.com/lisplizards/vinland/compare/v1.0.0...v1.1.0

[1.0.0] https://github.com/lisplizards/vinland/releases/tag/v1.0.0
