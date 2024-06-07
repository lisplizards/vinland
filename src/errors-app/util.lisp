(in-package #:foo.lisp.vinland/errors-app/util)

(defvar *root-directory* nil
  "Pathname of the root directory containing static error files; dynamically bound.")

(defvar *media-type-fallback* nil
  "Media-type string that is a fallback when the request Accept header does not match
any of the provided values for the static or dynamic media-types; dynamically bound.")

(defvar *handlers* nil
  "Association list containing a media-type string as each cons pair key and the symbol
corresponding to a body-returning function as the datum; dynamically bound.")

(defvar *handler-media-types* nil
  "List of media-type strings for which handler functions are defined; dynamically bound.")

(defvar *static-file-types* nil
  "Association list containing a media-type string as each cons pair key and file-type string
as the datum; dynamically bound.")

(defvar *static-file-namestrings* nil
  "List of namestrings representing files found in the specified root directory,
*ROOT-DIRECTORY*; dynamically bound.")

(defvar *static-media-types* nil
  "List of media-type strings for which static files are defined; dynamically bound.")

(defparameter *default-static-file-types*
  '(("text/html" . "html")
    ("application/json" . "json"))
  "Association list that is a mapping between media-types and file-type extensions. The
default values for the STATIC-FILE-TYPES parameter of function MAKE-APP.")

(defparameter *http-error-response-status-codes*
  (loop for (status-code text keyword) on foo.lisp.http-response::*status-codes*
        when (typep status-code 'foo.lisp.http-response:status-code-error)
        collect status-code)
  "List of all HTTP error response status codes, as defined in FOO.LISP.HTTP-RESPONSE::*STATUS-CODES*.
May be used as a value for REQUIRED-STATIC-RESPONSE-CODES and/or REQUIRED-DYNAMIC-RESPONSE-CODES when
calling MAKE-APP.")

(defparameter *recommended-http-error-response-status-codes*
  '(400 401 403 404 405 406 409 413 414 415 429 431 500 501 503)
  "List of HTTP error response status codes that are recommended to be handled. Used as
the default values for MAKE-APP parameters REQUIRED-STATIC-RESPONSE-CODES and
REQUIRED-DYNAMIC-RESPONSE-CODES.")
