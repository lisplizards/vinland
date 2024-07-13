;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.vinland/errors-app/simple/dynamic-override)

(declaim (type (or null function) *dynamic-override-p*))
(defvar *dynamic-override-p* nil
  "Function for which the result represents whether to serve a static file (NIL return value)
or call the dynamic override function (non-NIL return value); dynamically bound.")

(declaim (type function *default-dynamic-override-p*))
(defparameter *default-dynamic-override-p*
  (lambda (env)
    (declare (ignore env))
    nil)
  "Lambda that takes a Clack ENV and returns NIL. Default value for parameter DYNAMIC-OVERRIDE-P
of function MAKE-APP.")

(declaim (ftype (function (&key (:root pathname)
                                (:required-static-response-codes list)
                                (:required-handler-response-codes list)
                                (:static-file-types list)
                                (:handlers list)
                                (:media-type-fallback string)
                                (:dynamic-override-p function))
                          function)
                make-app))
(defun make-app (&key root
                   (required-static-response-codes ())
                   (required-handler-response-codes ())
                   (static-file-types *default-static-file-types*)
                   (handlers ())
                   (media-type-fallback "text/html")
                   (dynamic-override-p *default-dynamic-override-p*))
  "Returns a Clack application (a function that takes an ENV parameter and returns a
response list) that presents an error page to the user, with all the expected special
variables, including FOO.LISP.VINLAND:*REQUEST* and FOO.LISP.VINLAND:*RESPONSE*. Allows for
rendering static error files while also allowing for a user-provided handler to \"override\"
the static response, otherwise signals a CLIENT-ERROR with status code 406 if neither a
static file or handler function is defined.

When the provided function DYNAMIC-OVERRIDE-P returns a non-NIL value, calls the dynamic
handler for the matched media-type and response code found in HANDLERS,
an association list. When DYNAMIC-OVERRIDE-P returns NIL, renders the static file found in
ROOT, a directory pathname, matching the media-type type and response code, e.g., 404.html.
Matches a media-type by comparing the keys of HANDLERS or STATIC-FILE-TYPES to the
values found in the request Accept header (media type negotiation).

The list of static files found in ROOT is constructed when the app lambda is instantiated
in order to avoid unnecessary filesystem operations at runtime. This means in effect that
the Lisp process should be restarted whenever adding or removing a static error file.

When a dynamic handler or static file cannot be matched against the request Accept header,
falls back to MEDIA-TYPE-FALLBACK, which must be represented in both STATIC-FILE-TYPES and
HANDLERS association lists.

Signals FOO.LISP.VINLAND/WEB:CLIENT-ERROR with status-code 406 when a static file or dynamic
handler cannot be found for the given matched media-type and parsed response code.

Options:
* ROOT: root directory from which static files are served; a pathname
* REQUIRED-STATIC-RESPONSE-CODES: list of HTTP response status error codes for which static files must be defined for each media-type
* REQUIRED-HANDLER-RESPONSE-CODES: list of HTTP response status error codes for which handlers must be defined for each media-type
* STATIC-FILE-TYPES: association list: each pair has a media-type key and file-type extension as the datum
* HANDLERS: association list of dynamic handlers: each pair has a media-type key and an association list of response-codes and handlers as the datum
* MEDIA-TYPE-FALLBACK: default media-type when the request Accept header does not overlap with the defined media-types; a string
* DYNAMIC-OVERRIDE-P: function to check whether to render a dynamic handler; NIL return value indicates should render a static file
"
  (declare (type pathname root)
           (type list
                 required-static-response-codes
                 required-handler-response-codes
                 static-file-types
                 handlers)
           (type string media-type-fallback)
           (type function dynamic-override-p))
  (check-type root pathname)
  (assert (uiop:directory-pathname-p root)
          nil
          "ROOT (~A) is not a directory" root)
  (check-type dynamic-override-p function)
  (check-type static-file-types list)
  (check-type handlers list)
  (check-type media-type-fallback string)
  (assert (not (null static-file-types)))
  (assert (not (null handlers)))
  (dolist (pair static-file-types)
    (check-type pair cons)
    (destructuring-bind (media-type . file-type)
        pair
      (check-type media-type string)
      (check-type file-type string)))
  (multiple-value-bind (static-files static-file-namestrings static-media-types)
      (lack/middleware/errors/util:collect-static-files root :file-types static-file-types)
    (declare (ignore static-files))
    (dolist (pair handlers)
      (check-type pair cons)
      (destructuring-bind (media-type . handler-list)
          pair
        (check-type media-type string)
        (check-type handler-list list)
        (dolist (pair handler-list)
          (destructuring-bind (response-code . handler)
              pair
            (check-type response-code foo.lisp.http-response:status-code-error)
            (check-type handler symbol)
            (assert (fboundp handler))
            (let ((static-file-type (cdr (assoc media-type static-file-types
                                                :test #'string=))))
              (assert (not (null static-file-type))
                      nil
                      "Media-type not defined for static files: ~A" media-type))))))
    (assert (member media-type-fallback static-media-types :test #'string=)
            nil
            "MEDIA-TYPE-FALLBACK (~A) must be represented in STATIC-FILE-TYPES"
            media-type-fallback)
    (assert (assoc media-type-fallback handlers :test #'string=)
            nil
            "MEDIA-TYPE-FALLBACK (~A) must be represented in HANDLERS"
            media-type-fallback)
    (let ((handler-media-types (mapcar #'car handlers)))
      (declare (type list static-media-types))
      (dolist (media-type static-media-types)
        (check-type media-type string)
        (let ((file-type (cdr (assoc media-type static-file-types :test #'equal))))
          (check-type file-type string)
          (dolist (response-code required-static-response-codes)
            (let ((expected-file-namestring (format nil "~D.~A" response-code file-type)))
              (assert (member expected-file-namestring static-file-namestrings :test #'equal)
                      nil
                      "Expected static file to exist: ~A" expected-file-namestring)))))
      (dolist (media-type handler-media-types)
        (check-type media-type string)
        (let ((handler-list (cdr (assoc media-type handlers :test #'equal))))
          (check-type handler-list list)
          (dolist (response-code required-handler-response-codes)
            (assert (assoc response-code handler-list)
                    nil
                    "Expected handler to exist for response code ~A and media-type ~A"
                    response-code
                    media-type))))
      (lambda (env)
        (declare (type list env))
        (let ((*dynamic-override-p* dynamic-override-p)
              (*root-directory* root)
              (*static-file-types* static-file-types)
              (*static-file-namestrings* static-file-namestrings)
              (*static-media-types* static-media-types)
              (*handlers* handlers)
              (*handler-media-types* handler-media-types)
              (*media-type-fallback* media-type-fallback)
              (metadata (get 'dynamic-override-errors-app 'foo.lisp.raven:route-metadata)))
          (declare (type pathname *root-directory*)
                   (type function *dynamic-override-p*)
                   (type string *media-type-fallback*)
                   (type list
                         *static-file-types*
                         *static-file-namestrings*
                         *static-media-types*
                         *handlers*
                         *handler-media-types*))
          (if foo.lisp.raven:*fast-dispatch*
              (foo.lisp.raven:%handle-request/fast metadata 'dynamic-override-errors-app env)
              (foo.lisp.raven:%handle-request metadata 'dynamic-override-errors-app env)))))))

(foo.lisp.raven:define-route-metadata 'dynamic-override-errors-app
  :meta 'foo.lisp.vinland:route/simple
  :fboundp nil
  :method :GET
  :GET (lambda ()
         (block nil
           (let* ((env (lack/request:request-env foo.lisp.vinland:*request*))
                  (response-code (lack/middleware/errors/util:parse-response-code env))
                  (dynamic-override-p (funcall *dynamic-override-p* env)))
             (declare (type list env)
                      (type foo.lisp.http-response:status-code-error response-code)
                      (type t dynamic-override-p))
             (if dynamic-override-p
                 (let ((media-type-string (or (foo.lisp.lack/request/content-negotiation:negotiate-media-type
                                               foo.lisp.vinland:*request*
                                               *handler-media-types*)
                                              *media-type-fallback*)))
                   (declare (type string media-type-string))
                   (let ((handler-list (cdr (assoc media-type-string *handlers* :test #'string=))))
                     (declare (type list handler-list))
                     (let ((handler (cdr (assoc response-code handler-list))))
                       (declare (type (or null symbol) handler))
                       (unless handler
                         (error 'foo.lisp.vinland/web:client-error :status-code 406))
                       (let ((response-body (funcall (symbol-function handler))))
                         (declare (type (or string (vector (unsigned-byte 8))) response-body))
                         (etypecase response-body
                           (string
                            `(,response-code
                              (:content-type ,media-type-string)
                              (,response-body)))
                           ((vector unsigned-byte)
                            `(,response-code
                              (:content-type ,media-type-string)
                              ,response-body)))))))
                 (let ((media-type-string (or (foo.lisp.lack/request/content-negotiation:negotiate-media-type
                                               foo.lisp.vinland:*request*
                                               *static-media-types*)
                                              *media-type-fallback*)))
                   (declare (type string media-type-string))
                   (let* ((file-type (cdr (assoc media-type-string *static-file-types* :test #'string=)))
                          (file-namestring (format nil "~D.~A" response-code file-type)))
                     (declare (type string file-type file-namestring))
                     (unless (member file-namestring *static-file-namestrings* :test #'string=)
                       (error 'foo.lisp.vinland/web:client-error :status-code 406))
                     (let ((response (lack/component:call
                                      (make-instance
                                       'lack/app/file:lack-app-file
                                       :file (merge-pathnames file-namestring *root-directory*))
                                      env)))
                       (declare (type list response))
                       (setf (first response) response-code)
                       response)))))))
  :documentation "Raven route metadata for the dynamic-override errors application. Ensures that expected
special variables are established, such as FOO.LISP.VINLAND:*REQUEST* and FOO.LISP.VINLAND:*RESPONSE*, and
processes the request, presenting an error page from either a static file or by calling a handler function.")
