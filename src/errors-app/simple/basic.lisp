;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.vinland/errors-app/simple/basic)

(declaim (ftype (function (&key (:root pathname)
                                (:static-file-types list)
                                (:handlers list)
                                (:required-static-response-codes list)
                                (:required-handler-response-codes list)
                                (:media-type-fallback string))
                          function)
                make-app))
(defun make-app (&key root
                   (static-file-types *default-static-file-types*)
                   (handlers ())
                   (required-static-response-codes ())
                   (required-handler-response-codes ())
                   (media-type-fallback "text/html"))
  "Returns a Clack application (a function that takes an ENV parameter and returns a
response list) that presents an error page to the user, with all the expected special
variables, including FOO.LISP.VINLAND:*REQUEST* and FOO.LISP.VINLAND:*RESPONSE*. Checks
whether a static file exists, and if it does not, calls a provided handler function,
otherwise signals a CLIENT-ERROR with status code 406.

The list of static files found in ROOT is constructed when the app lambda is instantiated
in order to avoid unnecessary filesystem operations at runtime. This means in effect that
the Lisp process should be restarted whenever adding or removing a static error file.

When a dynamic handler or static file cannot be matched against the request Accept header,
falls back to MEDIA-TYPE-FALLBACK, which must be represented in either STATIC-FILE-TYPES
or HANDLERS association lists.

Signals FOO.LISP.VINLAND/WEB:CLIENT-ERROR with status-code 406 when a static file or
dynamic handler cannot be found for the given matched media-type and parsed response code.

Options:
* ROOT: root directory from which static files are served; a pathname
* REQUIRED-STATIC-RESPONSE-CODES: list of HTTP response status error codes for which static files must be defined for each media-type
* REQUIRED-HANDLER-RESPONSE-CODES: list of HTTP response status error codes for which handlers must be defined for each media-type
* STATIC-FILE-TYPES: association list: each pair has a media-type key and file-type extension as the datum
* HANDLERS: association list of dynamic handlers: each pair has a media-type key and an association list of response-codes and handlers as the datum
* MEDIA-TYPE-FALLBACK: default media-type when the request Accept header does not overlap with the defined media-types; a string
"
  (declare (type pathname root)
           (type list
                 static-file-types handlers
                 required-static-response-codes
                 required-handler-response-codes)
           (type string media-type-fallback))
  (check-type root pathname)
  (assert (uiop:directory-pathname-p root)
          nil
          "ROOT (~A) is not a directory" root)
  (check-type static-file-types list)
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
            (assert (fboundp handler))))))
    (assert (or (assoc media-type-fallback handlers :test #'string=)
                (member media-type-fallback static-media-types :test #'string=))
            nil
            "MEDIA-TYPE-FALLBACK (~A) must be represented in HANDLERS and/or STATIC-FILE-TYPES"
            media-type-fallback)
    (let ((handler-media-types (mapcar #'car handlers)))
      (declare (type list handler-media-types))
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
        (let ((*root-directory* root)
              (*static-file-types* static-file-types)
              (*static-file-namestrings* static-file-namestrings)
              (*static-media-types* static-media-types)
              (*handlers* handlers)
              (*handler-media-types* handler-media-types)
              (*media-type-fallback* media-type-fallback)
              (metadata (get 'basic-errors-app 'foo.lisp.raven:route-metadata)))
          (declare (type pathname *root-directory*)
                   (type string *media-type-fallback*)
                   (type list
                         *static-file-types*
                         *static-file-namestrings*
                         *static-media-types*
                         *handlers*
                         *handler-media-types*))
          (if foo.lisp.raven:*fast-dispatch*
              (foo.lisp.raven:%handle-request/fast metadata 'basic-errors-app env)
              (foo.lisp.raven:%handle-request metadata 'basic-errors-app env)))))))

(foo.lisp.raven:define-route-metadata 'basic-errors-app
  :meta 'foo.lisp.vinland:route/simple
  :fboundp nil
  :method :GET
  :GET (lambda ()
         (let* ((env (lack/request:request-env foo.lisp.vinland:*request*))
                (response-code (lack/middleware/errors/util:parse-response-code env))
                (response))
           (declare (type list env response)
                    (type foo.lisp.http-response:status-code-error response-code))
           (block render-static
             (let ((media-type-string (or (foo.lisp.lack/request/content-negotiation:negotiate-media-type
                                           foo.lisp.vinland:*request*
                                           *static-media-types*)
                                          (return-from render-static))))
               (declare (type string media-type-string))
               (let* ((file-type (cdr (assoc media-type-string *static-file-types* :test #'string=)))
                      (file-namestring (format nil "~D.~A" response-code file-type)))
                 (declare (type string file-type file-namestring))
                 (unless (member file-namestring *static-file-namestrings* :test #'string=)
                   (return-from render-static))
                 (setf response (lack/component:call
                                 (make-instance
                                  'lack/app/file:lack-app-file
                                  :file (merge-pathnames file-namestring *root-directory*))
                                 env))
                 (setf (first response) response-code))))
           (or response
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
                         ((vector (unsigned-byte 8))
                          `(,response-code
                            (:content-type ,media-type-string)
                            ,response-body))))))))))
  :documentation "Raven route metadata for the basic errors application. Ensures that expected special
variables are established, such as FOO.LISP.VINLAND:*REQUEST* and FOO.LISP.VINLAND:*RESPONSE*, and
processes the request, presenting an error page from either a static file or by calling a handler function.")
