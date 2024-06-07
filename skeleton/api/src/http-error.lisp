(in-package #:<% @var name %>/http-error)

(defparameter *required-handlers*
  '(400 401 403 404 405 406 409 413 414 415 418 429 431
    500 501 502 503 504)
  "List of HTTP response status codes representing errors for which static files
and dynamic handlers must be defined.")

(defparameter *static-file-types*
  '(("application/json" . "json"))
  "Association list used for generating static error files and also for use with
middleware LACK/MIDDLEWARE/ERRORS.")

(defparameter *http-errors*
  '(("application/json" . ((400 . 400-bad-request)
                           (401 . 401-unauthorized)
                           (403 . 403-forbidden)
                           (404 . 404-not-found)
                           (405 . 405-method-not-allowed)
                           (406 . 406-not-acceptable)
                           (409 . 409-conflict)
                           (413 . 413-content-too-large)
                           (414 . 414-uri-too-long)
                           (415 . 415-unsupported-media-type)
                           (418 . 418-im-a-teapot)
                           (429 . 429-too-many-requests)
                           (431 . 431-request-header-fields-too-large)
                           (500 . 500-internal-server-error)
                           (501 . 501-not-implemented)
                           (502 . 502-bad-gateway)
                           (503 . 503-service-unavailable)
                           (504 . 504-gateway-timeout))))
  "Nested association list used for both generating static error pages and for use with
middleware LACK/MIDDLEWARE/ERRORS.")

(defun generate-static (&key dry-run)
  "Generates static assets used by the application.

Generated error pages can be served by LACK/MIDDLEWARE/ERRORS or a reverse-proxy/CDN."
  (foo.lisp.vinland/static:generate-error-files
   *static-errors-directory*
   *static-file-types*
   *http-errors*
   :dry-run dry-run))

(defmacro json-error (response-code)
  (check-type response-code foo.lisp.http-response:status-code-error)
  (let* ((response-code response-code)
         (error-text (foo.lisp.http-response:status-code-to-text response-code))
         (response-body (make-hash:make-hash
                         :initial-contents `("error" ,error-text))))
    `(com.inuoe.jzon:stringify
      ,response-body
      :stream nil
      :pretty nil)))

(defun 400-bad-request ()
  (json-error 400))

(defun 401-unauthorized ()
  (json-error 401))

(defun 403-forbidden ()
  (json-error 403))

(defun 404-not-found ()
  (json-error 404))

(defun 405-method-not-allowed ()
  (json-error 405))

(defun 406-not-acceptable ()
  (json-error 406))

(defun 409-conflict ()
  (json-error 409))

(defun 413-content-too-large ()
  (json-error 413))

(defun 414-uri-too-long ()
  (json-error 414))

(defun 415-unsupported-media-type ()
  (json-error 415))

(defun 418-im-a-teapot ()
  (json-error 418))

(defun 429-too-many-requests ()
  (json-error 429))

(defun 431-request-header-fields-too-large ()
  (json-error 431))

(defun 500-internal-server-error ()
  (json-error 500))

(defun 501-not-implemented ()
  (json-error 501))

(defun 502-bad-gateway ()
  (json-error 502))

(defun 503-service-unavailable ()
  (json-error 503))

(defun 504-gateway-timeout ()
  (json-error 504))

