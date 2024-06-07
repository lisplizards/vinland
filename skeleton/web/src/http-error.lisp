(in-package #:<% @var name %>/http-error)

(defparameter *required-handlers*
  '(400 401 403 404 405 406 409 413 414 415 418 429 431
    500 501 502 503 504)
  "List of HTTP response status codes representing errors for which static files
and dynamic handlers must be defined.")

(defparameter *static-file-types*
  '(("text/html" . "html"))
  "Association list used for generating static error pages and also for use with
middleware LACK/MIDDLEWARE/ERRORS.")

(defparameter *http-errors*
  '(("text/html" . ((400 . 400-bad-request)
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

(defun 400-bad-request ()
  (with-main-layout (:title "400: Bad Request"
                     :skip-main-js t
                     :main-container-class "center")
    (:h1 "400: Bad Request")))

(defun 401-unauthorized ()
  (with-main-layout (:title "401: Unauthorized"
                     :skip-main-js t
                     :main-container-class "center")
    (:h1 "401: Unauthorized")))

(defun 403-forbidden ()
  (with-main-layout (:title "403: Forbidden"
                     :skip-main-js t
                     :main-container-class "center")
    (:h1 "403: Forbidden")))

(defun 404-not-found ()
  (with-main-layout (:title "404: Not Found"
                     :skip-main-js t
                     :main-container-class "center")
    (:h1 "404: Not Found")))

(defun 405-method-not-allowed ()
  (with-main-layout (:title "405: Method Not Allowed"
                     :skip-main-js t
                     :main-container-class "center")
    (:h1 "405: Method Not Allowed")))

(defun 406-not-acceptable ()
  (with-main-layout (:title "406: Not Acceptable"
                     :skip-main-js t
                     :main-container-class "center")
    (:h1 "406: Not Acceptable")))

(defun 409-conflict ()
  (with-main-layout (:title "409: Conflict"
                     :skip-main-js t
                     :main-container-class "center")
    (:h1 "409: Conflict")))

(defun 413-content-too-large ()
  (with-main-layout (:title "413: Content Too Large"
                     :skip-main-js t
                     :main-container-class "center")
    (:h1 "413: Content Too Large")))

(defun 414-uri-too-long ()
  (with-main-layout (:title "414: URI Too Long"
                     :skip-main-js t
                     :main-container-class "center")
    (:h1 "414: URI Too Long")))

(defun 415-unsupported-media-type ()
  (with-main-layout (:title "415: Unsupported Media Type"
                     :skip-main-js t
                     :main-container-class "center")
    (:h1 "415: Unsupported Media Type")))

(defun 418-im-a-teapot ()
  (with-main-layout (:title "418: I'm a teapot"
                     :skip-main-js t
                     :main-container-class "center")
    (:h1 "418: I'm a teapot")))

(defun 429-too-many-requests ()
  (with-main-layout (:title "429: Too Many Requests"
                     :skip-main-js t
                     :main-container-class "center")
    (:h1 "429: Too Many Requests")))

(defun 431-request-header-fields-too-large ()
  (with-main-layout (:title "431: Request Header Fields Too Large"
                     :skip-main-js t
                     :main-container-class "center")
    (:h1 "431: Request Header Fields Too Large")))

(defun 500-internal-server-error ()
  (with-main-layout (:title "500: Internal Server Error"
                     :skip-main-js t
                     :main-container-class "center")
    (:h1 "500: Internal Server Error")))

(defun 501-not-implemented ()
  (with-main-layout (:title "501: Not Implemented"
                     :skip-main-js t
                     :main-container-class "center")
    (:h1 "501: Not Implemented")))

(defun 502-bad-gateway ()
  (with-main-layout (:title "502: Bad Gateway"
                     :skip-main-js t
                     :main-container-class "center")
    (:h1 "502: Bad Gateway")))

(defun 503-service-unavailable ()
  (with-main-layout (:title "503: Service Unavailable"
                     :skip-main-js t
                     :main-container-class "center")
    (:h1 "503: Service Unavailable")))

(defun 504-gateway-timeout ()
  (with-main-layout (:title "504: Gateway Timeout"
                     :skip-main-js t
                     :main-container-class "center")
    (:h1 "504: Gateway Timeout")))
