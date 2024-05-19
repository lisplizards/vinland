;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.vinland/response)

(deftype status-code ()
  `(integer 100 599))

(deftype status-code-informational ()
  `(integer 100 199))

(deftype status-code-success ()
  `(integer 200 299))

(deftype status-code-redirect ()
  `(integer 300 399))

(deftype status-code-client-error ()
  `(integer 400 499))

(deftype status-code-server-error ()
  `(integer 500 599))

(define-condition unknown-status-error (simple-error)
  ((status :initarg :status))
  (:report (lambda (condition stream)
             (with-slots (status)
                 condition
               (format stream "Unknown status: ~A" status)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *status-code-to-text* (make-hash-table))
  (defparameter *status-code-to-keyword* (make-hash-table))
  (defparameter *status-keyword-to-code* (make-hash-table))
  (defparameter *status-keyword-to-text* (make-hash-table))

  (defparameter *status-codes*
    '((100 "Continue" :continue)
      (101 "Switching Protocols" :switching-protocols)
      (102 "Processing" :processing)
      (103 "Early Hints" :early-hints)
      (200 "OK" :ok)
      (201 "Created" :created)
      (202 "Accepted" :accepted)
      (203 "Non-Authoritative Information" :non-authoritative-information)
      (204 "No Content" :no-content)
      (205 "Reset Content" :reset-content)
      (206 "Partial Content" :partial-content)
      (207 "Multi-Status" :multi-status)
      (208 "Already Reported" :already-reported)
      (226 "IM Used" :im-used)
      (300 "Multiple Choices" :multiple-choices)
      (301 "Moved Permanently" :moved-permanently)
      (302 "Found" :found)
      (303 "See Other" :see-other)
      (304 "Not Modified" :not-modified)
      (307 "Temporary Redirect" :temporary-redirect)
      (308 "Permanent Redirect" :permanent-redirect)
      (400 "Bad Request" :bad-request)
      (401 "Unauthorized" :unauthorized)
      (402 "Payment Required" :payment-required)
      (403 "Forbidden" :forbidden)
      (404 "Not Found" :not-found)
      (405 "Method Not Allowed" :method-not-allowed)
      (406 "Not Acceptable" :not-acceptable)
      (407 "Proxy Authentication Required" :proxy-authentication-required)
      (408 "Request Timeout" :request-timeout)
      (409 "Conflict" :conflict)
      (410 "Gone" :gone)
      (411 "Length Required" :length-required)
      (412 "Precondition Failed" :precondition-failed)
      (413 "Payload Too Large" :payload-too-large)
      (414 "URI Too Long" :uri-too-long)
      (415 "Unsupported Media Type" :unsupported-media-type)
      (416 "Range Not Satisfiable" :range-not-satisfiable)
      (417 "Expectation Failed" :expectation-failed)
      (418 "I'm a teapot" :im-a-teapot)
      (421 "Misdirected Request" :misdirected-request)
      (422 "Unprocessable Content" :unprocessable-content)
      (423 "Locked" :locked)
      (424 "Failed Dependency" :failed-dependency)
      (425 "Too Early" :too-early)
      (426 "Upgrade Required" :upgrade-required)
      (428 "Precondition Required" :precondition-required)
      (429 "Too Many Requests" :too-many-requests)
      (431 "Request Header Fields Too Large" :request-header-fields-too-large)
      (451 "Unavailable For Legal Reasons" :unavailable-for-legal-reasons)
      (500 "Internal Server Error" :internal-server-error)
      (501 "Not Implemented" :not-implemented)
      (502 "Bad Gateway" :bad-gateway)
      (503 "Service Unavailable" :service-unavailable)
      (504 "Gateway Timeout" :gateway-timeout)
      (505 "HTTP Version Not Supported" :http-version-not-supported)
      (506 "Variant Also Negotiates" :variant-also-negotiates)
      (507 "Insufficient Storage" :insufficient-storage)
      (508 "Loop Detected" :loop-detected)
      (510 "Not Extended" :not-extended)
      (511 "Network Authentication Required" :network-authentication-required)))

  (dolist (entry *status-codes*)
    (destructuring-bind (code text keyword)
        entry
      (setf (gethash code *status-code-to-text*) text)
      (setf (gethash code *status-code-to-keyword*) keyword)
      (setf (gethash keyword *status-keyword-to-code*) code)
      (setf (gethash keyword *status-keyword-to-text*) text)))

  (flet ((copy-hash-table (hash-table)
           (let ((copy (make-hash-table :test #'eq
                                        :size (hash-table-count hash-table)
                                        :rehash-size 1
                                        :rehash-threshold 0)))
             (maphash #'(lambda (key value)
                          (setf (gethash key copy) value))
                      hash-table)
             copy)))
    (setq *status-code-to-text* (copy-hash-table *status-code-to-text*)
          *status-code-to-keyword* (copy-hash-table *status-code-to-keyword*)
          *status-keyword-to-code* (copy-hash-table *status-keyword-to-code*)
          *status-keyword-to-text* (copy-hash-table *status-keyword-to-text*))))

(defun status-code-to-keyword (status)
  "Translates STATUS, an HTTP response status code (integer) to a keyword."
  (declare (type integer status))
  (or (gethash status *status-code-to-keyword*)
      (signal 'unknown-status-error :status status)))

(defun status-code-to-text (status)
  "Translates STATUS, an HTTP response status code (integer) to a string."
  (declare (type integer status))
  (or (gethash status *status-code-to-text*)
      (signal 'unknown-status-error :status status)))

(defun status-keyword-to-code (status)
  "Translates STATUS, a keyword representing an HTTP response status code,
to the actual HTTP response status code integer."
  (declare (type keyword status))
  (or (gethash status *status-keyword-to-code*)
      (signal 'unknown-status-error :status status)))

(defun status-keyword-to-text (status)
  "Translates STATUS, a keyword representing an HTTP response status code,
to a string."
  (declare (type keyword status))
  (or (gethash status *status-keyword-to-text*)
      (signal 'unknown-status-error :status status)))

(defun status-text (status)
  "Translates STATUS, either an integer or keyword representing an HTTP response
status code, to a string."
  (declare (type (or integer keyword) status))
  (etypecase status
    (keyword (format nil "~A" (status-keyword-to-text status)))
    (integer (format nil "~A" (status-code-to-text status)))))

(defun status-text-clack-response (status)
  "Returns a Clack response list for STATUS, either an integer or keyword
representing an HTTP response status code."
  (declare (type (or integer keyword) status))
  (let* ((code (etypecase status
                 (integer status)
                 (keyword (status-keyword-to-code status))))
         (body (status-text code)))
    `(,code (:content-type "text/plain"
             :content-length ,(length body))
            (,body))))
